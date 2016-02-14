{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Version 4 of the interpreter
module Interpreter4 where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except    -- new

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Expr_Parser as P(parseExpr, Language(..))

-- | Finally, we add a way to catch exceptions arising from
-- unbound variables and dereferencing non-existent pointers.
data Expr = Lit Integer
          | Expr :+: Expr
          | Var Name
          | Let Name Expr Expr
          | NewRef Expr
          | Deref Expr
          | Expr := Expr
          | Catch Expr Expr      -- new
  deriving (Show)

-- | Preliminaries for (immutable) local bindings
type Name   = String
type Value  = Integer

-- | An environment maps variables to values.
type Env = Map Name Value

emptyEnv :: Env
emptyEnv = Map.empty

-- | Preliminaries for mutuable references
type Ptr    = Value
  -- ^ dangerous language: any 'Value' can be used as a 'Ptr'

-- | Store
data Store = Store { nextPtr :: Ptr
                   , heap    :: Map Ptr Value
                   }

emptyStore :: Store
emptyStore = Store 0 Map.empty


-- | We add an exception type...
data Err = SegmentationFault
         | UnboundVariable String
         | OtherError String
  deriving Show

-- ExceptT on the inside (wrapped by the state monad)
newtype Eval a = MkEval (ExceptT Err                              -- new
                                 (StateT Store
                                         (ReaderT Env Identity))
                                 a)

  deriving (Functor, Applicative,
            Monad, MonadState  Store
                 , MonadReader Env
                 , MonadError  Err -- new
                 )

runEval :: Eval a -> Either Err a
runEval (MkEval err) = runIdentity
                         (runReaderT
                            (evalStateT
                                        (runExceptT err) -- new
                                        emptyStore)
                        emptyEnv)


-- * Environment manipulation (no changes from Interpreter1)

lookupVar :: Name -> Eval Value
lookupVar x = do
  env <- ask
  case Map.lookup x env of
    Nothing -> throwError (UnboundVariable x) -- new
    Just v  -> return v

localScope :: Name -> Value -> Eval a -> Eval a
localScope n v = local (Map.insert n v)

-- * Store manipulation (new)

-- | Create a new reference containing the given value.
newRef :: Value -> Eval Ptr
newRef v = do store <- get
              let ptr      = nextPtr store
                  ptr'     = 1 + ptr
                  newHeap  = Map.insert ptr v (heap store)
              put (Store ptr' newHeap)
              return ptr

-- | Get the value of a reference. Crashes with our own
-- "segfault" if given a non-existing pointer.
deref :: Ptr -> Eval Value
deref p = do st <- get
             let h = heap st
             case Map.lookup p h of
               Nothing -> throwError SegmentationFault -- new
               Just v  -> return v

(=:) :: MonadState Store m => Ptr -> Value -> m Value
p =: v = do store <- get
            let heap' = Map.adjust (const v) p (heap store)
            put (store {heap = heap'})
            return v

-- | The case for 'Catch' simply uses the 'catchError' function
-- from the error monad.
eval :: Expr -> Eval Value
eval (Lit n)        = return n
eval (a :+: b)       = (+) <$> eval a <*> eval b
eval (Var x)        = lookupVar x
eval (Let n e1 e2) = do v <- eval e1
                        localScope n v (eval e2)
eval (NewRef e)     = do v <- eval e
                         newRef v
eval (Deref e)      = do r <- eval e
                         deref r
eval (pe := ve)     = do p <- eval pe
                         v <- eval ve
                         p =: v
eval (Catch e1 e2)  = catchError (eval e1) (\_err -> eval e2)


-- * Examples
testExpr1, testExpr2 :: Expr
testExpr1 = parse "let p=0; !p+1738"
testExpr2 = parse "(try !p catch 0)+1738"
testExpr3 :: Expr
testExpr3 = parse "let one = new 1; \
                 \ let dummy = (try ((one := 2) + !7) catch 0); \
                 \ !one"
-- | value is 2 if state is *not* discarded when error occurs
test1 :: Either Err Value
test1 = runEval $ eval testExpr1
test2 :: Either Err Value
test2 = runEval $ eval testExpr2
test3 :: Either Err Value
test3 = runEval $ eval testExpr3


----------------
-- | Parser stuff.
language :: P.Language Expr
language = P.Lang
  { P.lLit    = Lit
  , P.lPlus   = (:+:)
  , P.lLet    = Let
  , P.lVar    = Var
  , P.lNewref = NewRef
  , P.lDeref  = Deref
  , P.lAssign = (:=)
  , P.lCatch  = Catch
  }

parse :: String -> Expr
parse s = case P.parseExpr language s of
  Left err -> error (show err)
  Right x  -> x
