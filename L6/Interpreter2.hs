{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Version 2 of the interpreter
module Interpreter2 where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Expr_Parser as P (parseExpr, Language (..))

-- | Even more interesting stuff: mutable references!
data Expr = Lit Integer
          | Expr :+: Expr
          | Var Name
          | Let Name Expr Expr
          | NewRef Expr         -- new
          | Deref Expr          -- new
          | Expr := Expr        -- new
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

-- | We need to keep track of the store containing the values of
-- our references. We also remember the next unused pointer.
data Store = Store { nextPtr :: Ptr
                   , heap    :: Map Ptr Value
                   }

emptyStore :: Store
emptyStore = Store 0 Map.empty

-- | The store needs to be updated globally in a program so we
-- use a state monad to pass the store around.

type Eval a = StateT Store (ReaderT Env Identity) a

-- {- ^ Explaining and expanding the type
--   CMS.StateT s m' a  ~=  s -> m' (a, s)
--   CMR.ReaderT e m a  ~=  e -> m a
--   CMI.Identity a     ~=  a
--   => Eval a  ~=
--      s -> m' (a, s)     ~= {- where m' = CMR.ReaderT Env m -}
--      s -> e -> m (a,s)  ~= {- where m  = CMI.Identity      -}
--      s -> e -> (a,s)

-- -- Exercise: Implement a "reader-state-monad" directly:
-- newtype MyMonad s e a = MyMonad {runMyMonad :: s -> e -> (a,s)}
-- instance Monad (MyMonad s e) where
--   return = returnMyMonad
--   (>>=)  = bindMyMonad
-- returnMyMonad :: a -> MyMonad s e a
-- returnMyMonad x = MyMonad $ \s -> \ e -> (x, s)
-- -- ...
-- -}

runEval :: Eval a -> a
runEval st = runIdentity
                      (runReaderT
                          (evalStateT st emptyStore) -- new
                       emptyEnv)


-- * Environment manipulation (no changes from Interpreter1)
lookupVar :: Name -> Eval Value
lookupVar x = do
  env <- ask
  case Map.lookup x env of
    Nothing -> fail $ "Variable " ++ x ++ " not found."
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
               Nothing -> fail ("Segmentation fault: "++show p++" is not bound")
               Just v  -> return v

-- | Updating the value of a reference. Has no effect if the
-- reference doesn't exist. (Exercise: Maybe that's not the best
-- semantics... what would be a better one?)
-- Map.adjust :: (Ord k) => (a -> a) -> k -> Map k a -> Map k a

(=:) :: Ptr -> Value -> Eval Value
p =: v = do store <- get
            let heap' = Map.adjust (\val -> v) p (heap store)
            put (store {heap = heap'})
            return v
{-
-- Alternative
(=:) :: Ptr -> Value -> Eval Value
p =: v = do modify $ \s -> s { heap = Map.adjust (const v) p (heap s) }
            return v
-}

-- | As before we only need to add cases for the new con-
-- structors to the evaluator. No need to change the old stuff.
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

-- * Utilities: testing and parsing

testExercise :: Expr
testExercise = parse "let p=new 7; !p"
testUgly :: Expr
testUgly = parse "let p=new 1; let q=new 1738; !(p+1)"
test :: Value
test = runEval $ eval testUgly

-- | The parser is parameterised over the abstract syntax.
language :: P.Language Expr
language = P.Lang
  { P.lLit    = Lit
  , P.lPlus   = (:+:)
  , P.lLet    = Let
  , P.lVar    = Var
  , P.lNewref = NewRef
  , P.lDeref  = Deref
  , P.lAssign = (:=)
  , P.lCatch  = \_ _ -> Var "language: not implemented: catch"
  }

parse :: String -> Expr
parse s = case P.parseExpr language s of
  Left err -> error (show err)
  Right x  -> x
