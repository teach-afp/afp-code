{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Version 6 of the interpreter (your own monad transformers)

module Interpreter6 where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State(MonadState)
import MT

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Expr_Parser as P(parseExpr, Language(..))

data Expr
  = Lit Integer
  | Expr :+: Expr
  | Var Name
  | Let Name Expr Expr
  | NewRef Expr
  | Deref Expr
  | Expr := Expr
  | Catch Expr Expr
  deriving (Show)

-- | Preliminaries for (immutable) local bindings
type Name   = String
type Value  = Integer

-- | An environment maps variables to values.
type Env = Map Name Value

emptyEnv :: Env
emptyEnv = Map.empty

-- | Preliminaries for mutuable references
type Ptr = Integer

-- | Store
data Store = Store
  { nextPtr :: Ptr
  , heap    :: Map Ptr Value
  }

emptyStore :: Store
emptyStore = Store 0 Map.empty


-- | We add an exception type...
data Err
  = SegmentationFault
  | UnboundVariable String
  | OtherError String
  deriving Show

type Eval a = MyStateT Store
                        (MyEnvT Env
                                (MyExceptT Err Identity)) -- new
                        a

runEval :: Eval a -> Either Err a
runEval st = runIdentity
                  (runErr
                         (runEnv
                               (evalST st emptyStore)
                          emptyEnv))

-- * Environment manipulation (no changes from Interpreter1)

-- | We've changed variable lookup to throw an error in our new
-- error type rather than using 'fail'.
lookupVar :: Name -> Eval Value
lookupVar x = do
  env <- lift ask -- Lifting from inner to outer layer
  case Map.lookup x env of
    Nothing -> lift (lift (throwError (UnboundVariable x))) -- lifting from two layers above
    Just v  -> return v

localScope :: Name -> Value -> Eval a -> Eval a
localScope n v st = MyStateT \ s -> local (Map.insert n v) (env_m s)
  where env_m s = runST st s
                   -- break abstraction when going "down" the stack

-- -- * Store manipulation (new)

-- | Create a new reference containing the given value.
newRef :: Value -> Eval Ptr
newRef v = do
  store <- get
  let ptr      = nextPtr store
      ptr'     = 1 + ptr
      newHeap  = Map.insert ptr v (heap store)
  put (Store ptr' newHeap)
  return ptr

-- | Get the value of a reference. Crashes with our own
-- "segfault" if given a non-existing pointer.
deref :: Ptr -> Eval Value
deref p = do
  st <- get -- effect at top-level, nothing to do!
  let h = heap st
  case Map.lookup p h of
    Nothing -> lift (lift (throwError SegmentationFault))
    Just v  -> return v

(=:) :: MonadState Store m => Ptr -> Value -> m Value
p =: v = do
  store <- get
  let heap' = Map.adjust (const v) p (heap store)
  put (store {heap = heap'})
  return v

-- | The case for 'Catch' simply uses the 'catchError' function
-- from the error monad.
eval :: Expr -> Eval Value
eval (Lit n)        = return n
eval (a :+: b)      = (+) <$> eval a <*> eval b
eval (Var x)        = lookupVar x
eval (Let n e1 e2)  = do
  v <- eval e1
  localScope n v (eval e2)
eval (NewRef e)     = do
  v <- eval e
  newRef v
eval (Deref e)      = do
  r <- eval e
  deref r
eval (pe := ve)     = do
  p <- eval pe
  v <- eval ve
  p =: v
eval (Catch e1 e2)  = MyStateT \ s ->
  let
     st1  = runST (eval e1)
     st2  = runST (eval e2)
     env1 = runEnv (st1 s)
     env2 = runEnv (st2 s)
  in MyEnvT \ r -> catchError (env1 r) \ _err -> env2 r
  -- Here, catchError works at the level of error, but eval is at the level of state
  -- We need to break down the computation to get into the lower layers

-- * Examples
testExpr1, testExpr2 :: Expr
testExpr1 = parse "let p=0; !p+1738"
testExpr2 = parse "(try !p catch 0)+1738"
testExpr3 :: Expr
testExpr3 = parse "let one = new 1; \
                 \ let dummy = (try ((one := 2) + !7) catch 0); \
                 \ !one"
-- | value is 1 if state is discarded when error occurs
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
  , P.lPrint  = error "not implemented: printing"
  }

parse :: String -> Expr
parse s = case P.parseExpr language s of
  Left err -> error (show err)
  Right x  -> x
