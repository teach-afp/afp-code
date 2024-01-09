{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Version 2 of the interpreter

module Basic where

import Control.Monad.Identity
import Control.Monad.Reader

import           Data.Map        (Map)
import qualified Data.Map as Map

import qualified Expr_Parser as P (parseExpr, Language (..))

-- | Even more interesting stuff: mutable references!
data Expr
  = Lit Integer
  | Expr :+: Expr
  | Var Name
  | Let Name Expr Expr
  deriving (Show)

-- | Preliminaries for (immutable) local bindings
type Name  = String
type Value = Integer

-- | An environment maps variables to values.
type Env = Map Name Value

emptyEnv :: Env
emptyEnv = Map.empty

newtype Eval a = MkEval (ReaderT Env Identity a)
  deriving (Functor, Applicative, Monad, MonadReader Env)

runEval :: Eval a -> a
runEval (MkEval rd) = runIdentity (runReaderT rd emptyEnv)

-- * Environment manipulation (no changes from Interpreter1)
lookupVar :: Name -> Eval Value
lookupVar x = do
  env <- ask
  case Map.lookup x env of
    Nothing -> error $ "Variable " ++ x ++ " not found."
    Just v  -> return v

localScope :: Name -> Value -> Eval a -> Eval a
localScope n v = local (Map.insert n v)

-- * Store manipulation (new)

-- | As before we only need to add cases for the new con-
-- structors to the evaluator. No need to change the old stuff.
eval :: Expr -> Eval Value
eval (Lit n)       = return n
eval (a :+: b)     = (+) <$> eval a <*> eval b
eval (Var x)       = lookupVar x
eval (Let n e1 e2) = do v <- eval e1
                        localScope n v (eval e2)

-- * Utilities: testing and parsing

-- Test for variables declaration
test1, test2, test3, test4 :: Expr
runtest1, runtest2, runtest3, runtest4 :: Value

test1    = parse "let x=10; x"
runtest1 = runEval $ eval test1

test2    = parse "let x=secret 10; x"
runtest2 = runEval $ eval test2

test3    = parse "let x=secret 10; let y = 20; x+y"
runtest3 = runEval $ eval test3

test4    = parse "let x=10; let y=20; x+y"
runtest4 = runEval $ eval test4


-- | The parser is parameterised over the abstract syntax.
language :: P.Language Expr
language = P.Lang
  { P.lLit    = Lit
  , P.lPlus   = (:+:)
  , P.lLet    = Let
  , P.lVar    = Var
  , P.lNewSecretexp = id
  , P.lNewSecretref = error "Not yet defined"
  , P.lNewref = error "Not yet defined"
  , P.lDeref  = error "Not yet defined"
  , P.lAssign = error "Not yet defined"
  , P.lCatch  = error "Not yet defined"
  }

parse :: String -> Expr
parse s = case P.parseExpr language s of
  Left err -> error (show err)
  Right x  -> x
