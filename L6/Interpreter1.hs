{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Version 1 of the interpreter
module Interpreter1 where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Expr_Parser as P (parseExpr, Language (..))

-- | A more interesting language with variables and let bindings.
data Expr
  = Lit Integer
  | Expr :+: Expr
  | Var Name            -- new
  | Let Name Expr Expr  -- new
  deriving (Show)

type Name  = String
type Value = Integer

-- | An environment maps variables to values.
type Env = Map Name Value -- Think of it as [(Name, Value)] or Name -> Maybe Value

emptyEnv :: Env
emptyEnv = Map.empty

-- | The evaluation monad now keeps track of passing around
-- the environment.
type Eval a = ReaderT Env (Identity) a
-- Eval a ~= Env -> a

runEval :: Eval a -> a
runEval reader = runIdentity (runReaderT reader emptyEnv)

-- * Environment manipulation

-- | Looking up the value of a variable in the enviroment.
lookupVar :: Name -> Eval Value
lookupVar x = do
  env <- ask
  case Map.lookup x env of
    Nothing -> error $ "Variable " ++ x ++ " not found."
    Just v  -> return v


-- | We can extend the environment with a new binding for a local
-- computation.  Since we're using a reader monad we can be sure
-- that this binding does not escape outside its intended scope.
localScope :: Name -> Value -> Eval a -> Eval a
localScope n v comp = local (Map.insert n v) comp


-- | The evaluator is extended by simply adding cases for the
-- two new constructs. None of the old stuff has to change.
eval :: Expr -> Eval Value
eval (Lit n)     = return n
eval (a :+: b)   = (+) <$> eval a <*> eval b
eval (Var n)     = lookupVar n
-- let x = e1 in e2
eval (Let n e1 e2) = do
  v <- eval e1
  localScope n v (eval e2)

-- * Utilities: testing and parsing

testExpr :: Expr
testExpr = parse "let x=1+2; 1+x"
test :: Value
test = runEval $ eval testExpr

-- | The parser is parameterised over the abstract syntax.
language :: P.Language Expr
language = P.Lang
  { P.lLit    = Lit
  , P.lPlus   = (:+:)
  , P.lLet    = Let
  , P.lVar    = Var
  , P.lNewref = error "language: not implemented: new"
  , P.lDeref  = error "language: not implemented: !"
  , P.lAssign = error "language: not implemented: :="
  , P.lCatch  = error "language: not implemented: catch"
  }

parse :: String -> Expr
parse s = case  P.parseExpr language s  of
  Left err  -> error (show err)
  Right x   -> x
