-- | Version 0 of the interpreter
module Interpreter0 where

import Control.Applicative
import Control.Monad.Identity

import qualified Expr_Parser as P (parseExpr, Language (..))

-- | The simplest expression language imaginable.
data Expr = Lit Integer
          | Expr :+: Expr
  deriving (Show)

type Value = Integer

{- | A monad for evaluating expressions. Just the identity monad
   at this point.
-}
type Eval a = Identity a

runEval :: Eval a -> a
runEval = runIdentity

-- | A monadic evaluator.
eval :: Expr  -> Eval Value
eval (Lit n)    = return n
eval (a :+: b)  = (+) <$> (eval a) <*> (eval b)

testExpr :: Expr
testExpr = Lit 1700 :+: Lit 38

testRun :: Value
testRun = runEval $ eval testExpr


-- * Utilities: testing and parsing
-- | The parser is parameterised over the abstract syntax.
language :: P.Language Expr
language = P.Lang
  { P.lLit    = Lit
  , P.lPlus   = (:+:)
  , P.lLet    = error "language: not implemented: let"
  , P.lVar    = error "language: not implemented: variables"
  , P.lNewref = error "language: not implemented: new"
  , P.lDeref  = error "language: not implemented: !"
  , P.lAssign = error "language: not implemented: :="
  , P.lCatch  = error "language: not implemented: catch"
  }

parse :: String -> Expr
parse s = case  P.parseExpr language s  of
  Left err  -> error (show err)
  Right x   -> x

testParse = eval $ parse "(1+2)+3"
