{-# LANGUAGE GADTs #-}
module Expr where

import           Control.Applicative
import           Data.Char
import           Parser

-- | A simple expression language with integers and booleans.
-- Contains both well- and ill-typed expressions.
data Expr where
  LitI  :: Int  -> Expr
  LitB  :: Bool -> Expr
  (:+:) :: Expr -> Expr -> Expr
  (:==:) :: Expr -> Expr -> Expr
  If    :: Expr -> Expr -> Expr -> Expr

-- | A value is an integer or a boolean.
data Value = VInt Int | VBool Bool
  deriving (Eq, Show)

showTypeOfVal :: Value -> String
showTypeOfVal (VBool _) = "Bool"
showTypeOfVal (VInt  _) = "Int"

-- | Evaluation needs to check the expressions are evaluated to
-- the right type; otherwise, it produces an error.
eval :: Expr -> Value
eval (LitI n)       =  VInt n
eval (LitB b)       =  VBool b
eval (e1 :+: e2)     =  plus (eval e1) (eval e2)
  where plus (VInt n) (VInt m) = VInt $ n + m
        plus _        _        = error "plus: type error"
eval (e1 :==: e2)    =  eq (eval e1) (eval e2)
  where eq (VInt n)  (VInt m)  = VBool $ n == m
        eq (VBool a) (VBool b) = VBool $ a == b
        eq _         _         = error "equal: type error"
eval (If e1 e2 e3)  =  case eval e1 of
  VBool True   ->  eval e2
  VBool False  ->  eval e3
  _            ->  error "if: type error!"

-- | Examples
eOK, eBad, eBad2  :: Expr
eOK   = If (LitB False) (LitI 1) (LitI 2 :+: LitI 1736)
eBad  = If (LitB False) (LitI 1) (LitI 2 :+: LitB True)
eBad2 = If (LitI 20) (LitI 1) (LitI 7)

-- Pretty printing.
instance Show Expr where
  showsPrec p e = case e of
    LitI n       -> shows n

    LitB b       -> shows b

    e1 :+: e2     -> showParen (p > 2) $
      showsPrec 2 e1 . showString " + " . showsPrec 3 e2

    e1 :==: e2    -> showParen (p > 1) $
      showsPrec 2 e1 . showString " == " . showsPrec 2 e2

    If e1 e2 e3  -> showParen (p > 0) $
      showString "if "    . shows e1 .
      showString " then " . shows e2 .
      showString " else " . shows e3

-- Parsing expressions. Uses a slightly modified version of our
-- parser library from lecture 4. Also goes crazy with the
-- operators from "Control.Applicative".  Exercise: check out
-- these combinators.
type Token = String
instance Read Expr where
  readsPrec p s = [ (x, unwords ts)
                  | (x, ts) <- parse (exprP p) $ tokenize s ]
    where
      tokenize :: String -> [Token]
      tokenize "" = []
      tokenize s  = t : tokenize s'
        where [(t, s')] = lex s

      exprP :: Int -> P Token Expr
      exprP 0 = If <$> (this "if"   *> exprP 0)
                   <*> (this "then" *> exprP 0)
                   <*> (this "else" *> exprP 0)
            <|> exprP 1

      exprP 1 = (:==:) <$> exprP 2 <*> (this "==" *> exprP 2)
            <|> exprP 2

      exprP 2 = chainLeft plusP (exprP 3)
        where plusP = (:+:) <$ this "+"

      exprP _ = foldr1 (<|>)
        [ LitI . read  <$>  sat (all isDigit)
        , LitB . read  <$>  sat (`elem` ["True", "False"])
        , this "(" *> exprP 0 <* this ")"
        ]
