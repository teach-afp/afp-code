{-# LANGUAGE GADTs #-}

module Parser0
  ( Parser0 (..) -- :: * -> * -> *
  , symbol       -- :: Parser0 s s
  , pfail        -- :: Parser0 s a
  , (+++)        -- :: Parser0 s a -> Parser0 s a -> P s a
  , Semantics    -- :: * -> * -> *
  , parse        -- :: Parser0 s a -> Semantics s a
  ) where

import Control.Applicative
import Control.Monad (liftM, ap)
import Data.Char (isDigit, digitToInt)

-- Final semantics to expose:
type Semantics s a = [s] -> [(a,[s])]

data Parser0 s a where
  Symbol  ::  Parser0 s s
  Fail    ::  Parser0 s a
  Choice  ::  Parser0 s a -> Parser0 s a -> Parser0 s a
  Return  ::  a -> Parser0 s a
  (:>>=)  ::  Parser0 s a -> (a -> Parser0 s b) -> Parser0 s b


{- | Constructors -}
symbol = Symbol
pfail  = Fail

{- | Combinators -}
(+++)  = Choice

{- | Functor instance from Monad. -}
instance Functor (Parser0 s) where
  fmap = liftM

{- | Applicative instance with idiomatic application from Monad. -}
instance Applicative (Parser0 s) where
  pure  = Return
  (<*>) = ap

{- | Monadic instance for Parser0 -}
instance Monad (Parser0 s) where
  (>>=)  = (:>>=)
  -- return = pure   -- automatic


{- | Run function -}
parse  = run0

run0 :: Parser0 s a -> Semantics s a
run0 Symbol          [] = []
run0 Symbol      (s:ss) = [(s,ss)]
run0 Fail            _  = []
run0 (Choice p q)    ss = run0 p ss ++ run0 q ss
run0 (Return x)      ss = [(x,ss)]
run0 (p :>>= f)      ss =
  [ (y,s2)
  | (x,s1) <- run0 p ss
  , (y,s2) <- run0 (f x) s1
  ]


{- | Example -}
data Expr where
  Val   :: Int -> Expr
  (:+:) :: Expr -> Expr -> Expr
  deriving Show

infixr 6 :+:

char :: (s -> Bool) -> Parser0 s s
char p = do
  s <- symbol
  if p s then return s else pfail

skip :: Char -> Parser0 Char ()
skip c = () <$ char (== c)

digit :: Parser0 Char Int
digit = digitToInt <$> char isDigit

digits :: Parser0 Char [Int]
digits = do
  n <- digit
  rest <- digits +++ return []
  return (n:rest)

number :: Parser0 Char Int
number = do
  ln <- digits
  return $ sum $ zipWith (\ n p -> n*10^p) (reverse ln) [0..]

expr, atom, add,  parenExpr :: Parser0 Char Expr

expr = atom +++ add

atom = (Val <$> number) +++ parenExpr

add = do
  e1 <- atom
  skip '+'
  e2 <- expr
  return (e1 :+: e2)

parenExpr = do
  skip '('
  e <- expr
  skip ')'
  return e

ex  =  parse expr "1+(3+4)"
ex2 =  parse expr "(1+(3+4))"
