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

digit :: Parser0 Char Int
digit = do
  s <- symbol
  if isDigit s then return $ digitToInt s
  else pfail

digits :: Parser0 Char [Int]
digits = do
  n <- digit
  rest <- digits +++ return []
  return (n:rest)

number :: Parser0 Char Int
number = do
  ln <- digits
  let power = map (\(n,p) -> n*10^p) (zip (reverse ln) [0..])
  return $ sum power

plus :: Parser0 Char ()
plus = do
  s <- symbol
  if s == '+' then return ()
  else pfail

pexpr :: Parser0 Char Expr
pexpr = paren +++ add +++ (number >>= return . Val)

add = do
  e1 <- paren +++ (number >>= return . Val)
  plus
  e2 <- pexpr
  return (e1 :+: e2)

paren = do
  s <- symbol
  if s == '(' then pexpr >>= body
  else pfail

   where
   body e = do
     s <- symbol
     case s of
       ')' -> return e
       _   -> pfail

ex  =  parse pexpr "1+(3+4)"
ex2 =  parse pexpr "(1+(3+4))"
