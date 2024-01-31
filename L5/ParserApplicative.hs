module ParserApplicative where

import Prelude hiding (exp, fail)

import Control.Applicative
import Data.Bifunctor
import Data.Char
import Data.Foldable1

import Data.List.NonEmpty (pattern (:|))
import qualified Data.List.NonEmpty as List1
type List1 = List1.NonEmpty

-- Parser a = String -> a
-- Parser a = String -> Maybe a
-- Parser a = String -> (Maybe a, String)

-- type Parser a = String -> [(a,String)]

newtype P s a = P { runP :: [s] -> [(a,[s])] }

char :: (s -> Bool) -> P s s
char p = P \case
  (s:ss) | p s -> [(s,ss)]
  _ -> []

fail :: P s a
fail = P \ _ -> []

instance Functor (P s) where
  fmap f p = P \ ss -> map (first f) (runP p ss)
  -- first (f :: a -> b) :: (a,c) -> (b,c)

instance Applicative (P s) where
  pure a = P \ ss -> [(a,ss)]
  liftA2 f p1 p2 = P \ ss ->
    [ (f a b, ss2)
    | (a, ss1) <- runP p1 ss
    , (b, ss2) <- runP p2 ss1
    ]

liftA1 :: (a -> b) -> P s a -> P s b
liftA1 = fmap

instance Alternative (P s) where
  empty     = fail
  p1 <|> p2 = P \ ss -> runP p1 ss ++ runP p2 ss

digit :: P Char Int
digit = (\ c -> ord c - ord '0') <$> char isDigit

eps :: P s [a]
eps = pure []

star :: P s a -> P s [a]
star p = eps <|> liftA2 (:) p (star p)

plus :: P s a -> P s (List1 a)
plus p = liftA2 (:|) p (star p)

base10 :: List1 Int -> Integer
base10 = foldlMap1' toInteger \ n d -> n*10 + toInteger d

decimal :: P Char Integer
decimal = base10 <$> plus digit

skip :: Char -> P Char ()
skip c = const () <$> char (c ==)
-- skip c = () <$ char (c ==)

-- Example: Hutton's Razor

data Exp
  = EInt Integer
  | EPlus Exp Exp
  deriving Show

-- Right-recursive context-free LBNF grammar
--
-- EInt.  Exp1 ::= Integer
-- EPlus. Exp  ::= Exp1 "+" Exp
-- _.     Exp  ::= Exp1

-- exp1 :: P Char Exp
-- exp1 = liftA1 EInt decimal

-- exp :: P Char Exp
-- exp = exp1 <|> liftA2 EPlus exp1 exp

-- Using <*> syntax

exp1' :: P Char Exp
exp1' = EInt <$> decimal

exp' :: P Char Exp
exp' = exp1' <|> (EPlus <$> exp1' <* skip '+' <*> exp')
-- (<*) :: f a -> f b -> f a

inp = "1+17+42"
test1 = runP exp' inp

eof :: P s ()
eof = P \case
  [] -> [((),[])]
  _  -> []

test2 = runP  (exp' <* eof) inp

peek :: (s -> Bool) -> P s ()
peek p = P \case
  (s:ss) | p s -> [((), s:ss)]
  _ -> []

decimal' :: P Char Integer
decimal' = base10 <$> plus digit <* peek (not . isDigit)


exp1'' :: P Char Exp
exp1'' = EInt <$> decimal'

exp'' :: P Char Exp
exp'' = exp1'' <|> (EPlus <$> exp1'' <* skip '+' <*> exp'')

test3 = runP exp'' inp

-- * Monadic parsing

instance Monad (P s) where
  return = pure
  m >>= k = P \ ss ->
    [ (b, ss2)
    | (a, ss1) <- runP m ss
    , (b, ss2) <- runP (k a) ss1
    ]

replicateP :: Int -> P s a -> P s [a]
replicateP n p = loop n
  where
    loop n
      | n <= 0    = pure []
      | otherwise = liftA2 (:) p $ loop (n-1)

a = char ('a' ==)
b = skip 'b'
c = skip 'c'

-- Context-sensitive language aⁿbⁿcⁿ

anbncn :: P Char ()
anbncn = do
  as <- star a
  let n = length as
  replicateP n b
  replicateP n c
  eof

test4 = runP anbncn "aaabbccc"
test5 = runP anbncn "aaabbbccc"

-- Context-free language  aⁿbⁿ

anbn :: P Char ()
anbn = pure () <|> (a *> anbn *> b)

test6 = runP (anbn *> eof) "aaabb"
test7 = runP (anbn *> eof) "aaabbb"


-- Monadic parsing: char is definable

symbol :: P s s
symbol = P \case
  (s:ss) -> [(s,ss)]
  [] -> []

char' :: (s -> Bool) -> P s s
char' f = do
  s <- symbol
  if f s then return s else fail
