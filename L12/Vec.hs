{-# OPTIONS_GHC -Wall -Wno-unused-matches #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- | Length-index lists (vectors).

module Vec where

import Data.Bifunctor

data Nat where
  Zero :: Nat
  Suc  :: Nat -> Nat

integer :: Nat -> Integer
integer Zero = 0
integer (Suc n) = 1 + integer n

-- unsafe!
natural :: Integer -> Nat
natural 0 = Zero
natural n = Suc (natural (n - 1))

instance Show Nat where
  show = show . integer

instance Read Nat where
  readsPrec i s = first natural <$> readsPrec i s

-- Nat is usable at the type level via DataKinds

data Vec a n where                        -- GADTs, DataKinds
  Nil  :: Vec a Zero
  Cons :: a -> Vec a n -> Vec a (Suc n)

------------------------------------------------------------------------
-- Indexing

data Fin n where
  FZero :: Fin (Suc n)
  FSuc  :: Fin n -> Fin (Suc n)

look :: Vec a n -> Fin n -> a
look (Cons x xs) FZero    = x
look (Cons x xs) (FSuc i) = look xs i
look _ i = case i of {}                   -- EmptyCase

look' :: Fin n -> Vec a n -> a
look' FZero    (Cons x xs) = x
look' (FSuc i) (Cons x xs) = look' i xs

look2 :: Vec a n -> Fin n -> a
look2 (Cons x xs) = \case
  FZero  -> x
  FSuc i -> look2 xs i
look2 _ = \case {}                        -- EmptyCase

------------------------------------------------------------------------
-- Append

type family Plus n m where                -- TypeFamilies
  Plus Zero    m = m
  Plus (Suc n) m = Suc (Plus n m)

append :: Vec a n -> Vec a m -> Vec a (Plus n m)
append Nil         ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

------------------------------------------------------------------------
-- Replicate

-- Singleton type for natural numbers

data SNat n where
  SZero :: SNat Zero
  SSuc  :: SNat n -> SNat (Suc n)

repl :: forall n a. SNat n -> a -> Vec a n
repl SZero    a = Nil
repl (SSuc n) a = Cons a (repl n a)

------------------------------------------------------------------------
-- To list

toList :: Vec a n -> [a]
toList Nil = []
toList (Cons a as) = a : toList as

------------------------------------------------------------------------
-- From list

data SomeVec a where
  SomeVec :: Vec a n -> SomeVec a

fromList :: [a] -> SomeVec a
fromList []     = SomeVec Nil
fromList (x:xs) = case fromList xs of
  SomeVec v -> SomeVec (Cons x v)

-- -}
-- -}
-- -}
