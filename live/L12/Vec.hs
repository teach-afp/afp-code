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
look = undefined

------------------------------------------------------------------------
-- Append

type family Plus (n :: Nat) (m :: Nat) :: Nat where                -- TypeFamilies

append :: Vec a n -> Vec a m -> Vec a (Plus n m)
append Nil         ys = undefined
append (Cons x xs) ys = undefined

------------------------------------------------------------------------
-- Replicate

-- Singleton type for natural numbers

-- repl :: (n : Nat) -> a -> Vec a n

data SNat (n :: Nat)

------------------------------------------------------------------------
-- To list

toList :: Vec a n -> [a]
toList Nil = []
toList (Cons a as) = a : toList as

------------------------------------------------------------------------
-- From list

-- fromList :: [a] -> Vec a ?

-- -}
-- -}
-- -}
