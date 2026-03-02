{-# OPTIONS_GHC -Wall -Wno-unused-matches #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE TypeApplications #-}

-- | Length-index lists (vectors).

module Vec where

import Data.Bifunctor ( Bifunctor(first) )
import Data.Kind (Type)

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

data Vec (a :: Type) (n :: Nat) where                        -- GADTs, DataKinds
  Nil  :: Vec a 'Zero
  Cons :: a -> Vec a n -> Vec a ('Suc n)

------------------------------------------------------------------------
-- Indexing

data Fin n where
  FZero :: Fin (Suc n)
  FSuc  :: Fin n -> Fin (Suc n)

look :: Vec a n -> Fin n -> a
look (Cons a v) (FZero)  = a
look (Cons a v) (FSuc n) = look v n
look Nil        i        = case i of

look' :: Fin n -> Vec a n -> a
look' (FZero)  (Cons a v) = a
look' (FSuc n) (Cons a v) = look' n v

------------------------------------------------------------------------
-- Append

type family Plus (n :: Nat) (m :: Nat) :: Nat where                -- TypeFamilies
  Plus Zero    m = m
  Plus (Suc n) m = Suc (Plus n m)

append :: Vec a n -> Vec a m -> Vec a (Plus n m)
append Nil         ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

class Collection c where
  type E c

instance Collection [Int] where
  type E ([Int]) = Int

-- class IsSum (n :: Nat) (m :: Nat) (k :: Nat) | n m -> k where

-- instance IsSum Zero m m
-- instance IsSum n m k => IsSum (Suc n) m (Suc k)

-- append' :: (IsSum n m k) => Vec a n -> Vec a m -> Vec a k
-- append' Nil         ys = ys
-- append' (Cons x xs) ys = Cons x (append' xs ys)

------------------------------------------------------------------------
-- Replicate

-- Singleton type for natural numbers

-- repl :: (n : Nat) -> a -> Vec a n

data SNat (n :: Nat) where
  SZero :: SNat Zero
  SSuc  :: SNat n -> SNat (Suc n)

repl :: SNat n -> a -> Vec a n
repl SZero a = Nil
repl (SSuc n) a = Cons a (repl n a)

data SomeNat where
  SomeNat :: forall n. SNat n -> SomeNat
-- unSome :: SomeNat -> SNat n

sg :: Nat -> SomeNat
sg Zero    = SomeNat SZero
sg (Suc k) = case sg k of
  SomeNat @n m -> SomeNat @(Suc n) (SSuc m)

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
