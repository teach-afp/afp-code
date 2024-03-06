{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeBasedM where

data Nat where
  Zero :: Nat
  Suc  :: Nat -> Nat

-- Datakinds
data Vector a (n :: Nat) where   -- uses KindSignatures
  Nil  :: Vector a Zero
  (:-) :: a -> Vector a n -> Vector a (Suc n)

array :: Vector Int (Suc (Suc Zero))
array = 2 :- (1 :- Nil)

-- ill-typed
-- array2 :: Vector Int (Suc (Suc Zero))
-- array2 = (1 :- Nil)

-- Type families
type family (n :: Nat) :+: (m :: Nat) :: Nat   -- uses TypeOperators,
                                               --      TypeFamilies
type instance Zero    :+: m = m
type instance (Suc n) :+: m = Suc (n :+: m)

-- -- Kind constraint for less-equal
class Less (n :: Nat) (m :: Nat) where  -- uses MultiParamTypeClasses
instance Less (Zero :: Nat)  m where    -- uses FlexibleInstances
instance Less n m => Less (Suc n :: Nat) (Suc m :: Nat) where

-- Append (using type-families)
append :: Vector a n -> Vector a m -> Vector a (n :+: m)
append (x :- xs) ys = x :- append xs ys
append Nil       ys = ys

-- Forget types
toList :: Vector a n -> [a]
toList Nil       = []
toList (x :- xs) = x : toList xs

-- -- Singletons to access an index
data SNat (n :: Nat) where
     SZero :: SNat Zero
     SSuc  :: SNat n -> SNat (Suc n)

-- -- From singleton to Int
toNat :: SNat n -> Int
toNat SZero    = 0
toNat (SSuc n) = 1 + toNat n

index :: Less n m => SNat n -> Vector a (Suc m :: Nat) -> a
index tn vec = toList vec !! toNat tn

zero = SZero
one  = SSuc SZero
two  = SSuc (SSuc SZero)

t1 = index zero array
t2 = index one array
-- t3 = index two array   -- Bad type-check!
