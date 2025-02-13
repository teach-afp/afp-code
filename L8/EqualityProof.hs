{-# LANGUAGE ExistentialQuantification #-}

-- | Poor man's equality proofs.

module EqualityProof
  ( Proof, proof, (=<), (>=)
  , Justification(Conv, Def, Hyp, IH, Thm)
  , check
  )
where

import Prelude
  ( (.)
  , Bool(True, False), (&&)
  , Eq(..)
  )
import Test.QuickCheck.Property
  ( Testable(property), liftBool
  )

infixl 0 :>=:
infixl 0 :=<:
infixl 0 >=
infixl 0 =<

-- | An equality chain of elements of type @a@.
--
data Proof a
  = Proof a
  | ProofL a :>=: a

data ProofL a
  = Proof a :=<: Justification

-- | Start an equality chain.
--
proof :: a -> Proof a
proof = Proof

-- | Left part of @=< justification >=@ to connect two
--   consecutive elements of the equation chain.
--
(=<) :: Proof a -> Justification -> ProofL a
(=<) = (:=<:)

-- | Right part of @=< justification >=@ to connect two
--   consecutive elements of the equation chain.
--
(>=) :: ProofL a -> a -> Proof a
(>=) = (:>=:)

-- | Justification (unchecked).
--
data Justification
  = IH                -- ^ By induction hypothesis.
  | Conv              -- ^ By conversion (computation).
  | forall a. Def a   -- ^ By definition of the given symbol.
  | forall a. Hyp a   -- ^ By some hypothesis.
  | forall a. Thm a   -- ^ By some theorem or lemma.

-- | Check that all elements of the chain are actually equal.
--
check :: Eq a => Proof a -> Bool
check = \case
  Proof  a -> True
  p :>=: a -> checkL a p

checkL :: Eq a => a -> ProofL a -> Bool
checkL a (p :=<: _) = check1 a p

check1 :: Eq a => a -> Proof a -> Bool
check1 a0 = \case
  Proof  a -> a == a0
  p :>=: a -> a == a0 && checkL a p

instance Eq a => Testable (Proof a) where
  property = property . liftBool . check
