{-# LANGUAGE ExistentialQuantification #-}

-- | Poor man's equality proofs.

module EqualityProof
  ( Proof, proof, (=<), (>=)
  , Justification(Conv, Def, IH, Thm)
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

data Proof a
  = Proof a
  | ProofL a :>=: a

data ProofL a
  = Proof a :=<: Justification

proof :: a -> Proof a
proof = Proof

(>=) :: ProofL a -> a -> Proof a
(>=) = (:>=:)

(=<) :: Proof a -> Justification -> ProofL a
(=<) = (:=<:)

data Justification
  = IH
  | Conv
  | forall a. Def a
  | forall a. Thm a

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
