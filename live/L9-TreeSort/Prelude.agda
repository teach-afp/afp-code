-- Advanced Functional Programming course 2024
-- Chalmers TDA342 / GU DIT260
--
-- Introduction to Dependent Types with Agda
--
-- File 1: The Curry-Howard Isomorphism

{-# OPTIONS --allow-unsolved-metas #-}

module Prelude where

open import Agda.Primitive renaming (Set to Type)

-- Natural numbers as our first example of
-- an inductive data type.

data ℕ : Type where
  zero : ℕ
  suc  : (n : ℕ) → ℕ

-- To use decimal notation for numerals, like
-- 2 instead of (suc (suc zero)), connect it
-- to the built-in natural numbers.

{-# BUILTIN NATURAL ℕ #-}

-- Lists are a parameterized inductive data type.

data List (A : Type) : Type where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A   -- C-\ : :

infixr 6 _∷_

-- C-c C-l   load

-- Disjoint sum type.

data _⊎_ (A B : Type) : Type where  -- \uplus
  inl : A → A ⊎ B
  inr : B → A ⊎ B

infixr 4 _⊎_

-- The empty sum is the type with 0 alternatives,
-- which is the empty type.
-- By the Curry-Howard-Isomorphism,
-- which views a proposition as the set/type of its proofs,
-- it is also the absurd proposition.

data False : Type where

⊥-elim : False → {A : Type} → A
⊥-elim = {!!}

variable
  A B : Type

⊥-unit-left : False ⊎ A → A
-- ⊥-unit-left (inl ())
⊥-unit-left (inr x)  = x

-- C-c C-SPC give
-- C-c C-, show hypotheses and goal
-- C-c C-. show hypotheses and infers type

-- Tuple types

-- The generic record type with two fields
-- where the type of the second depends on the value of the first
-- is called Sigma-type (or dependent sum), in analogy to
--
--   Σ ℕ B =  Σ   B(n) = B(0) + B(1) + ...
--           n ∈ ℕ

record Σ (A : Type) (B : A → Type) : Type where  -- \GS  \Sigma
  constructor _,_
  field  fst : A
         snd : B fst
open Σ

infixr 5 _,_

-- data  Σ (A : Type) (B : A → Type) : Type where
--   _,_ : (fst : A) (snd : B fst) → Σ A B

-- The non-dependent version is the ordinary Cartesian product.

_×_ : (A B : Type) → Type
A × B = Σ A (λ _ → B)

-- mkPair : (a : A) → B a → Σ A (λ x → B x)
-- mkPair a b = a , b

infixr 5 _×_

-- ex0 : Σ ℕ (λ n → n ≡ 0)
-- ex0 = 0 , refl

-- The record type with no fields has exactly one inhabitant
-- namely the empty tuple record{}
-- By Curry-Howard, it corresponds to Truth, as
-- no evidence is needed to construct this proposition.

record True : Type where

trivial : True
trivial = {!!}

-- Example: distributivity  A ∧ (B ∨ C) → (A ∧ B) ∨ (A ∧ C)

dist : ∀{A B C : Type} → A × (B ⊎ C) → (A × B) ⊎ (A × C)
dist (a , inl b) = inl (a , b)
dist (a , inr c) = inr (a , c)

-- Relations

-- Type-theoretically, the type of relations 𝓟(A×A) is
--
--   A × A → Prop
--
-- which is
--
--   A × A → Type
--
-- by the Curry-Howard-Isomorphism
-- and
--
--   A → A → Type
--
-- by currying.

Rel : (A : Type) → Type₁
Rel A = A → A → Type

-- Less-or-equal on natural numbers

_≤_ : Rel ℕ
zero ≤ y = True
suc x ≤ zero = False
suc x ≤ suc y = x ≤ y

-- C-c C-l load
-- C-c C-c case split
-- C-c C-, show goal and assumptions
-- C-c C-. show goal and assumptions and current term
-- C-c C-SPC give

≤-refl : (n : ℕ) → n ≤ n
≤-refl zero    = trivial
≤-refl (suc n) = ≤-refl n

ex34 : 3 ≤ 4
ex34 = trivial

ex : 1 ≤ 0 → False
ex x = x
