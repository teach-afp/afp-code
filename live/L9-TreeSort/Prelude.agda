-- Advanced Functional Programming
-- Chalmers TDA342 / GU DIT260
--
-- Introduction to Dependent Types with Agda
--
-- File 1: The Curry-Howard Isomorphism

-- {-# OPTIONS --allow-unsolved-metas #-}

module Prelude where

open import Agda.Primitive renaming (Set to Type)

-- Natural numbers as our first example of
-- an inductive data type.

data ℕ : Type where
  zero : ℕ
  suc  : (n : ℕ) → ℕ

id : ℕ → ℕ
id zero    = zero
id (suc n) = suc (id n)

f : ℕ → ℕ
f zero = zero
f (suc (suc x)) = f (suc x)
f _ = zero

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
⊥-elim ()

-- C-c C-SPC give
-- C-c C-, show hypotheses and goal
-- C-c C-. show hypotheses and infers type

¬ : (A : Type) → Type
¬ A = A → False

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

-- foo : {!   !}
-- foo = {! snd  !}

-- data Σ (A : Type) (B : A → Type) : Type where
--   _,_ : (a : A) (b : B a) → Σ A B

-- The non-dependent version is the ordinary Cartesian product.

_×_ : (A B : Type) → Type
A × B = Σ A (λ _ → B)

infixr 5 _×_

-- p : Σ A B
-- p = fst p , snd p  -- eta law
-- p = record { fst = fst p; snd = snd p }

-- The record type with no fields has exactly one inhabitant
-- namely the empty tuple record{}
-- By Curry-Howard, it corresponds to Truth, as
-- no evidence is needed to construct this proposition.

record True : Type where
  -- no-eta-equality

-- t : True
-- t = record {}

trivial : True
trivial = record{}

-- Example: distributivity  A ∧ (B ∨ C) → (A ∧ B) ∨ (A ∧ C)

dist : ∀{A B C : Set} → A × (B ⊎ C) → (A × B) ⊎ (A × C)
dist = {! !}

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
x  ≤ y     = {! !}

-- C-c C-l load
-- C-c C-c case split
-- C-c C-, show goal and assumptions
-- C-c C-. show goal and assumptions and current term
-- C-c C-SPC give

-- -}