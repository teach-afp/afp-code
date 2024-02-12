-- Advanced Functional Programming course 2024
-- Chalmers TDA342 / GU DIT260
--
-- Introduction to Agda
--
-- File 1: The Curry-Howard Isomorphism

{-# OPTIONS --allow-unsolved-metas #-}

module Prelude where

-- Natural numbers as our first example of
-- an inductive data type.

data ℕ : Set where
  zero : ℕ
  suc  : (n : ℕ) → ℕ

-- M-x set-input-method RET Agda RET
-- \ b N

plus : ℕ → ℕ → ℕ   -- \ t o
plus zero    y = zero
plus (suc x) y = plus x (suc y)

-- C-c C-l load
-- C-c C-c case split
-- C-c C-, goal & hypotheses
-- C-c C-. goal & hypotheses & term
-- C-c C-SPC give

-- To use decimal notation for numerals, like
-- 2 instead of (suc (suc zero)), connect it
-- to the built-in natural numbers.

{-# BUILTIN NATURAL ℕ #-}

-- Lists are a parameterized inductive data type.

data List (A : Set) : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A   -- C-\ : :
        -- A → List A → List A
        -- A → List A → List A
        -- (x : A) → List A → List A
        -- (x : A) → (xs : List A) → List A
        -- (x : A) (xs : List A) → List A

infixr 6 _∷_

-- infix 4 if_then_else_  _[_!_]foo

-- C-c C-l   load

-- Disjoint sum type A ⊎ B (sometimes written A + B).

data _⊎_ (A B : Set) : Set where  -- \uplus
  inl : A → A ⊎ B
  inr : B → A ⊎ B

infixr 4 _⊎_

-- Curry-Howard-Isomorphism, Brouwer-Heyting-Kolmogorov Interpretation:
-- A proposition is the set of its proofs (aka Propositions-as-Types).

-- Proposition | Type/structure
-------------------------------
--   A ∨ B     |   A ⊎ B
--   A ∧ B     |   A × B
--   A ⇒ B     |   A → B
--   False     |   Empty
--   True      |   Unit

-- The empty sum is the type with 0 alternatives,
-- which is the empty type.
-- By the Curry-Howard-Isomorphism,
-- which views a proposition as the set/type of its proofs,
-- it is also the absurd proposition.

data False : Set where

⊥-elim : False → {A : Set} → A
⊥-elim ()

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

record Σ (A : Set) (B : A → Set) : Set where  -- \GS  \Sigma
  constructor _,_
  field  fst : A
         snd : B fst
open Σ

infixr 5 _,_

-- The non-dependent version is the ordinary Cartesian product.

_×_ : (A B : Set) → Set
A × B = Σ A (λ _ → B)

infixr 5 _×_

-- The record type with no fields has exactly one inhabitant
-- namely the empty tuple record{}
-- By Curry-Howard, it corresponds to Truth, as
-- no evidence is needed to construct this proposition.

record True : Set where

test : True
test = record {}

-- C-c C-a auto

-- Example: distributivity  A ∧ (B ∨ C) → (A ∧ B) ∨ (A ∧ C)

dist : ∀{A B C : Set} → A × (B ⊎ C) → (A × B) ⊎ (A × C)
dist (a , inl x) = inl (a , x)
dist (a , inr x) = inr (a , x)

-- Predicate on ℕ is an element ℕ → Set

-- Relations

-- Type-theoretically, the type of relations 𝓟(A×A) is
--
--   A × A → Prop
--
-- which is
--
--   A × A → Set
--
-- by the Curry-Howard-Isomorphism
-- and
--
--   A → A → Set
--
-- by currying.

Rel : (A : Set) → Set₁
Rel A = A → A → Set

-- Less-or-equal on natural numbers

_≤_ : Rel ℕ
zero  ≤ y     = True
suc x ≤ zero  = False
suc x ≤ suc y = x ≤ y

-- C-c C-l load
-- C-c C-c case split
-- C-c C-, show goal and assumptions
-- C-c C-. show goal and assumptions and current term
-- C-c C-SPC give

2-leq-3 : 2 ≤ 3
2-leq-3 = _

-- C-c C-= constraint
