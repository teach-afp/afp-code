-- Advanced Functional Programming course 2016 Chalmers/GU
--
-- 2016-02-25 Guest lecture by Andreas Abel
--
-- Introduction to Agda
--
-- File 6: insertion sort sorts

module InsertionSortLe where

open import Data.List.Base
open import Data.Nat.Base using (ℕ; zero; suc)
open import Logic
open import Le

-- In constructive type theory, we can use proofs of theorems
-- to compute a decision for us.
--
-- We redefine insert in terms of ≤total, as we will need
-- the computed derivations in our correctness proof.

insert : (x : ℕ) (ys : List ℕ) → List ℕ
insert x [] = x ∷ []
insert x (y ∷ ys) with ≤total x y
... | inl _ = x ∷ y ∷ ys
... | inr _ = y ∷ insert x ys

-- Repeating isort with new insert function.

isort : (xs : List ℕ) → List ℕ
isort []       = []
isort (x ∷ xs) = insert x (isort xs)

-- Proving that isort returns an ordered list.

-- Empty and singleton lists are always ordered.
-- A list of length 2 is ordered if the first pair is ordered
-- and the list minus first element is ordered.

module Naive-Ordered where

  data Ordered : (ns : List ℕ) → prop where
    onil  : Ordered []
    osg   : ∀{n} → Ordered (n ∷ [])
    ocons : ∀{n m ms} (p : n  ≤ m) (o : Ordered (m ∷ ms)) → Ordered (n ∷ m ∷ ms)

-- Using this direct definition of Ordered is possible,
-- but gives an awkward proof.

-- The proof becomes smoother if we only distinguish empty and cons list,
-- defining two predicates instead.

-- Ordered' x ys expresses that the first element y of ys, if it exists
-- is greater or equal than x.
-- Also, the rest must be ordered again, this time starting with with y.

data Ordered' (x : ℕ) : List ℕ → prop where
  onil  : Ordered' x []
  ocons : ∀{y ys} (p : x ≤ y) (o : Ordered' y ys) → Ordered' x (y ∷ ys)

-- Ordered can now be defined non-recursively.
-- The empty list is always ordered, and the non-empty list is handled
-- by Ordered'

data Ordered : List ℕ → prop where
  onil  : Ordered []
  ocons : ∀{x xs} (o : Ordered' x xs) → Ordered (x ∷ xs)

-- Agda allows us to overload constructors.

-- Lemma 1: if (x ∷ ys) is ordered and x ≤ y then (x ∷ insert y ys) is ordered.

insert-ordered' : ∀ x y ys (p : x ≤ y) (o : Ordered' x ys) → Ordered' x (insert y ys)
insert-ordered' x y [] p o = ocons p onil
insert-ordered' x y (z ∷ ys) p (ocons p₁ o) with ≤total y z
... | inl q = ocons p (ocons q o)
... | inr q = ocons p₁ (insert-ordered' z y ys q o)

-- Lemma 2: if ys is ordered then (insert x ys) is ordered.

insert-ordered : ∀ x ys (o : Ordered ys) → Ordered (insert x ys)
insert-ordered x [] o = ocons onil
insert-ordered x (y ∷ ys) (ocons o) with ≤total x y
... | inl q = ocons (ocons q o)
... | inr q = ocons (insert-ordered' y x ys q o)

-- Theorem: isort returns an ordered list.

isort-ordered : ∀ ys → Ordered (isort ys)
isort-ordered [] = onil
isort-ordered (x ∷ ys) = insert-ordered x (isort ys) (isort-ordered ys)

-- Q.E.D.
