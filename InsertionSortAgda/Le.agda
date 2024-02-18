-- Advanced Functional Programming course 2016 Chalmers/GU
--
-- 2016-02-25 Guest lecture by Andreas Abel
--
-- Introduction to Agda
--
-- File 5: Inductive relations

module Le where

open import Data.Nat.Base using (ℕ; zero; suc)
open import Logic

-- We can define our own propositions as we can define data types.

-- The less-or-equal-than relation on natural numbers n ≤ m
-- is given as the smallest relation closed under the following
-- two rules:

data _≤_ : (n m : ℕ) → prop where
  z≤  : ∀{m} → zero ≤ m
  s≤s : ∀{n m} (p : n ≤ m) → suc n ≤ suc m

-- n ≤ m is a dependent type.
-- Which derivations inhabit it depends on the values of n and m

-- Example derivation:

1≤3 : 1 ≤ 3
1≤3 = s≤s z≤

¬1≤0 : ¬ (1 ≤ 0)
¬1≤0 ()

-- Theorem : n ≤ n
-- By induction on n

≤refl : ∀ n → n ≤ n
≤refl zero = z≤
≤refl (suc n) = s≤s (≤refl n)

-- Theorem: ≤ is transitive
-- By induction on the two derivations.

≤trans : ∀ {n m k} (p : n ≤ m) (q : m ≤ k) → n ≤ k
≤trans z≤ q = z≤
≤trans (s≤s p) (s≤s q) = s≤s (≤trans p q)

-- Theorem: ≤ is total
-- By induction on n and cases on m

≤total : ∀ n m → (n ≤ m) ∨ (m ≤ n)
≤total zero m = inl z≤
≤total (suc n) zero = inr z≤
≤total (suc n) (suc m) with ≤total n m
... | inl p = inl (s≤s p)
... | inr q = inr (s≤s q)
