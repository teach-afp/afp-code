-- Advanced Functional Programming course 2016 Chalmers/GU
--
-- 2016-02-25 Guest lecture by Andreas Abel
--
-- Introduction to Agda
--
-- File 3: insertion sort preserves length, proved using equality chains

module LengthIsortChain where

open import InsertionSortBool
open import Relation.Binary.PropositionalEquality

-- ≡-Reasoning gives us a defined (!) notation to write equality chains.

open ≡-Reasoning

-- Lemma: insertion increases the length of a list by one.
-- Proof by induction on ys.

length-insert : ∀ x ys → length (insert x ys) ≡ suc (length ys)

-- Case: empty list
length-insert x [] = refl

-- Case: cons list (y ∷ ys)
length-insert x (y ∷ ys) with x <= y

-- Subcase: x <= y is true
... | true  = begin

  length (x ∷ y ∷ ys)   ≡⟨⟩
  suc (length (y ∷ ys))
  ∎

-- Subcase: x <= y is false
... | false = begin

  length (y ∷ insert x ys)     ≡⟨ refl ⟩
  suc (length (insert x ys))   ≡⟨ cong suc (length-insert x ys) ⟩
  suc (suc (length ys))        ≡⟨ refl ⟩
  suc (length (y ∷ ys))
  ∎


-- Theorem: sorting preserves the length
-- By induction on the xs.

length-isort : ∀ xs → length (isort xs) ≡ length xs

-- Case: empty list
length-isort [] = begin
  length (isort [])  ≡⟨⟩
  length {A = ℕ} []  ≡⟨⟩
  zero               ≡⟨⟩
  length {A = ℕ} []
  ∎

-- Case: cons list (y ∷ ys)
length-isort (x ∷ xs) = begin
  length (isort (x ∷ xs))       ≡⟨⟩
  length (insert x (isort xs))  ≡⟨ length-insert _ _ ⟩
  suc (length (isort xs))       ≡⟨ cong suc (length-isort xs) ⟩
  suc (length xs)               ≡⟨⟩
  length (x ∷ xs)
  ∎
