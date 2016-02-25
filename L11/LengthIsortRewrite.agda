-- Advanced Functional Programming course 2016 Chalmers/GU
--
-- 2016-02-25 Guest lecture by Andreas Abel
--
-- Introduction to Agda
--
-- File 2: insertion sort preserves length

module LengthIsortRewrite where

open import InsertionSortBool

-- We import the equality type from the standard library.
-- Equality is the smallest equivalence relation
--
-- refl  : ∀{x} → x ≡ x
-- sym   : ∀{x y} → x ≡ y → y ≡ x
-- trans : ∀{x y z} → x ≡ y → y ≡ z → x ≡ z
-- cong  : ∀{x y} f → x ≡ y → f x ≡ f y

open import Relation.Binary.PropositionalEquality

-- Lemma: insertion increases the length of a list by one.
--
-- The logical statement is simply a type signature.
-- The proof is a terminating program of the correct type

length-insert : ∀ x ys → length (insert x ys) ≡ suc (length ys)

-- Case: empty list
-- Show: length (insert x []) ≡ 1

length-insert x [] = refl

-- Case: cons list (y ∷ ys)
-- Show: length (insert x (y ∷ ys)) = suc (suc (length ys))

length-insert x (y ∷ ys) with x <= y

-- Subcase: x <= y is true
-- Show: length (x ∷ y ∷ ys) = suc (suc (length ys))
... | true                             = refl

-- Subcase: x <= y is false
-- Show: length (y ∷ insert x ys) = suc (suc (length ys))
... | false rewrite length-insert x ys = refl

-- We use the induction hypothesis
-- length-insert x ys : length (insert x ys) ≡ suc (length ys)


-- Theorem: sorting preserves the length
-- By induction on the list.

length-isort : ∀ xs → length (isort xs) ≡ length xs

-- Case empty list
-- Show: length [] ≡ length []
length-isort []
        = refl

-- Case cons list (x ∷ xs)
-- Show: length (insert x (isort xs) ≡ suc (length xs)

length-isort (x ∷ xs)
  rewrite length-insert x (isort xs)  -- length (insert x (isort xs)) = suc (length (isort xs))
        | length-isort xs             -- induction hypothesis
        = refl

-- Without comments, it is hard to understand the proof.
-- One only gets an idea what has been used to show the theorem.
