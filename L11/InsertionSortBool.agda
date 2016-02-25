-- Advanced Functional Programming course 2016 Chalmers/GU
--
-- 2016-02-25 Guest lecture by Andreas Abel
--
-- Introduction to Agda
--
-- File 1: definition of insertion sort

module InsertionSortBool where

-- Agda is a Haskell-like language
-- comes with little primitives
-- lists are definable

data List (A : Set) : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

infixr 5 _∷_

-- Similar, there are no primitive numbers
-- We define (Peano) natural numbers

data ℕ : Set where
  zero : ℕ
  suc  : (n : ℕ) → ℕ

-- The tools of choice for defining functions are pattern matching and recursion.
-- Each function must be terminating.

length : ∀{A} → List A → ℕ
length []       = zero
length (x ∷ xs) = suc (length xs)

-- As in Haskell, Booleans need not be primitive, since we have call-by-name evaluation.

data Bool :  Set where
  true false : Bool

if_then_else_ : ∀{A : Set} (b : Bool) (x y : A) → A
if true  then x else y = x
if false then x else y = y

infix 0 if_then_else_

-- Comparison of natural numbers (like Ord in Haskell)

_<=_ : ℕ → ℕ → Bool
zero  <= m     = true
suc n <= zero  = false
suc n <= suc m = n <= m

-- Insertion into an ascending list.

insert : (x : ℕ) (ys : List ℕ) → List ℕ
insert x []       = x ∷ []
insert x (y ∷ ys) = if x <= y then x ∷ y ∷ ys else y ∷ insert x ys

-- Insertion sort.

isort : (xs : List ℕ) → List ℕ
isort []       = []
isort (x ∷ xs) = insert x (isort xs)
