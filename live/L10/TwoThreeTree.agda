-- AFP 2024
--
-- Balanced search trees implemented as 2-3-trees in Agda.
-- These are a specific case of B-trees.
--
-- The balancing invariant is enforced by indexing trees by their height,
-- which needs to be the same for all subtrees in a node.

{-# OPTIONS --allow-unsolved-metas #-}

module TwoThreeTree where

open import Agda.Primitive renaming (Set to Type)

open import Data.Bool.Base using (Bool; true; false; if_then_else_)
open import Data.Nat.Base using (ℕ; zero; suc; _+_)
open import Data.Product using (Σ; ∃; _,_)

variable
  A : Type
  n : ℕ

-- 2-3-trees (John Hopcroft, 1970):
-- An inner node has either 2 or 3 subtrees, and consequently 1 or 2 keys.
-- Leafs are unlabeled.
--
-- Trees are balanced: all paths to a leaf have the same length.
-- This invariant can be expressed by indexing the Tree type with its height.

data Tree A : (height : ℕ) → Type where

  leaf  : Tree A 0

  node2 : (l : Tree A n)
          (p : A)
          (r : Tree A n)
        → Tree A (1 + n)

  node3 : (l : Tree A n)
        → (p : A)
        → (m : Tree A n)
        → (q : A)
        → (r : Tree A n)
        → Tree A (1 + n)

-- Defined tree constructors

empty : Tree A 0
empty = leaf

singleton : A → Tree A 1
singleton a = node2 empty a empty

node4 : (a₁ a₂ a₃ : A) (t₁ t₂ t₃ t₄ : Tree A n) → Tree A (2 + n)
node4 a₁ a₂ a₃ t₁ t₂ t₃ t₄ = node2 (node2 t₁ a₁ t₂) a₂ (node2 t₃ a₃ t₄)

node5 : (a₁ a₂ a₃ a₄ : A) (t₁ t₂ t₃ t₄ t₅ : Tree A n) → Tree A (2 + n)
node5 a₁ a₂ a₃ a₄ t₁ t₂ t₃ t₄ t₅ = node2 (node2 t₁ a₁ t₂) a₂ (node3 t₃ a₃ t₄ a₄ t₅)

node6 : (a₁ a₂ a₃ a₄ a₅ : A) (t₁ t₂ t₃ t₄ t₅ t₆ : Tree A n) → Tree A (2 + n)
node6 a₁ a₂ a₃ a₄ a₅ t₁ t₂ t₃ t₄ t₅ t₆ = node2 (node3 t₁ a₁ t₂ a₂ t₃) a₃ (node3 t₄ a₄ t₅ a₅ t₆)

node7 : (a₁ a₂ a₃ a₄ a₅ a₆ : A) (t₁ t₂ t₃ t₄ t₅ t₆ t₇ : Tree A n) → Tree A (2 + n)
node7 a₁ a₂ a₃ a₄ a₅ a₆ t₁ t₂ t₃ t₄ t₅ t₆ t₇ = node3 (node2 t₁ a₁ t₂) a₂ (node2 t₃ a₃ t₄) a₄ (node3 t₅ a₅ t₆ a₆ t₇)

-- For inserting and deleting, we need to be able to compare keys

data Ord : Type where
  lt : Ord
  eq : Ord
  gt : Ord

module _ (A : Set) (compare : A → A → Ord) where

  -- Inserting a key into a tree

  insert : A → Tree A n → {!!}
  insert a leaf = {!!}

  insert a (node2 l p r) with compare a p
  insert a (node2 l p r) | eq = {! node2 l a r !}

  insert a (node2 l p r) | lt with insert a l
  insert a (node2 l p r) | lt | l' = {!!}

  insert a (node2 l p r) | gt = {!!}

  insert a (node3 l p m q r) with compare a p
  insert a (node3 l p m q r) | eq = {! node3 l a m q r !}
  insert a (node3 l p m q r) | lt with insert a l

  insert a (node3 l p m q r) | lt | l' = {!!}

  insert a (node3 l p m q r) | gt with compare a q
  insert a (node3 l p m q r) | gt | eq = {! node3 l p m a r !}
  insert a (node3 l p m q r) | gt | lt = {!!}
  insert a (node3 l p m q r) | gt | gt = {!!}

{-
  ------------------------------------------------------------------------
  -- Deletion

  -- Join two trees

  join : Tree A n → Tree A n → {!!}

  join leaf leaf = {! leaf !}

  -- Join 2-node to 2-node
  join (node2 t₁ a₁ t₂) (node2 t₃ a₃ t₄) with join t₂ t₃
  ... | t₂₃ = {!!}

  -- Join 2-node to 3-node
  join (node2 t₁ a₁ t₂) (node3 t₃ a₃ t₄ a₄ t₅) with join t₂ t₃
  ... | t₂₃ = {!!}

  -- Join 3-node to 2-node: symmetric
  join (node3 t₁ a₁ t₂ a₂ t₃) (node2 t₄ a₄ t₅) with join t₃ t₄
  ... | t₃₄ = {!!}

  join (node3 t₁ a₁ t₂ a₂ t₃) (node3 t₄ a₄ t₅ a₅ t₆) with join t₃ t₄
  ... | t₃₄ = {!!}

  -- Prepending a tree fragment to a tree

  cons : (t₁ : Tree A n) (p : A) (t₂ : Tree A (1 + n)) → {!!}
  cons t₁ p₁ (node2 t₂ p₂ t₃)       = {! node3 t₁ p₁ t₂ p₂ t₃ !}
  cons t₁ p₁ (node3 t₂ p₂ t₃ p₃ t₄) = {! node4 p₁ p₂ p₃ t₁ t₂ t₃ t₄ !}

  -- Appending a tree fragment to a tree

  snoc : (t₁ : Tree A (1 + n)) (p : A) (t : Tree A n) → {!!}
  snoc (node2 t₁ p₁ t₂) p₂ t₃       = {! node3 t₁ p₁ t₂ p₂ t₃ !}
  snoc (node3 t₁ p₁ t₂ p₂ t₃) p₃ t₄ = {! node4 p₁ p₂ p₃ t₁ t₂ t₃ t₄ !}


  -- Deleting a key

  delete : A → Tree A n → {!!}
  delete a leaf = {! leaf !}

  delete a (node2 t p t₁) with compare a p

  -- Delete this 2-node
  delete a (node2 t p t₁) | eq with join t t₁
  ... | t = {!!}

  -- Delete left
  delete a (node2 t p t₁) | lt with delete a t
  ... | t = {!!}

  -- Delete right
  delete a (node2 t p t₁) | gt with delete a t₁
  ... | t₁ = {!!}

  -- Delete in 3-node
  delete a (node3 t p t₁ q t₂) = {!!}


-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
