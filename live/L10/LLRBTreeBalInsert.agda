-- Left leaning red-black trees in Agda.
--
-- * Balancing is ensured by typing.
-- * Ordering is not ensured, could be added following
--
--   Conor McBride, Keeping your neighbours in order, ICFP 2010

{-# OPTIONS --allow-unsolved-metas #-}

import Level
open import Relation.Binary using (StrictTotalOrder; tri≈; tri<; tri>)
open import Relation.Nullary using (yes; no)

module LLRBTreeBalInsert (order : StrictTotalOrder Level.zero Level.zero Level.zero) where

open module sto = StrictTotalOrder order

A : Set
A = StrictTotalOrder.Carrier order

open import Data.Product.Base using (∃; ∃₂; _×_; _,_; proj₁; proj₂)
open import Data.Nat.Base using (ℕ; zero; suc; _+_)
open import Data.List.Base using (List; []; _∷_; [_]; _++_; foldr)

------------------------------------------------------------------------
-- Type of left-leaning red-black trees.

-- Node coloring.

data Color : Set where
  black : Color
  red   : Color

variable
  n : ℕ
  c c₁ c₂ cₗ cᵣ : Color

-- Trees indexed by color and black-height.
--
-- * Only black nodes increase the height.
-- * Red nodes need to have black children.
-- * Black nodes _can_ have a left red child, the right one _must_ be black.
--
-- The latter characterizes these trees as _left-leaning red-black trees_,
-- which are a representation of 2-3 trees.
--
-- If the right child of a black node can also be red,
-- we speak of (ordinary) red-black trees, which represent 2-3-4 trees.

data Tree' : Color → ℕ → Set where

  -- Leaves are black and contain no data.
  lf : Tree' black 0

  -- Red node.
  nr : (a : A)
     → Tree' black n
     → Tree' black n
     → Tree' red n

  -- Black node.
  nb : (a : A)
     → Tree' c n
     → Tree' black n
     → Tree' black (suc n)

-- We can color a red node as black, increasing the black-height.

redToBlack : Tree' red n → Tree' black (suc n)
redToBlack (nr a l r) = nb a l r

------------------------------------------------------------------------
-- Derived tree constructors

-- Combining three black trees into a big one, making a "3-node".
-- Deterministic.

3black : (a₁₂ a₂₃ : A) (t₁ t₂ t₃ : Tree' black n) → Tree' black (suc n)
3black a₁₂ a₂₃ t₁ t₂ t₃ = nb a₂₃ (nr a₁₂ t₁ t₂) t₃

-- The same seen as a left rotation
--
--
--     a₁₂                      a₂₃
--          a₂₃     ⇒      a₁₂
--   t₁   t₂   t₃        t₁   t₂   t₃
--
rotˡ : (a₁₂ : A) (t₁ : Tree' black n) (a₂₃ : A) (t₂ t₃ : Tree' black n) → Tree' black (suc n)
rotˡ a₁₂ t₁ a₂₃ t₂ t₃ = nb a₂₃ (nr a₁₂ t₁ t₂) t₃

-- Combining four black trees into a big red one.
-- Deterministic.

4black : (a₁₂ a₂₃ a₃₄ : A) (t₁ t₂ t₃ t₄ : Tree' black n) → Tree' red (suc n)
4black a₁₂ a₂₃ a₃₄ t₁ t₂ t₃ t₄ = nr a₂₃ (nb a₁₂ t₁ t₂) (nb a₃₄ t₃ t₄)

------------------------------------------------------------------------
-- Inserting a key into a tree.

-- Result of inserting into a red node:
-- A decomposed red node with children of any color (except red-red).
-- Does not satisfy the red-black invariant (unless both are black).

data OneBlack : (cₗ cᵣ : Color) → Set where
  black-black : OneBlack black black
  red-black   : OneBlack red   black
  black-red   : OneBlack black red

data PreNode (n : ℕ) : Set where
  prenode
    : OneBlack cₗ cᵣ
    → (a : A)
    → Tree' cₗ n
    → Tree' cᵣ n
    → PreNode n

-- Smart constructors for OneBlack.

left-black : (c : Color) → OneBlack black c
left-black black = black-black
left-black red   = black-red

right-black : (c : Color) → OneBlack c black
right-black black = black-black
right-black red   = red-black

mutual

  ------------------------------------------------------------------------
  -- Inserting into black tree.
  --
  -- Can return a red or a black tree.

  insertB : (a : A) → Tree' black n → ∃ λ c → Tree' c n

  -- Insert into leaf: make red singleton tree.

  insertB a lf = {!!}

  -- Insert here.

  insertB a (nb b l r) with compare a b
  insertB a (nb b l r) | tri≈ _ _ _  = {!!}

  -- Insert left into black node.
  -- We can integrate the result as-is into the parent node.

  insertB a (nb {c = black} b l r) | tri< a<b _ _ = let _ , l' = insertB a l in {!!}

  -- Insert left into red node.
  -- We get back a pre-node which we need might need integrate with the parent through rotation.

  insertB a (nb {c = red}   b l r) | tri< a<b _ _ with insertR a l
  ... | prenode black-black c ll lr             = {!!}
  ... | prenode red-black   c ll lr             = {!!}
  ... | prenode black-red   c ll (nr d lrl lrr) = {!!}

  -- Insert right (into black node).
  -- If the result is a red node, we need to rotate or recolor as right children cannot be red.

  insertB a (nb             b l r) | tri> _ _ b<a with insertB a r
  insertB a (nb             b l r) | tri> _ _ b<a | black , r'         = {!!}
  insertB a (nb {c = black} b l r) | tri> _ _ b<a | red   , nr c rl rr = {!!}
  insertB a (nb {c = red  } b l r) | tri> _ _ b<a | red   , r'         = {!!}

  ------------------------------------------------------------------------
  -- Inserting into red tree.
  -- We return a decomposed node possibly violating the red-black invariant.

  insertR : (a : A) → Tree' red n → PreNode n

  insertR a (nr b l r) with compare a b
  ... | tri≈ _ _ _   = {!!}
  ... | tri< a<b _ _ = let c , l' = insertB a l in {!!}
  ... | tri> _ _ b<a = let c , r' = insertB a r in {!!}


------------------------------------------------------------------------
-- Non-indexed interface

data Tree : Set where
  tree : Tree' black n → Tree

singleton : A → Tree
singleton x = tree (nb x lf lf)

-- Insertion (makes the root black)

insert : A → Tree → Tree
insert x (tree t) = tree (makeBlack (proj₂ (insertB x t)))
  where
    ifRed : ∀ {A : Set} → Color → A → A → A
    ifRed red   a b = a
    ifRed black a b = b

    makeBlack : ∀ {c n} → Tree' c n → Tree' black (ifRed c (suc n) n)
    makeBlack {black} t = t
    makeBlack {.red} (nr b t1 t2) = nb b t1 t2


-- Conversion from and to list

fromList : List A → Tree
fromList = foldr insert (tree lf)

toList : Tree → List A
toList (tree t) = toList' t
  where
    toList' : ∀ {c n} → Tree' c n → List A
    toList' lf = []
    toList' (nr a l r) = toList' l ++ [ a ] ++ toList' r
    toList' (nb a l r) = toList' l ++ [ a ] ++ toList' r

-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
