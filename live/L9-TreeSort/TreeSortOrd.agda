-- Advanced Functional Programming course 2024
-- Chalmers TDA342 / GU DIT260
--
-- Introduction to Dependent Types with Agda
--
-- File 2: How to Keep Your Neighbours in Order (Conor McBride)

module TreeSortOrd where

open import Prelude

-- Comparing natural numbers

Total : ∀{A} (R : Rel A) → Set
Total R = ∀ x y → R x y ⊎ R y x

pattern le z = inl z
pattern ge z = inr z

compare : Total _≤_
compare = {!!}

-- Extension by a least and a greatest element

data Ext (A : Set) : Set where
  ⊤ : Ext A
  # : A → Ext A
  ⊥ : Ext A

ext : ∀{A} → Rel A → Rel (Ext A)
ext R x y = {!!}

module _ {A : Set} (R : Rel A) (compare : Total R) where

  -- Binary search trees (ordered)

  data BST (l u : Ext A) : Set where

    leaf : ext R l u            -- proof that l ≤ u
         → BST l u

    node : (p : A)
           (lt : BST l (# p))   -- l--p
           (rt : BST (# p) u)   --    p--u
         → BST l u              -- l-----u

  -- Insert into an ordered tree

  insert : ∀{l u : Ext A}           -- bounds
         → (p : A)                  -- element to insert
         → (l≤p : ext R l (# p))    -- proof that element is within bounds
         → (p≤u : ext R (# p) u)
         → (t : BST l u)            -- insert into this tree
         → BST l u
  insert p l≤p p≤u (leaf l≤u) = {!!}
  insert p l≤p p≤u (node q lt rt) with compare p q
  ... | le p≤q = {!!}
  ... | ge q≤p = {!!}

  -- Building a binary search tree from a list

  tree : (xs : List A) → BST ⊥ ⊤
  tree []       = leaf _
  tree (x ∷ xs) = insert x _ _ (tree xs)

{-
  -- Ordered lists

  data OList (l u : Ext A) : Set where
    onil  : ext R l u → OList l u
    ocons : (p : A)
            (l≤p : ext R l (# p))
            (ps : OList (# p) u) → OList l u

  -- Flattening a BST

  _++_ : ∀{l m u}
         (xs : OList l m)
         (ys : ∀{k} (k≤m : ext R k m) → OList k u) → OList l u
  ocons x l≤x xs ++ ys = ocons x l≤x (xs ++ ys)
  onil l≤m       ++ ys = ys l≤m

  infixr 1 _++_

  flatten : ∀{l u} (t : BST l u) → OList l u
  flatten (leaf l≤u)     = onil l≤u
  flatten (node p lt rt) = flatten lt ++ λ prf → ocons p prf (flatten rt)

  -- Sorting is flatten ∘ tree

  sort : (xs : List A) → OList ⊥ ⊤
  sort xs = flatten (tree xs)

-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
