-- Advanced Functional Programming course 2022
-- Chalmers TDA342 / GU DIT260
--
-- 2022-02-28 Guest lecture by Andreas Abel
--
-- Introduction to Agda
--
-- File 2: How to Keep Your Neighbours in Order (Conor McBride)

module TreeSortOrd where

open import Prelude

-- Comparing natural numbers

Total : ∀{A} (R : Rel A) → Set
Total R = ∀ x y → R x y ⊎ R y x
       -- (x y : A) → R x y ⊎ R y x

pattern le z = inl z
pattern ge z = inr z

compare : Total _≤_
compare zero    y       = le _
compare (suc x) zero    = ge _
compare (suc x) (suc y) = compare x y

-- C-c C-r  refine

-- Extension by a least and a greatest element

data Ext (A : Set) : Set where
  ⊤ : Ext A
  # : A → Ext A
  ⊥ : Ext A

ext : ∀{A} → Rel A → Rel (Ext A)
ext R x ⊤ = True
ext R ⊤ y = False
ext R (# x) (# y) = R x y
ext R ⊥ y = True
ext R x ⊥ = False

--
--               2
--
--     1                            5
--  .    .                3             .
--                     .     .
-- ⊥≤1  1≤2           2≤3   3≤5        5≤⊤

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
  insert p l≤p p≤u (leaf l≤u) = node p (leaf l≤p) (leaf p≤u)
  insert p l≤p p≤u (node q lt rt) with compare p q
  ... | le p≤q = node q (insert p l≤p p≤q lt) rt
  ... | ge q≤p = node q lt (insert p q≤p p≤u rt)

  -- Building a binary search tree from a list

  tree : (xs : List A) → BST ⊥ ⊤
  tree []       = leaf _
  tree (x ∷ xs) = insert x _ _ (tree xs)

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
