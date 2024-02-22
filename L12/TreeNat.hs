{-# OPTIONS_GHC -Wall -Wno-unused-matches #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- | Length-index lists (vectors).

module TreeNat where

import Vec ( Nat(Zero, Suc), SNat(SZero, SSuc) )

------------------------------------------------------------------------
-- * Ordered binary trees for natural numbers

data Empty

-- | Comparing two type-level Nats.
--
-- Truth is the unit type.  Absurdity the empty type.

type family Le (l :: Nat) (r :: Nat) where
  Le Zero r          = ()
  Le (Suc l) Zero    = Empty
  Le (Suc l) (Suc r) = Le l r

-- | Extending a type by a least and a greatest element.

data Ext a where
  Bot :: Ext a
  Top :: Ext a
  Emb :: a -> Ext a

-- | Lifting 'Le' to 'Ext Nat'.

type family LE l r where
  LE Bot r = ()
  LE l Top = ()
  LE l Bot = Empty
  LE Top r = Empty
  LE (Emb l) (Emb r) = Le l r

-- | Search trees indexed by an interval of keys they can contain.
--
-- Keys are type-level natural numbers.
-- In the tree a value-level key is stored via the singleton connection.
--
data Tree (l :: Ext Nat) (r :: Ext Nat) where
  Leaf :: LE l r -> Tree l r
  Node :: SNat p -> Tree l (Emb p) -> Tree (Emb p) r -> Tree l r

------------------------------------------------------------------------
-- * Inserting into a search tree.

-- | What it means for 'Le' to be total.

data Total x y where
  Leq :: Le x y -> Total x y
  Geq :: Le y x -> Total x y

-- | Proving the totality of 'Le' means comparing numbers.

comp :: SNat x -> SNat y -> Total x y
comp SZero    _        = Leq ()
comp (SSuc x) SZero    = Geq ()
comp (SSuc x) (SSuc y) =
  case comp x y of
    Leq prf -> Leq prf
    Geq prf -> Geq prf

-- | Insert into ordered search tree.

insert :: SNat p -> LE l (Emb p) -> LE (Emb p) r -> Tree l r -> Tree l r
insert p lp pr = \case
  Leaf _ -> Node p (Leaf lp) (Leaf pr)
  Node q t1 t2 ->
    case comp p q of
      Leq pq -> Node q (insert p lp pq t1) t2
      Geq qp -> Node q t1 (insert p qp pr t2)

------------------------------------------------------------------------
-- * Using search trees for sorting.

-- Turning a value into a singleton value.

-- We would like to write the following:
--
-- sing :: (n :: Nat) -> SNat n
-- sing Zero    = SZero
-- sing (Suc n) = SSuc (sing n)
--
-- Haskell does not have Pi-types.
-- Use an existential type instead:
--
-- sing :: Nat -> ∃ (n :: Nat). SNat n
--
-- Note: the n is here a type-level Nat!

data SomeNat where
  SomeNat :: forall n. SNat n -> SomeNat

-- | Convert a natural number into a singleton natural number.
--
-- However, the existential obscures the connection between input and output.

sing :: Nat -> SomeNat
sing Zero = SomeNat SZero
sing (Suc n) = case sing n of
  SomeNat k -> SomeNat (SSuc k)

-- | Turn a list into a search tree.

tree :: [Nat] -> Tree Bot Top
tree [] = Leaf ()
tree (n : ns) = case sing n of
  SomeNat k -> insert k () () (tree ns)

-- | Convert a singleton back to a non-indexed value.

unSing :: SNat n -> Nat
unSing SZero    = Zero
unSing (SSuc n) = Suc (unSing n)

-- | Turn a search tree into a list.

list :: Tree l r -> [Nat]
list (Leaf _)       = []
list (Node p t1 t2) = list t1 ++ unSing p : list t2

-- | Sorting function.

sorting :: [Nat] -> [Nat]
sorting = list . tree

test :: [Nat]
test = sorting $ read "[3,1,4,1,5,9,2,6,3]"


-- -}
-- -}
-- -}
