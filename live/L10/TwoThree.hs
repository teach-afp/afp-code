module TwoThree where

import Data.Kind (Type)

data Nat :: Type where
  Zero :: Nat
  Suc  :: Nat -> Nat

-- | Two-three trees indexed by their height.

data Tree a n :: Type where
  Leaf  :: Tree a Zero
  Node2 :: Tree a n -> a -> Tree a n -> Tree a (Suc n)
  Node3 :: Tree a n -> a -> Tree a n -> a -> Tree a n -> Tree a (Suc n)

singleton :: a -> Tree a (Suc Zero)
singleton a = Node2 Leaf a Leaf

data Insert a n :: Type where
  Stay :: Tree a n -> Insert a n
  Grow :: Tree a (Suc n) -> Insert a n

insert :: Ord a => a -> Tree a n -> Insert a n
insert a Leaf = Grow $ singleton a
insert a (Node2 l p r) =
  case compare a p of
    EQ -> Stay $ Node2 l a r
    LT -> case insert a l of
      Stay l' -> Stay $ Node2 l p r
      Grow (Node2 l1 p1 l2) -> Stay $ Node3 l1 p1 l2 p r
      Grow (Node3 l1 p1 l2 p2 l3) -> Grow $ Node2 (Node2 l1 p1 l2) p2 (Node2 l3 p r)
    GT -> undefined
insert a (Node3 l p m q r) =
  case compare a p of
    EQ -> Stay $ Node3 l a m q r
    LT -> case insert a l of
      Stay l' -> Stay $ Node3 l' p m q r
      Grow l' -> Grow $ Node2 l' p (Node2 m q r)
    GT -> undefined

data Exists (b :: a -> Type) where
  Ex :: b x -> Exists b

fromInsert :: Insert a n -> Exists (Tree a)
fromInsert (Stay t) = Ex t
fromInsert (Grow t) = Ex t
