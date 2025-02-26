{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Tree where

import Data.Bifunctor
import Data.Kind (Type)

import Vec ( Nat(Zero, Suc), SNat(SZero, SSuc) )

------------------------------------------------------------------------
-- Singletons and their existential closure.

type family Sing k :: k -> Type  -- declaration of an open type family

data SomeSing k where
  Wrap :: Sing k p -> SomeSing k

------------------------------------------------------------------------
-- Comparing elements of type @'Sing' k@.

type family Le (x :: k) (y :: k)

class Total k where
  comp :: Sing k x -> Sing k y -> Either (Le x y) (Le y x)

------------------------------------------------------------------------
-- Extending a type/kind by a least and greatest element.

data Ext a where
  Bot :: Ext a
  Top :: Ext a
  Emb :: a -> Ext a

data Empty where

-- Just like a class can get new instances,
-- an open type family can always get new clauses:

type instance Le Bot     Bot     = ()
type instance Le Bot     (Emb y) = ()
type instance Le Bot     Top     = ()
type instance Le (Emb x) Bot     = Empty
type instance Le (Emb x) Top     = ()
type instance Le (Emb x) (Emb y) = Le x y
type instance Le Top     (Emb y) = Empty
type instance Le Top     Bot     = Empty
type instance Le Top     Top     = ()

------------------------------------------------------------------------
-- Search trees with keys in kind k.

data Tree k (l :: Ext k) (r :: Ext k) where

  Leaf :: Le l r
       -> Tree k l r

  Node :: Sing k p
       -> Tree k l (Emb p)
       -> Tree k (Emb p) r
       -> Tree k l r

insert :: Total k
  => Sing k p
  -> Le l (Emb p)
  -> Le (Emb p) r
  -> Tree k l r
  -> Tree k l r

insert p lp pr (Leaf _) = Node p (Leaf lp) (Leaf pr)
insert p lp pr (Node q t1 t2) =
  case comp p q of
    Left  pq -> Node q (insert p lp pq t1) t2
    Right qp -> Node q t1 (insert p qp pr t2)

------------------------------------------------------------------------
-- * Promoting a value to the type-level via the singleton connection.

class Reflect a k | a -> k where
  sing :: a -> SomeSing k

tree :: (Reflect a k, Total k) => [a] -> Tree k Bot Top
tree []     = Leaf ()
tree (a:as) = case sing a of
  Wrap x -> insert x () () (tree as)

------------------------------------------------------------------------
-- * Demoting a type-level value to its runtime representation.

class Reify a k where
  unSing :: Sing k p -> a

list :: (Reify a k) => Tree k l r -> [a]
list (Leaf _) = []
list (Node p t1 t2) = list t1 ++ unSing p : list t2

sorting :: (Reflect a k, Reify a k, Total k) => [a] -> [a]
sorting = list . tree

------------------------------------------------------------------------
-- * Example with keys = natural numbers.

type instance Sing Nat = SNat

-- Promotion

instance Reflect Nat Nat where
  sing Zero    = Wrap SZero
  sing (Suc n) = case sing n of
    Wrap k -> Wrap (SSuc k)

-- Demotion

instance Reify Nat Nat where
  unSing SZero    = Zero
  unSing (SSuc k) = Suc (unSing k)

-- Order

type instance Le Zero    y       = ()
type instance Le (Suc x) Zero    = Empty
type instance Le (Suc x) (Suc y) = Le x y

instance Total Nat where
  comp SZero    _        = Left ()
  comp (SSuc _) SZero    = Right ()
  comp (SSuc l) (SSuc r) = comp l r

test :: [Nat]
test = sorting $ read "[3,1,4,1,5,9,2,6,3]"



-- -}
-- -}
-- -}
-- -}
-- -}
