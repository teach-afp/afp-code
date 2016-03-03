{-# LANGUAGE FlexibleInstances #-}
module Functos where

import Control.Applicative
import Data.Monoid

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

{-- A version of map for Trees --}
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Node t1 t2) = Node (mapTree f t1)
                              (mapTree f t2)

tree = Node (Leaf 2) ((Node (Node (Leaf 3) (Leaf 4)) (Leaf 5)))

ex1 = (+1) `mapTree` tree

{-- Functor instance for Tree --}
instance Functor Tree where
  fmap f (Leaf a)     = Leaf (f a)
  fmap f (Node t1 t2) = Node (fmap f t1) (fmap f t2)

ex2 = (+1) `fmap` tree

{-- Known functors --}

-- Maybe data type
ex3 = (+1) `fmap` (Just 10)

-- Generalized trees
data TreeG a = LeafG a | BranchG [TreeG a]
               deriving Show

instance Functor TreeG where
   fmap f (LeafG a)    = LeafG (f a)
   fmap f (BranchG ts) = BranchG (fmap (fmap f) ts)

ex4 :: TreeG Int
ex4 = (+1)
      `fmap` BranchG [LeafG 10,
                      BranchG [LeafG 11, LeafG 12],
                      BranchG [LeafG 13, LeafG 14, LeafG 15]]



{-- Mapping a multi-argument function to multiple containers at once  --}
mp_fmap :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
mp_fmap f ma mb = let m_b_to_c = fmap f ma
                  in case m_b_to_c of
                          Nothing -> Nothing
                          Just fa -> fmap fa mb

mp_fmap2 :: (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
mp_fmap2 f ma mb mc = case fmap f ma of
                           Nothing -> Nothing
                           Just fa -> case fmap fa mb of
                                          Nothing  -> Nothing
                                          Just fab -> fmap fab mc


-- I am defining my own Maybe type to show how to instancitate Applicative
data MMaybe a = JJust a | NNothing

instance Functor MMaybe where
   fmap f NNothing  = NNothing
   fmap f (JJust a) = JJust (f a)

instance Applicative MMaybe where
    pure            = JJust
    NNothing <*> vv  = NNothing
    JJust f  <*> vv  = fmap f vv

{-------------------}
{-- Not a functor --}
{-------------------}
type NotAFunctor = Equal
newtype Equal a = Equal {runEqual :: a -> a -> Bool}


fmapEqual :: (a -> b) -> Equal a -> Equal b
fmapEqual _f (Equal _op) = Equal $ \_b1 _b2 -> error "Hopeless!"
-- Hopeless! The value of type a is in a negative position!


{------------------------------}
{-- A functor, not applicative --}
{------------------------------}
data Pair r a = P r a

instance Functor (Pair r) where
    fmap f (P r a) = P r (f a)

   {- Observe that the value of type r is kept as it is in
      the container.

      Exercise: check that Pair satisfies the functor laws
   -}


instance Applicative (Pair r) where
   pure x = P (error "Hopeless!") x
   {- To create a container (as pure does), we need to have a value of
      type r, which is never abailable to pure.
   -}
   f <*> v = error "Hopeless!"


{----------------------------------------}
{-- Applicative functor, but not a monad --}
{----------------------------------------}

data Nat = Zero | Suc Nat
           deriving Show

instance Monoid Nat where
   mempty = Zero
   mappend Zero    m = m
   mappend (Suc n) m = Suc (mappend n m)


newtype Phanton o a =  Phanton o
                       deriving Show

instance Functor (Phanton o) where
   fmap f (Phanton o) = Phanton o

instance Monoid o => Applicative (Phanton o) where
   pure x = Phanton mempty
   Phanton o1 <*> Phanton o2 = Phanton (mappend o1 o2)

instance Monad (Phanton Nat) where
    return x        = Phanton Zero
                    -- Here, you can choose any Nat
    Phanton n >>= k = Phanton n
                    {- There is no value of type `a` to
                       pass to `k`!
                       So, we ignore it and we can return
                       a constant Phanton value!
                    -}

onePhanton :: Phanton Nat Int
onePhanton = Phanton (Suc Zero)

exPhanton :: Phanton Nat Int
exPhanton = pure (\x y z -> z) <*> onePhanton <*> onePhanton <*> onePhanton


instance Monad (Phanton Nat) where
   return = pure
   (Phanton n) >>= k = Phanton Zero


{-
   Observe that, by the left identity law,

   return x >>= k == k x

   We know that

   return x >>= k == return x (by definition of >>=)

   So, the monadic law says

   return x == k x

   However, it is easy to come up with a function k which falsifies
   the equation. For example,

   k = \_ -> exPhanton

   Observe that ex1P was created with applicative functors!
-}
