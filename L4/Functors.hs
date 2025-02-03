{-# LANGUAGE FlexibleInstances #-}

module Functors where

import Control.Applicative
import Data.Bifunctor (first)
import Data.Monoid

-- * Functors

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving Show

-- | A version of map for Trees.
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a)     = Leaf (f a)
mapTree f (Node t1 t2) = Node (mapTree f t1)
                              (mapTree f t2)

tree = Node (Leaf 2) ((Node (Node (Leaf 3) (Leaf 4)) (Leaf 5)))

ex1 = (+ 1) `mapTree` tree

{-- Functor instance for Tree --}
instance Functor Tree where
  fmap f (Leaf a)     = Leaf (f a)
  fmap f (Node t1 t2) = Node (fmap f t1) (fmap f t2)

ex2 = (+ 1) `fmap` tree

{-- Known functors --}

-- Maybe data type
ex3 = (+ 1) `fmap` (Just 10)

-- Generalized trees
data TreeG a = LeafG a | BranchG [TreeG a]
               deriving Show

instance Functor TreeG where
   fmap f (LeafG a)    = LeafG (f a)
   fmap f (BranchG ts) = BranchG (fmap (fmap f) ts)

ex4 :: TreeG Int
ex4 = (+ 1)
      `fmap` BranchG [LeafG 10,
                      BranchG [LeafG 11, LeafG 12],
                      BranchG [LeafG 13, LeafG 14, LeafG 15]]

-- Functor instance for State monad

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State g) = State \ s -> first f (g s)
  {- From Monad:
  fmap f m = do
    a <- m
    return (f a)
  fmap = liftM
  -}

-- first :: (a1 -> a2) -> (a1, b) -> (a2, b)

{- Functor instance for IO

instance Functor IO where
  fmap f m = do
    a <- m
    return (f a)
-}

-- Free monad over functor f

data Free f a = Return a | Oper (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Return a) = Return (f a)
  fmap f (Oper ts)  = Oper (fmap (fmap f) ts)

-- * Applicative functors

-- | Unary map ('fmap', 'liftA').
mapMaybe1 :: (a -> b) -> Maybe a -> Maybe b
mapMaybe1 f (Just a) = Just (f a)
mapMaybe1 f _ = Nothing

-- | Binary map ('zipWith', 'liftA2').
mapMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
mapMaybe2 f (Just a) (Just b) = Just (f a b)
mapMaybe2 f _ _ = Nothing

-- | Ternary map ('zipWith3', 'liftA3').
mapMaybe3 :: (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
mapMaybe3 f (Just a) (Just b) (Just c) = Just (f a b c)
mapMaybe3 f _ _ _ = Nothing

-- | Nullary map ('pure').
mapMaybe0 :: a -> Maybe a
mapMaybe0 f = Just f
-- no catch-all case here!


{-- Mapping a multi-argument function to multiple containers at once  --}
fmap2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
fmap2 f ma mb =
  case fmap f ma of
    Nothing -> Nothing
    Just fa -> fmap fa mb

fmap3 :: (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
fmap3 f ma mb mc =
  case fmap f ma of
    Nothing -> Nothing
    Just fa ->
      case fmap fa mb of
        Nothing  -> Nothing
        Just fab -> fmap fab mc


-- Define our own Maybe type to show how to instantiate Applicative:
data MMaybe a = JJust a | NNothing

instance Functor MMaybe where
   fmap f NNothing  = NNothing
   fmap f (JJust a) = JJust (f a)

instance Applicative MMaybe where
    pure             = JJust
    NNothing <*> vv  = NNothing
    JJust f  <*> vv  = fmap f vv

{-------------------}
{-- Not a functor --}
{-------------------}

type NotAFunctor = Equal
newtype Equal a = Equal { runEqual :: a -> a -> Bool }


fmapEqual :: (a -> b) -> Equal a -> Equal b
fmapEqual _f (Equal _op) = Equal \_b1 _b2 -> error "Hopeless!"
-- Hopeless! The value of type a is in a negative position!


{--------------------------------}
{-- A functor, not applicative --}
{--------------------------------}

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
      type r, which is never available to pure.
   -}
   P r1 f <*> P r2 a = P (r1 {- ?? Or r2 ? -}) (f a)

-- Exercise:  Define:
-- instance Monoid r => Applicative (Pair r)

{------------------------------------------}
{-- Applicative functor, but not a monad --}
{------------------------------------------}

data Nat
  = Zero
  | Suc Nat
  deriving Show

instance Semigroup Nat where
  Zero  <> m = m
  Suc n <> m = Suc (n <> m)

instance Monoid Nat where
   mempty = Zero
   -- Inherited from Semigroup
   -- mappend = (<>)

newtype Phantom a = Phantom Nat
  deriving Show

instance Functor Phantom where
   fmap f (Phantom n) = Phantom n

instance Applicative Phantom where
   pure _ = Phantom mempty
   Phantom n1 <*> Phantom n2 = Phantom (n1 <> n2)

instance Monad Phantom where
    return          = pure
                    -- Here, you can choose any Nat
    Phantom n >>= k = Phantom n
                    {- There is no value of type `a` to
                       pass to `k`!
                       So, we ignore it and we can return
                       a constant Phantom value!
                    -}

onePhantom :: Phantom ()
onePhantom = Phantom (Suc Zero)

exPhantom :: Phantom ()
exPhantom = pure (\ x y z -> z) <*> onePhantom <*> onePhantom <*> onePhantom


{-
   Observe that, by the left identity law,

   return x >>= k == k x

   We know that

   return x >>= k == return x (by definition of >>=)

   So, the monadic law says

   return x == k x

   However, it is easy to come up with a function k which falsifies
   the equation. For example,

   k = \_ -> exPhantom

   Observe that ex1P was created with applicative functors!
-}
