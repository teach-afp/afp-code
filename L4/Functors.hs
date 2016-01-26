{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Functos where

import Control.Applicative

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
data AppNotMonad a = AppNotMonad Bool

instance Functor AppNotMonad where
   fmap f (AppNotMonad b) = AppNotMonad b

{-
   Here, AppNotMonad b :: a or AppNotMonad b:: b. So, the
   definition of fmap goes through.

   Exercise: check that AppNotMonad satisfies the functor rules
-}

instance Applicative AppNotMonad where
   pure x = AppNotMonad True -- You choose either True or False
   AppNotMonad b1 <*> AppNotMonad b2 = AppNotMonad (b1 == b2)
                                    -- AppNotMonad b1
                                    -- AppNotMonad b2

   {- Observe that AppNotMonad True is the only value handled by
      the applicative functor

      Exercise: check that AppNotMonad satisfies the applicative laws
   -}

instance Monad AppNotMonad where
    return x = AppNotMonad True
            -- AppNotMonad False
    (AppNotMonad t) >>= f = error "Hopeless!"
    {-
       f needs to be applied to a polymorphic type a, but
       it is applied to a boolean!
    -}
