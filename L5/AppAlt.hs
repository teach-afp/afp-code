{-# OPTIONS_GHC -w #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module AppAlt where

import Prelude hiding (Applicative(..), Alternative(..), liftA3, liftA4, liftM2)

class MultiFunctor f where
  liftA1 :: (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  liftA3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
  -- ...
  liftA0 :: a -> f a

instance MultiFunctor Maybe where
  liftA0 a = Just a

  liftA1 = fmap

  liftA2 f (Just a) (Just b) = Just (f a b)
  liftA2 f _ _ = Nothing

  liftA3 f (Just a) (Just b) (Just c) = Just (f a b c)
  liftA3 f _ _ _ = Nothing

data Stream a = Cons a (Stream a)

forever :: a -> Stream a
forever a = Cons a (forever a)

instance MultiFunctor Stream where
  liftA0 a = forever a
  liftA1 f (Cons a s) = Cons (f a) (liftA1 f s)
  liftA2 f (Cons a1 s1) (Cons a2 s2) = Cons (f a1 a2) (liftA2 f s1 s2)
  -- ...

{- Laws

compose n m f g x1 ... xn y1 ... ym = f x1 ... xn (g y1 ... ym)

compose 0 0 f g     = f g        -- application
compose 0 1 f g y   = f (g y)    -- composition
compose 1 0 f g x   = f x g      -- swap
compose 1 1 f g x y = f x (g y)

Identity law (first functor law)

  liftA1 id x = x

Composition law family

  liftA(n+1+l) f x1 ... xn (liftAm g y1 ... ym) z1 ... zl
  =
  liftA(n+m+l) (compose n m f g) x1 ... xn y1 ... ym z1 ... zl

E.g.

  liftA1 f (liftA1 g y)
  = liftA1 (compose 0 1 f g) y
  = liftA1 (\ x -> f (g x)) y
  = liftA1 (f . g) y

  liftA1 f (liftA0 g)
  = liftA0 (compose 0 0 f g)
  = liftA0 (f g)

  liftA2 f (liftA0 g) (liftA0 h)
  = liftA1 (compose 0 0 f g) (liftA0 h)
  = liftA1 (f g) (liftA0 h)
  = liftA0 (compose 0 0 (f g) h)
  = liftA0 (f g h)

-}

class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- fmap :: (a -> b) -> f a -> f b
-- fmap f x = pure f <*> x

instance {-# OVERLAPPABLE #-} MultiFunctor f => Functor f where
  fmap = liftA1

instance {-# OVERLAPPABLE #-} MultiFunctor f => Applicative f where
  pure = liftA0
  x <*> y = liftA2 (\ f a -> f a) x y  -- liftA2 ($) x y

instance {-# OVERLAPPABLE #-} Applicative f => MultiFunctor f where
  liftA0 = pure
  liftA1 f x = pure f <*> x
  liftA2 f x y = (pure f <*> x) <*> y
  liftA3 f x y z = pure f <*> x <*> y <*> z
  -- ...

instance Applicative Maybe where
  pure = Just
  Just f <*> Just a = Just (f a)
  _ <*> _ = Nothing

instance Applicative Stream where
  pure = forever
  Cons f fs <*> Cons a as = Cons (f a) (fs <*> as)

{- Laws

Functoriality is definable:

  fmap f x = pure f <*> x

Identity (first functor law)

  pure (\ a -> a) <*> x = x

Composition

  pure (\ f g a -> f (g a)) <*> x <*> y <*> z = x <*> (y <*> z)

Interchange

  pure (\ f -> f a) <*> x = x <*> pure a

Homomorphism

  pure (f x) = pure f <*> pure x

These laws follow from the multifunctor laws.

E.g homomorphism:

  pure f <*> pure x
  = liftA2 ($) (liftA0 f) (liftA0 x)   (** see above)
  = liftA0 (($) f x)
  = liftA0 (f $ x)
  = liftA0 (f x)
  = pure (f x)

E.g. interchange

  x <*> pure a
  = liftA2 ($) x (liftA0 a)
  = liftA1 (compose 1 0 ($) a) x
  = liftA1 (\ f -> compose 1 0 ($) a f) x
  = liftA1 (\ f -> ($) f a) x
  = liftA1 (\ f -> f a) x
  = pure (\ f -> f a) <*> x

We can also prove the MultiFunctor laws from the Applicative laws.
-}

-- * Applicative instances of list

-- ** zipWith-like instance

newtype ZipList a = ZipList { getZipList :: [a] }

instance Applicative ZipList where
  pure a = ZipList $ repeat a
  ZipList fs <*> ZipList as = ZipList $ zipWith (\ f a -> f a) fs as
  -- liftA2 f (ZipList as) (ZipList bs) = ZipList (zipWith f as bs)

-- ** Cartesian product (monad-like) instance

instance Applicative [] where
  pure a    = [a]
  fs <*> as = [ f a | f <- fs, a <- as ]

-- * Functor and Applicative from Monad

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = do
  a <- ma
  return (f a)

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = do
  a <- ma
  b <- mb
  return (f a b)

-- ZipList does not have a monad instance such that liftM2 = liftA2!

-- * Functor but not Applicative

data Decorate d a = Decorate { decoration :: d, value :: a }

instance Functor (Decorate d) where
  fmap f (Decorate d a) = Decorate d (f a)

-- instance Applicative (Decorate d) where
--   pure :: a -> Decorate d a
--   pure a = ???

instance Monoid d => Applicative (Decorate d) where
  pure a                          = Decorate mempty a
  Decorate m1 f <*> Decorate m2 a = Decorate (m1 <> m2) (f a)


-- * Alternative

class Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

instance Alternative Maybe where
  empty         = Nothing
  Nothing <|> x = x
  Just a  <|> _ = Just a

instance Alternative [] where
  empty = []
  (<|>) = (++)
