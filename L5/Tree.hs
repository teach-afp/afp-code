module Tree where

import Control.Monad

-- Deduce Functor and Applicative from Monad
-- Example: leaf-labeled trees

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)

instance Functor Tree where
  fmap = liftM

instance Applicative Tree where
  pure  = Leaf
  (<*>) = liftM2 ($)

instance Monad Tree where
  return = pure
  -- (>>=) :: Tree a -> (a -> Tree b) -> Tree b
  Leaf a     >>= k = k a
  Node t1 t2 >>= k = Node (t1 >>= k) (t2 >>= k)

-- Bind is substitution!
