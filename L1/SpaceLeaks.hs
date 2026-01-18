{-# LANGUAGE LambdaCase #-}

module SpaceLeaks where

import Prelude hiding (foldr, foldl, foldl')
import Data.List (genericLength)

-- * Large summation @sum [1..10000000]@ via folds
------------------------------------------------------------------------

-- | @ foldr f e [x1,x2..xn] = f x1 $ f x2 $ ... $ f xn e @
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f e = go where
  go = \case
    []   -> e
    x:xs -> f x $ go xs

sumr = foldr (+) 0
-- for 1..10⁷: 870MB memory in use; 2GB allocated

-- | @ foldl f e [x1,x2..xn] = e `f` x1 `f` x2 ... `f` xn @
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f e = go e where
  go acc = \case
    []   -> acc
    x:xs -> go (acc `f` x) xs

suml = foldl (+) 0
-- for 1..10⁷: 1GB memory in use; 1.5GB allocated

-- | @ foldl' f e [x1,x2..xn] = e `f` x1 `f` x2 ... `f` xn @
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f e = go e where
  go !acc = \case
    []   -> acc
    x:xs -> go (acc `f` x) xs

suml' = foldl' (+) 0
-- for 1..10⁷: 6MB memory in use (baseline); 1.2GB allocated

-- | @ foldl' f e [x1,x2..xn] = e `f` x1 `f` x2 ... `f` xn @
foldl'1 :: (b -> a -> b) -> b -> [a] -> b
foldl'1 f e = go e where
  go acc = \case
    []   -> acc
    x:xs -> let acc' = acc `f` x in
            acc' `seq` go acc' xs

suml'1 = foldl'1 (+) 0
-- for 1..10⁷: 6MB memory in use (baseline); 880MB allocated

main = print $ sum $ map (^2) [1..10000000]

-- * Computing the average of a generated list

avg_fold :: (Num a, Fractional a) => [a] -> a
avg_fold xs = sum / len
  where
    sum = foldl' (+) 0 xs
    len = foldl' (const (+ 1)) 0 xs
-- for 1..10⁷: 800MB memory in use; 2.4GB allocated

avg_iter :: (Num a, Fractional a) => [a] -> a
avg_iter xs = sum / len
  where
    (sum, len) = foldl' step (0, 0) xs
    step (s, l) x = (s + x, l + 1)
-- for 1..10⁷: 2GB memory in use; 3.8GB allocated

avg_iter'0 :: (Num a, Fractional a) => [a] -> a
avg_iter'0 xs = sum / len
  where
    (sum, len) = foldl' step (0, 0) xs
    step !(s, l) x = (s + x, l + 1)      -- ! achieves nothing here
-- for 1..10⁷: 2GB memory in use; 3.8GB allocated

avg_iter' :: (Num a, Fractional a) => [a] -> a
avg_iter' xs = sum / len
  where
    (sum, len) = foldl' step (0, 0) xs
    step (!s, !l) x = (s + x, l + 1)
-- for 1..10⁷: 6MB memory in use (baseline); 3.6GB allocated

avg_iter'1 :: (Num a, Fractional a) => [a] -> a
avg_iter'1 xs = sum / len
  where
    (sum, len) = foldl' step (0, 0) xs
    step ~(!s, !l) x = (s + x, l + 1)   -- lazy match prevents strict match
-- for 1..10⁷: 2.2GB memory in use; 4.5GB allocated

-- main = print $ avg_iter' [1..10^7]
