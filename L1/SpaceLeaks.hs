{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

-- For use with -O we have to disable -ffull-laziness
-- as it may introduce space leaks in e.g. cycle_thunk.
{-# OPTIONS_GHC -fno-full-laziness #-}

module SpaceLeaks where

import Prelude hiding (foldr, foldl, foldl')
import Data.List (genericLength)

-- We evaluate the following definitions according to their space consumption,
-- which we extract from the report produced by passing the options @+RTS -s@,
-- e.g. @ghc -main-is SpaceLeaks SpaceLeaks && ./SpaceLeaks +RTS -s@.


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

-- main = print $ sum [1..10000000]
main1 = print $ sum $ enumFromTo 1 10000000
  where
    sum = go 0 where
      go acc []      = acc
      -- go !acc (x:xs) = go (acc + x) xs
      go acc (x:xs) = let y = acc + x in seq y (go y xs)
    sum' [] = 0
    sum' (x:xs) = x + sum xs
-- main = print $ sum $ map (^2) [1..10000000]

-- * Computing the average of a generated list
------------------------------------------------------------------------

avg_fold :: (Num a, Fractional a) => [a] -> a
avg_fold xs = sum / len
  where
    sum = foldl' (+) 0 xs
    -- len = foldl' (const (+ 1)) 0 xs
    len = foldl' (\ acc _x -> 1 + acc) 0 xs
-- for 1..10⁷: 800MB memory in use; 2.4GB allocated

-- main = print $ avg_fold [1..10^7]
-- main = print $ sum [1..10^7] / foldl' (const (+ 1)) 0 [1..10^7]

-- avg_fold [1..10^7]
-- sum / len where sum = foldl' (+) 0 xs; len = foldl' (const (+ 1)) 0 xs; xs = [1..10^7]
-- foldl' (+) 0 xs / len  where xs = [1..10^7]
-- foldl' (+) 0 xs / len  where xs = 1 : xs'; xs' = [2..10^7]
-- foldl' (+) 1 xs' / len  where xs = 1 : xs'; xs' = [2..10^7]
-- foldl' (+) 3 xs' / len  where xs = 1 : xs'; xs' = 2 : xs2; xs2 = [3..10^7]

avg_thunked :: (Num a, Fractional a) => (() -> [a]) -> a
avg_thunked xs = sum / len
  where
    sum = foldl' (+) 0 (xs ())
    len = foldl' (const (+ 1)) 0 (xs ())

main = print $ avg_thunked $ \ () -> [1..10^7]

avg_iter :: (Num a, Fractional a) => [a] -> a
avg_iter xs = sum / len
  where
    (sum, len) = foldl' step (0, 0) xs
    step (s, l) x = (s + x, l + 1)
-- for 1..10⁷: 2GB memory in use; 3.8GB allocated

-- main = print $ avg_iter $ [1..10^7]

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

-- * Exercises
------------------------------------------------------------------------

-- Preliminaries.

size :: Int
size = 10^7

-- | @consume n xs@ reads @n@ elements from stream @xs@ before
--   returning the given @'String'@.
consume :: String -> Int -> [a] -> String
consume msg = go where
  go n | n <= 0    = \ _xs -> msg
       | otherwise = \case
           []      -> msg
           _x : xs -> go (n - 1) xs

-- | @run msg xs@ consumes @'size'@ elements from stream @xs@ before printing @msg@.
run :: String -> [a] -> IO ()
run msg xs = putStrLn $ consume msg size xs

-- Exercise: Which of the following functions have a space leak?
-- By this we mean they need @O(size)@ memory to run rather than constant (@O(1)@) memory.

-- 1. Reverse
--
-- Bonus question: Do the different versions of @reverse@ differ in memory usage?
-- (And if yes, how?)

reverse1 :: [a] -> [a]
reverse1 = go [] where
  go acc = \case
    []   -> acc
    x:xs -> go (x:acc) xs

reverse2 :: [a] -> [a]
reverse2 = foldl (\ acc x -> x : acc) []

reverse3 :: [a] -> [a]
reverse3 = foldl' (\ acc x -> x : acc) []

-- main = run "Reverse" (reverse1 [1..size])

-- 2. Cycle

-- | @cycle xs@ produces the infinite list @xs ++ xs ++ xs ++ ...@.
cycle1 :: [a] -> [a]
cycle1 xs = xs ++ cycle1 xs

cycle2 :: [a] -> [a]
cycle2 xs = go xs where
  go = \case
    []     -> go xs
    y : ys -> y : go ys

-- main = run "Cycle" (cycle2 [1..size])

cycle_thunk :: (() -> [a]) -> [a]
cycle_thunk f = f () ++ cycle_thunk f

-- main = run "Cycle thunk" (cycle_thunk \ () -> [1..size])

-- 3. Cycle fused with enumFromTo

cycle_upto1 :: Int -> [Int]
cycle_upto1 size = [1..size] ++ cycle_upto1 size

cycle_upto2 :: Int -> [Int]
cycle_upto2 size = go [1..size] where
  go = \case
    []     -> go [1..size]
    y : ys -> y : go ys

cycle_upto3 :: Int -> [Int]
cycle_upto3 size = go 1 where
  go n | n > size = go 1
       | otherwise = n : go (n + 1)

-- main = run "Cycle without list" (cycle_upto1 size)
