{-# LANGUAGE BangPatterns #-}

module Lect1 where

import Data.Maybe (isNothing)

-- An example of a pure function returning an Int

f :: String -> Int
f = read

-- An example of a pure function returning an IO action (which in turn
-- returns an Int). Sometimes called an "impure function returning an Int".

g :: String -> IO Int
g xs = do
  putStrLn xs
  ys <- getLine
  return (f ys)

-- An IO action can be executed several times and can give different results

testg :: IO [Int]
testg = do
  let a = g "Shoe size?"
  s1 <- a
  s2 <- a
  s3 <- a
  return [s1, s2, s3]

testg' :: IO [Int]
testg' = sequence $ replicate 3 $ g "Shoe size?"
  -- replicate :: Int -> a -> [a]
  -- sequence  :: [IO Int] -> IO [Int]

-----------------------
--- Programming with IO
-----------------------

hello :: IO ()
hello = do
  putStrLn "Hello! What is your name?"
  name <- getLine
  putStrLn ("Hi, " ++ name ++ "!")

printTable :: [String] -> IO ()
printTable = prnt 1  -- Note the use of partial application
 where
  prnt :: Int -> [String] -> IO ()
  prnt _i []    = return ()
  prnt i (x:xs) = do
    putStrLn (show i ++ ": " ++ x)
    prnt (i+1) xs

lussekatter :: [String]
lussekatter = ["1g saffran", "1kg (17dl) vetemjöl", "5dl mjölk",
               "250g mager kesella", "50g jäst", "1.5dl socker", "0.5tsk salt"]

testTable :: IO ()
testTable = printTable lussekatter

printTable2 :: [String] -> IO ()
printTable2 xs =
  sequence_ [ putStrLn (show i ++ ":" ++ x)
            | (x,i) <- xs `zip` [1..length xs]
            ]

testTable2 :: IO ()
testTable2 = printTable2 lussekatter

-- Exercise: write printTable using the zipWithM_ :: (a -> b -> IO ()) -> [a] -> [b] -> IO ()
-- zipWithM_ f as bs = mapM_ (uncurry f) $ zip as bs
-- -- uncurry f :: (a,b) -> IO ()

-- Law:  (do return e; e') == e'
-- Law:  (do _ <- return e; e') == e'
-- Law:  (do x <- return e; e') == e'[x=e]
-- Law:  (return e >>= \ x -> e') == (let x = e in e') == e'[x=e]

--------------------
--- Evaluation order
--------------------

eager :: (Int, Int) -> Int
eager (!_x, y) = y -- the function is strict on the first argument

lazy :: (Int, Int) -> Int
lazy (_x, y) = y



fun :: Maybe Int -> Int
fun mx  | isNothing mx   = 0
        | otherwise      = x + 3
 where
  x = fromJust mx

fromJust :: Maybe a -> a -- also available in module Data.Maybe
fromJust (Just x)  = x
fromJust Nothing   = error "fromJust: Nothing is not allowed"

testFun :: (Int, Int)
testFun = (fun (Just 100), fun Nothing)

----------------

expn :: Integer -> Integer
expn n | n <= 1    = 1
       | otherwise = expn (n-1) + expn (n-2)

choice :: Bool -> a -> a -> a
choice True  fn  _t  =  fn
choice False _fn  t  =  t

testChoice1 :: Integer
testChoice1 = choice False 17 (expn 99)

testChoice2 :: Integer
testChoice2 = choice False 17 (error "Don't touch me!")

-- Laziness
testLazy1 = choice False 17 undefined
testLazy2 = head [3, undefined, 17]
testLazy3 = head (3:4:error "no tail")
testLazy4 = head [error "no first elem", 17, 13]
testLazy5 = head (error "no list at all")

----------------

strange :: Bool -> Integer
strange False = 17
strange True  = 17

testStrange = strange undefined

----------------
f' :: Integer -> Integer
f' x = (x - 2) * 10

foo :: Integer -> Integer
foo x = f' x + f' x

bar :: Integer -> Integer -> Integer
bar x y = f' 17 + x + y

testBar =  bar 1 2 + bar 3 4

----------------

testInfList1 n = take n [3..]
testInfList2 xs = xs `zip` [1..]

----

printTable3 :: [String] -> IO ()
printTable3 xs =
  sequence_ [ putStrLn (show i ++ ":" ++ x)
            | (x,i) <- xs `zip` [1..]
            ]
testTable3 = printTable3 lussekatter

----------------

-- the real iterate is defined in the standard prelude
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

testIterate :: [Integer]
testIterate = iterate' (2*) 1
testIt :: [Integer]
testIt = take 12 testIterate

repeat' :: a -> [a]
repeat' x = x : repeat' x

cycle' :: [a] -> [a]
cycle' xs = xs ++ cycle' xs

repeat'' :: a -> [a]
repeat'' = iterate id

cycle'' :: [a] -> [a]
cycle'' xs = concat (repeat xs)

----------------
-- 2015: Skipped replicate' and group in the lecture

replicate' :: Int -> a -> [a]
replicate' n x = take n (repeat x)

group :: Int -> [a] -> [[a]]
group n = takeWhile (not . null)
        . map (take n)
        . iterate (drop n)

-- 2015: Only a brief explanation
primes :: [Integer]
primes = sieve [2..]
 where
  sieve (p:xs)  = p : sieve [ y | y <- xs, y `mod` p /= 0 ]
  sieve []      = error "sieve: empty list is impossible"

----------------

data Labyrinth
  = Crossroad
  { what  :: String
  , left  :: Labyrinth
  , right :: Labyrinth
  }

labyrinth :: Labyrinth
labyrinth = start
 where
  start  = Crossroad "start"  forest town
  town   = Crossroad "town"   start  forest
  forest = Crossroad "forest" town   exit
  exit   = Crossroad "exit"   exit   exit

showLabyrinth :: Labyrinth -> String
showLabyrinth (Crossroad _label _left _right) = error "Exercise!"

-- Next: TypeClasses.hs
