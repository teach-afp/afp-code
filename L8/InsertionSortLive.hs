module InsertionSortLive where

import Control.Monad (liftM2)
import Test.QuickCheck (Arbitrary(..), Gen(..), Property, (==>), collect, quickCheck, elements, frequency, sample)
import qualified Test.QuickCheck as Q

q :: Q.Testable prop => prop -> IO ()
q = Q.quickCheck

-- The familiar insert sort function

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : xs)
  | x <  y    = x : y : xs
  | otherwise = y : insert x xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x : xs) = insert x (isort xs)
-- or equivalently
-- > isort = foldr insert []

-- * Properties

-- | Checking that a list is ordered.
ordered :: Ord a => [a] -> Bool
ordered []           = True
ordered [_x]         = True
ordered (x : y : xs) = x <= y  &&  ordered (y : xs)

-- | 'isort' should produce ordered results.
prop_isort :: [Integer] -> Bool
prop_isort xs = ordered (isort xs)

-- | 'insert' should preserve orderedness. Bad property!  Why:
-- it's quite unlikely that a random longish list will be
-- ordered so we will only test very short lists.  'collect'ing
-- the lengths of the lists reveal this.
--
-- Fix (exercise): write a generator for ordered lists.
prop_ins :: Integer -> [Integer] -> Property
prop_ins x xs = ordered xs ==>
                -- ordered (insert x xs)
                Q.collect (length xs) (ordered (insert x xs))

implies :: Bool -> Bool -> Bool
implies False _ = True
implies True  b = b

-- Generator for Lists

newtype List a = List { theList :: [a] }
  deriving (Eq, Ord, Show)

nil :: List a
nil = List []

cons :: a -> List a -> List a
cons x (List xs) = List (x : xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, genNil), (9, genCons)]
    where
      genNil = return nil
      genCons = do
        x <- arbitrary
        xs <- arbitrary
        return (cons x xs)

-- sample (arbitrary :: Gen (List Int))

newtype SortedList a = SortedList { theSortedList :: [a] }
  deriving (Eq, Ord, Show)

nilS :: SortedList a
nilS = SortedList []

consS :: a -> SortedList a -> SortedList a
consS x (SortedList xs) = SortedList (x : xs)

instance Arbitrary (SortedList Integer) where
  arbitrary = frequency [(1, genNil), (9, genCons)]
    where
      genNil = return nilS
      genCons = do
        xs <- arbitrary
        case xs of
          SortedList [] -> do
            x <- arbitrary
            return $ consS x nilS
          SortedList (y : _) -> do
            x <- elements [0,1,2]
            return $ consS (y - x) xs


prop_ins_sorted :: Integer -> SortedList Integer -> Property
prop_ins_sorted x (SortedList xs) =
                Q.collect (length xs) (ordered (insert x xs))

-- How to test properties about how our function treats elements
-- it thinks are equal? We define a new type El

-- | 'El's are compared only on their keys when sorting.
type Key = Integer
type Value = Integer
data El = El Key Value
  deriving (Eq, Show)

instance Ord El where
  compare (El a _) (El b _) = compare a b
  -- note that compare x y == EQ   /=   x==y in general

instance Q.Arbitrary El where
  -- arbitrary = liftM2 El Q.arbitrary Q.arbitrary
  arbitrary = El <$> arbitrary <*> arbitrary

-- | Sorting twice is the same as sorting once. Not true for our
-- insertion sort!
prop_idem :: [El] -> Bool
prop_idem xs = isort (isort xs) == isort xs

-- What is wrong? (spoiler below)













----------------
-- A fixed (stable) version uses (<=) in insert
insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:xs)
  | x <= y    = x : y : xs
  | otherwise = y : insert' x xs
