-- | Contains extra notes from 2025-01-23 lecture (AFP 2025 lecture 2).

module TypeClasses2025 where

-- (===) :: Int -> Int -> Bool
-- (===) :: String -> String -> Bool

class Eq' a where            -- simplified version
  (===) :: a -> a -> Bool

  (/==) :: a -> a -> Bool
  x /== y = not (x === y)    -- default method definition

-- instance Eq' Int where
--   (===) x y = (x - y) == 0

-- | Just some type.
data My = One | Two

instance Eq' My where
  (===) x y = case (x, y) of
    (One, One) -> True
    -- other cases...

-- | Just some function.
foo :: Eq' a => a -> Int
foo x = if x === x then 0 else if One === Two then 1 else 2

-- Dictionaries

-- | Dictionary for @class Eq' a@.
data EqD a = EqD
  { eqD  :: a -> a -> Bool
  , neqD :: a -> a -> Bool
  }

-- | @instance Eq' My@.
myEqDict :: EqD My
myEqDict = EqD
  { eqD  = \ x y -> case (x, y) of
      (One, One) -> True
      -- other
  , neqD = \ x y -> not (eqD myEqDict x y)
  }

-- | @foo@ translated to dictonary use.
fooI :: EqD a -> a -> Int
fooI dict x = if eqD dict x x then 0 else if eqD myEqDict One Two then 1 else 2

-- Monomorphisations of foo:
fooMy     :: My -> Int
fooMy     = undefined
fooString :: String -> Int
fooString = undefined

class Eq' a => Ord' a where   -- simplified version
  (<==) :: a -> a -> Bool
  (>==) :: a -> a -> Bool

instance Eq' Int where
  (===) = somePrimitiveEqualityTest
  -- ...

somePrimitiveEqualityTest :: Int -> Int -> Bool
somePrimitiveEqualityTest = (==) -- from the Haskell Prelude

-- instance Eq' a => Eq' [a] where
--   (===) = eqList (===)

-- | A function constrained over @Ord'@.
bar :: Ord' a => a -> a -> Bool
bar x y = x === y

-- | Not the most sensible instance...
instance Ord' My where
  (<==) = (===)
  (>==) = undefined

-- | Dictionary type for @Ord'@.
data OrdD a = OrdD
  { eqDict :: EqD a
  , leqD :: a -> a -> Bool
  , geqD :: a -> a -> Bool
  }

-- | Dictionary for @instance Ord' My@.
myOrdDict :: OrdD My
myOrdDict = OrdD
  { eqDict = myEqDict
  , leqD   = eqD myEqDict
  , geqD   = undefined
  }

-- | @bar@ translated to use of dictionaries.
barI :: OrdD a -> a -> a -> Bool
barI ordD x y = eqD (eqDict ordD) x y


instance Eq' a => Eq' [a] where
  []     === []     = True
  (_:_)  === []     = False
  []     === (_:_)  = False
  (x:xs) === (y:ys) = x === y && xs === ys

-- | (Higher-order) dictionary for @instance Eq' []@.
eqListDict :: EqD a -> EqD [a]
eqListDict eqADict = EqD
  { eqD  = \ as bs -> case (as, bs) of
      ([]   , []  ) -> True
      (_:_  , []  ) -> False
      ([]   , _:_ ) -> False
      (x:xs , y:ys) -> eqD eqADict x y && eqD (eqListDict eqADict) xs ys
  , neqD = \ x y -> not $ eqD (eqListDict eqADict) x y
  }

-- eqMyDict     : instance Eq' My
-- eqListDict   : instance {-# OVERLAPPABLE #-} Eq' a => Eq' [a] where ...
-- eqListMyDict : instance {-# OVERLAPPING  #-} Eq' [My]         where ...
-- Eq' [My] --> eqListDict eqMyDict

-- type List a = [a]
-- instance Eq a => Eq (List a)  -- {-# LANGUAGE TypeSynonymInstances #-}

-- | If you want a different equality on lists, make a @newtype@.
newtype MyList a = MyList { theList :: [a] }
instance Eq a => Eq (MyList a)

-- class Num a

-- {-# LANGUAGE MultiParamTypeClasses #-}
-- class Foo a b where
--   bar :: a -> b -> Int

-- instance Monoid a => Semigroup a where
--   (<>) = mappend

-- foo :: Bar a => a -> Int

type EqT a = a -> a -> Bool

eqList :: EqT a -> EqT [a]
eqList _eq []     []      = True
eqList _eq []     (_:_)   = False
eqList _eq (_:_)  []      = False
eqList  eq (x:xs) (y:ys)  = eq x y  &&  eqList eq xs ys

class Finite a where
  domain :: [a]
  -- A finite list of all values of type a

instance Finite Bool where
  domain = [False, True]

instance (Finite a, Finite b) => Finite (a, b) where
  domain = [(x, y) | x <- domain, y <- domain]

-- Exercise: (more work)
-- instance (Finite a, Finite b) => Finite (a->b) where
--   domain = ?

-- 2015: This is the solution to a problem posed in the lecture:
instance (Finite a, Eq b) => Eq (a -> b) where
  f == g  = all (\ x -> f x == g x) domain

-- and some example uses:
testEqFun :: (Bool, Bool, Bool, Bool)
testEqFun =
  ( not == id
  , not == not
  , (\ x y z -> (x && y) || z) ==
    (\ x y z -> (x || z) && (y || z))
  , (\ x y z -> ((x && y) || z) == ((x || z) && (y || z))) ==
    (\ _ _ _ -> True)
  )

-- Next: Signal.hs
