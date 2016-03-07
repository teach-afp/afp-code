{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Add where

class Add a b where           -- uses MultiParamTypeClasses
  type F a b                  -- uses TypeFamilies as well
  add :: a -> b -> F a b

instance Add Integer Double where
  type F Integer Double = Double
  add x y = fromIntegral x + y

instance Add Double Integer where
  type F Double Integer = Double
  add x y = x + fromIntegral y

instance (Num a) => Add a a where -- uses FlexibleInstances
  type F a a = a
  add x y = x + y

instance (Add Integer a) =>
         Add Integer [a] where    -- uses FlexibleContexts
  type F Integer [a] = [F Integer a]
  add x ys = map (add x) ys

-- --------------
--
test :: Double
test = add (3::Integer) (4::Double)

aList :: [Integer]
aList =  [0,6,2,7]

test2 :: [Integer]
test2 = add (1::Integer) aList

test3 :: [Double]
test3 = add (38::Integer) [1700::Double]

-- What is the type of this function?
test4 x y z = add x (add y z)










test4 :: (Add b c, Add a (F b c)) =>
  a -> b -> c -> F a (F b c)
