{-# LANGUAGE GADTs #-}
-- | A very simple library for manipulating continuous signals. Deep embedding.
module Signal.Deep
  ( Time
  -- | the 'Signal' type is abstract
  , Signal
  -- * Smart constructors
  , constS, timeS
  -- * Combinators
  , applyS, mapT
  -- * Derived operation
  , mapS
  -- * Run function
  , sample
  ) where

-- * Smart constructors
constS :: a -> Signal a
timeS  ::      Signal Time
-- * Combinators
applyS   :: Signal (a -> b) -> Signal a -> Signal b
mapT   :: (Time -> Time)  -> Signal a -> Signal a
-- * Derived operation
mapS   :: (a -> b)        -> Signal a -> Signal b
-- * Run function
sample :: Signal a -> Time -> a

type Time = Double
data Signal a where
  ConstS :: a -> Signal a
  TimeS  :: Signal Time
  MapT   :: (Time -> Time) -> Signal a -> Signal a
  ApplyS  :: Signal (a -> b) -> Signal a -> Signal b

-- | The constant signal.
constS = ConstS
-- | This definition is Eta-equivalent to
-- > constS x = ConstS x

-- | The time signal
timeS = TimeS

-- | Function application lifted to signals.
applyS = ApplyS

-- | Mapping a function over a signal.
mapS f xs = constS f `applyS` xs

-- | Transforming the time.
mapT = MapT

-- | Sampling a signal at a given time point.
sample (ConstS x)   = const x
sample TimeS        = id
sample (ApplyS f s) = \ t -> sample f t $ sample s t
sample (MapT f s)   = sample s . f
