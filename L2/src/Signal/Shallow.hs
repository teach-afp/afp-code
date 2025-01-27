-- | A very simple library for manipulating continuous signals. Shallow embedding.
module Signal.Shallow
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
applyS :: Signal (a -> b) -> Signal a -> Signal b
mapT   :: (Time -> Time)  -> Signal a -> Signal a
-- * Derived operation
mapS   :: (a -> b)        -> Signal a -> Signal b

type Time = Double

newtype Signal a = Sig {
    -- | Sampling a signal at a given time point.
    -- This is the /semantic function/ of our library.
    sample :: Time -> a
  }

-- | The constant signal.
constS x = Sig (const x)

-- | The time signal
timeS = Sig id

-- | Function application lifted to signals.
fs `applyS` xs = Sig (\ t -> sample fs t (sample xs t))

-- | Mapping a function over a signal.
mapS f xs = constS f `applyS` xs

-- | Transforming the time.
mapT f xs = Sig (sample xs . f)
