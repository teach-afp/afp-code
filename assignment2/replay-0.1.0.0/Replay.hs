module Replay
  (
    Replay

  , io
  , ask
  , emptyTrace
  , addAnswer
  , run
  ) where

import Control.Monad (ap)

data Replay q r a

instance Monad (Replay q r) where
  return = undefined
  (>>=)  = undefined

instance Applicative (Replay q r) where
  pure  = return
  (<*>) = ap

instance Functor (Replay q r) where
  fmap f x = pure f <*> x

data Trace r
--  deriving (Show, Read)

io :: (Show a, Read a) => IO a -> Replay q r a
io = undefined

ask :: q -> Replay q r r
ask = undefined

emptyTrace :: Trace r
emptyTrace = undefined

addAnswer :: Trace r -> r -> Trace r
addAnswer = undefined

run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run = undefined
