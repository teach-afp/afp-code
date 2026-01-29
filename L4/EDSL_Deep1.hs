{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

{-|
  A simple embedded language for input/output. Deep embedding.
-}

module EDSL_Deep1 where

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

type Input  = String
type Output = String

data Program a where
  Put    :: Char -> Program ()
  Get    :: Program (Maybe Char)
  Return :: a -> Program a
  Bind   :: Program a -> (a -> Program b) -> Program b

type IOSem a = Input  ->  (a      , Input, Output)

-- | 'run' function: translate syntax to semantics.
run :: Program a -> IOSem a
run (Put c)    inp     =  (()     , inp  , [c]           )
run (Get)      ""      =  (Nothing, ""   , ""            )
run (Get)      (c:cs)  =  (Just c , cs   , ""            )
run (Return x) inp     =  (x      , inp  , ""            )
run (Bind m k) inp     =  (b      , inp_b, out_a ++ out_b)
  where
    -- Execute first computation, i.e., m :: Program a
    (a, inp_a, out_a) = run m inp
    -- Obtain the next one to run by applying k :: a -> Program b
    m' = k a
    -- Run m' :: Program b
    (b, inp_b, out_b) = run m' inp_a

putC = Put
getC = Get

example1 :: Program ()
example1 = putC 'a'

echo :: Program ()
echo = getC >>= \case
  Nothing -> return ()
  Just c  -> do
    putC c
    echo

double_echo :: Program ()
double_echo = getC >>= \case
  Nothing -> return ()
  Just c  -> do
    putC c
    putC c
    double_echo

instance Monad Program where
  return  =  Return
  (>>=)   =  Bind


{- Type class hierarchy Functor < Applicative < Monad -}

-- | The following instances are valid for _all_ monads:
instance Functor Program where
  fmap = liftM

instance Applicative Program where
  pure   = return
  (<*>)  = ap
