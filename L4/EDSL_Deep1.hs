{-# LANGUAGE GADTs #-}
{-|
  A simple embedded language for input/output. Deep embedding.
-}
module EDSL_Deep1 where
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

type Input   =  String
type Output  =  String

data Program a where
  Put    :: Char -> Program ()
  Get    :: Program (Maybe Char)
  Return :: a -> Program a
  Bind   :: Program a -> (a -> Program b) -> Program b

type IOSem a = Input  -> (a      , Input, Output)
-- | run function: translate syntax to semantics
run :: Program a -> IOSem a
run (Put c)    inp =     (()     ,inp   , c:"")
run (Get)      ""  =     (Nothing, ""   , ""  )
run (Get)      (c:cs) =  (Just c ,cs    , ""  )
run (Return x) inp =     (x      ,inp   , ""  )
run (Bind p g) inp =     (someb  ,someinp', someoutp ++ someoutp')
   where  -- Executes first computation, i.e., p :: Program a
          (somea, someinp, someoutp) = run p inp
          -- Obtains the next one to run by applying g :: a -> Program b
          pb = g somea
          -- It runs pb :: Program b
          (someb, someinp', someoutp') = run pb someinp

putC = Put
getC = Get

example1 :: Program ()
example1 = putC 'a'

echo :: Program ()
echo = getC >>= f
  where f Nothing  = return ()
        f (Just c) = putC c

double_echo :: Program ()
double_echo = getC >>= f
  where f Nothing  = return ()
        f (Just c) = putC c >> putC c

instance Monad Program where
  return  =  Return
  (>>=)   =  Bind


{- GHC 7.10 -}
-- | The following instances are valid for _all_ monads:
instance Functor Program where
  fmap = liftM

instance Applicative Program where
  pure   = return
  (<*>)  = ap
