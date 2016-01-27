{-# LANGUAGE GADTs #-}
{-|
  A simple embedded language for input/output. Intermediate emb.
-}
module Program.Deep2 where
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

type Input   =  String
type Output  =  String

data Program a where
  PutThen :: Char -> Program a         -> Program a
  GetBind :: (Maybe Char -> Program a) -> Program a
  Return  :: a                        -> Program a

-- | It turns out that bind can still be defined!
instance Monad Program where
  return  = Return
  (>>=)   = bindP

-- | Bind takes the first argument apart:
bindP :: Program a -> (a -> Program b) -> Program b
bindP (PutThen c p)  k   =  PutThen c (bindP p k)
bindP (GetBind f)    k   =  GetBind (\x -> bindP (f x) k)
bindP (Return x)     k   =  k x


-- | Output a character.
putC :: Char -> Program ()
putC c = PutThen c $ Return ()

-- | Input a character.
getC :: Program (Maybe Char)
getC = GetBind Return


type IOSem a = Input -> (a, Input, Output)
-- | The run function is easier than before
run :: Program a -> IOSem a
run (PutThen c p)  i        =  (x,  i',  c : o)
  where (x, i', o)  =  run p i
run (GetBind f)    []       =  run (f Nothing)   []
run (GetBind f)    (c : i)  =  run (f $ Just c)  i
run (Return x)     i        =  (x,  i,  [])


{- GHC 7.10 -}
-- | The following instances are valid for _all_ monads:
instance Functor Program where
  fmap = liftM

instance Applicative Program where
  pure   = return
  (<*>)  = ap
