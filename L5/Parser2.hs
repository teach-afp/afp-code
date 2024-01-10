{-# LANGUAGE GADTs #-}

module Parser2
  ( Parser2     -- :: * -> * -> *
  , symbol      -- :: Parser2 s s
  , pfail       -- :: Parser2 s a
  , (+++)       -- :: Parser2 s a -> Parser2 s a -> Parser2 s a
  , Semantics   -- :: * -> * -> *
  , parse       -- :: Parser2 s a -> Semantics s a
  , run2        -- Just for testing
  ) where

import qualified Parser1 as P1
import Control.Applicative
import Control.Monad (liftM, ap)

-- Final semantics to expose:
type Semantics s a = [s] -> [(a,[s])]

data Parser2 s a where
  SymbolBind   :: (s -> Parser2 s a) -> Parser2 s a
  Fail         :: Parser2 s a
  ReturnChoice :: a -> Parser2 s a -> Parser2 s a

{- | Constructors -}
symbol = SymbolBind \ s -> ReturnChoice s Fail
pfail  = Fail

(+++) :: Parser2 s a -> Parser2 s a -> Parser2 s a
SymbolBind f     +++ SymbolBind g     = SymbolBind \ s -> f s +++ g s
                                        -- L10
p                +++ Fail             = p
                                        -- L6
Fail             +++ q                = q
                                        -- L7
ReturnChoice x p +++ q                = ReturnChoice x (p +++ q)
p                +++ ReturnChoice x q = ReturnChoice x (p +++ q)

instance Functor (Parser2 s) where
  fmap = liftM

instance Applicative (Parser2 s) where
  pure x = ReturnChoice x Fail
  (<*>) = ap

instance Monad (Parser2 s) where
   Fail             >>= k = Fail
   SymbolBind f     >>= k = SymbolBind \ s -> f s >>= k
   ReturnChoice x p >>= k = k x +++ (p >>= k)

{- | Run function -}
parse  = run2

run2 :: Parser2 s a -> Semantics s a
run2 (SymbolBind k)     [] = []
run2 (SymbolBind k) (s:ss) = run2 (k s) ss
run2 Fail                _ = []
run2 (ReturnChoice x p) ss = (x, ss) : run2 p ss
