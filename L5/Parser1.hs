{-# LANGUAGE GADTs #-}

module Parser1
  ( Parser1     -- :: * -> * -> *
  , symbol      -- :: Parser1 s s
  , pfail       -- :: Parser1 s a
  , (+++)       -- :: Parser1 s a -> Parser1 s a -> P s a
  , Semantics   -- :: * -> * -> *
  , parse       -- :: Parser1 s a -> Semantics s a
  , run1        -- Just for testing
  ) where

import qualified Parser0 as P0
import Control.Applicative
import Control.Monad (liftM, ap)

-- Final semantics to expose:
type Semantics s a = [s] -> [(a,[s])]

data Parser1 s a where
  SymbolBind ::  (s -> Parser1 s a) -> Parser1 s a
  Fail       ::  Parser1 s a
  Choice     ::  Parser1 s a -> Parser1 s a -> Parser1 s a
  Return     ::  a -> Parser1 s a

{- | Constructors -}
symbol = SymbolBind Return
pfail  = Fail

{- | Combinators -}
(+++)  = Choice

{- | Functor instance from Monad -}
instance Functor (Parser1 s) where
  fmap = liftM

{- | Applicative instance from monadic bind -}
instance Applicative (Parser1 s) where
  pure  = Return
  (<*>) = ap

{- | Monad instance for Parser1 -}
instance Monad (Parser1 s) where
  Fail         >>= k = Fail
  Choice p q   >>= k = Choice (p >>= k) (q >>= k)
  Return x     >>= k = k x
  SymbolBind f >>= k = SymbolBind \ s -> f s >>= k
{- Derivation
  SymbolBind f >>= k = (symbol >>= f) >>= k
                     = symbol >>= \ s -> (f s >>= k)
                     = SymbolBind \ s -> (f s >>= k)
-}
{- | Run function -}
parse  = run1

run1 :: Parser1 s a -> Semantics s a
run1 (SymbolBind k)     [] = []
run1 (SymbolBind k) (s:ss) = run1 (k s) ss
run1 Fail                _ = []
run1 (Choice p q)       ss = run1 p ss ++ run1 q ss
run1 (Return x)         ss = [(x,ss)]


{- | Transforming parsers -}
cast :: P0.Parser0 s a -> Parser1 s a
cast P0.Symbol       = SymbolBind Return -- L1
cast P0.Fail         = Fail
cast (P0.Choice p q) = Choice (cast p) (cast q)
cast (P0.Return x)   = Return x

-- The core of the translation: interpreting bind
cast (P0.Symbol P0.:>>= k)       = SymbolBind (cast . k)
                                   -- def of SymbolBind

cast (P0.Fail P0.:>>= _)         = Fail
                                   -- Parser law, L4.

cast ((P0.Choice p q) P0.:>>= k) = Choice (cast (p P0.:>>= k))
                                          (cast (q P0.:>>= k))
                                   -- Parser law, L5

cast ((P0.Return x) P0.:>>= k)   = cast (k x)
                                   -- monad law, L1

cast ((p P0.:>>= k') P0.:>>= k)  = cast (p P0.:>>= \x -> k' x P0.:>>= k)
                                   -- monad law, L3
