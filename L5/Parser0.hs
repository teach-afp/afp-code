{-# LANGUAGE GADTs #-}
module Parser0
  (
    Parser0 (..) -- :: * -> * -> *
  , symbol       -- :: Parser0 s s
  , pfail        -- :: Parser0 s a
  , (+++)        -- :: Parser0 s a -> Parser0 s a -> P s a
  , Semantics    -- :: * -> * -> *
  , parse        -- :: Parser0 s a -> Semantics s a
  ) where

import Control.Applicative
import Control.Monad (liftM, ap)

-- Final semantics to expose:
type Semantics s a = [s] -> [(a,[s])]

data Parser0 s a where
  Symbol  ::  Parser0 s s
  Fail    ::  Parser0 s a
  Choice  ::  Parser0 s a -> Parser0 s a -> Parser0 s a
  Return  ::  a -> Parser0 s a
  (:>>=)  ::  Parser0 s a -> (a -> Parser0 s b) -> Parser0 s b


{- | Constructors -}
symbol = Symbol
pfail  = Fail

{- | Combinators -}
(+++)  = Choice

{- | Monadic instance for Parser0 -}
instance Monad (Parser0 s) where
   return = Return
   (>>=)  = (:>>=)

{- | Run function -}
parse  = run0

run0 :: Parser0 s a -> Semantics s a
run0 Symbol          [] = []
run0 Symbol      (s:ss) = [(s,ss)]
run0 Fail            _  = []
run0 (Choice p q)    ss = (run0 p ss) ++ (run0 q ss)
run0 (Return x)      ss = [(x,ss)]
run0 (p :>>= f)      ss = [(y,s2) | (x,s1) <- run0 p ss,
                                    (y,s2) <- run0 (f x) s1]

{- GHC 7.10 -}
instance Functor (Parser0 s) where
   fmap = liftM

instance Applicative (Parser0 s) where
    pure  = return
    (<*>) = ap
