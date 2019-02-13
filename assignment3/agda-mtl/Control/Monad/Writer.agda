module Control.Monad.Writer where

open import Prelude
open import Control.Monad
open import Data.Monoid

Writer : ∀ (W : Set) → Set → Set
Writer = ?

runWriter : ∀ {W A} → Writer W A → A × W
runWriter = ?

execWriter : ∀ {W A} → Writer W A → W
execWriter ma = snd (runWriter ma)

evalWriter : ∀ {W A} → Writer W A → A
evalWriter ma = fst (runWriter ma)

instance
  WriterFunctor : ∀ {W} → Functor (Writer W)
  WriterFunctor = ?

  WriterMonad : ∀ {W} {{_ : Monoid W}} → Monad (Writer W)
  WriterMonad = ?

-- https://hackage.haskell.org/package/mtl/docs/Control-Monad-Writer-Class.html#t:MonadWriter

writer : ∀ {W A} → A × W → Writer W A
writer = ?

tell : ∀ {W} → W → Writer W Unit
tell = ?

listen : ∀ {W A} → Writer W A → Writer W (A × W)
listen = ?

pass : ∀ {W A} → Writer W (A × (W → W)) → Writer W A
pass = ?
