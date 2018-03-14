module Control.Monad.Cont where

open import Prelude
open import Control.Monad

Cont : Set → Set → Set
Cont = ?

runCont : ∀ {R A} → Cont R A → (A → R) → R
runCont = ?

instance
  ContFunctor : ∀ {R} → Functor (Cont R)
  ContFunctor = ?

  ContMonad : ∀ {R} → Monad (Cont R)
  ContMonad = ?

-- https://hackage.haskell.org/package/mtl/docs/Control-Monad-Cont.html#t:MonadCont

callCC : ∀ {R A B} → ((A -> Cont R B) -> Cont R A) -> Cont R A
callCC = ?
