module Control.Monad.Trans.State where

open import Prelude
open import Control.Monad.Trans.Class

StateT : (S : Set) → (M : Set → Set) → (A : Set) → Set
StateT = ?

runStateT : ∀ {S M A} → StateT S M A → S → M (A × S)
runStateT = ?

instance
  StateTMonadTrans : ∀ {W} → MonadTrans (StateT W)
  StateTMonadTrans = ?
