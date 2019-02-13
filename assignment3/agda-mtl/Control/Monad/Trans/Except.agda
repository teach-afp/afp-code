module Control.Monad.Trans.Except where

open import Prelude
open import Control.Monad.Trans.Class

ExceptT : (E : Set) → (M : Set → Set) → (A : Set) → Set
ExceptT = ?

runExceptT : ∀ {E M A} → ExceptT E M A → M (Either E A)
runExceptT = ?

instance
  ExceptTMonadTrans : ∀ {E} → MonadTrans (ExceptT E)
  ExceptTMonadTrans = ?
