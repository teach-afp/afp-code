module Control.Monad.Trans.Identity where

open import Prelude
open import Control.Monad.Trans.Class

IdentityT : (M : Set → Set) → (A : Set) → Set
IdentityT = ?

runIdentityT : ∀ {M A} → IdentityT M A → M A
runIdentityT = ?

instance
  IdentityTMonadTrans : MonadTrans IdentityT
  IdentityTMonadTrans = ?
