module Control.Monad.Trans.Maybe where

open import Prelude
open import Control.Monad.Trans.Class

MaybeT : (M : Set → Set) → (A : Set) → Set
MaybeT = ?

runMaybeT : ∀ {M A} → MaybeT M A → M (Maybe A)
runMaybeT = ?

instance
  MaybeTMonadTrans : MonadTrans MaybeT
  MaybeTMonadTrans = ?
