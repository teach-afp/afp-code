module Control.Monad.Trans.List where

open import Prelude
open import Control.Monad.Trans.Class

ListT : (M : Set → Set) → (A : Set) → Set
ListT = ?

runListT : ∀ {M A} → ListT M A → M (List A)
runListT = ?

instance
  ListTMonadTrans : MonadTrans ListT
  ListTMonadTrans = ?
