module Control.Monad.Trans.Reader where

open import Prelude
open import Control.Monad.Trans.Class

ReaderT : (R : Set) → (M : Set → Set) → (A : Set) → Set
ReaderT = ?

runReaderT : ∀ {R M A} → ReaderT R M A → R → M A
runReaderT = ?

instance
  ReaderTMonadTrans : ∀ {R} → MonadTrans (ReaderT R)
  ReaderTMonadTrans = ?
