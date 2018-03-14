module Control.Monad.Except where

open import Prelude
open import Control.Monad

instance
  ExceptFunctor : ∀ {E} → Functor (Either E)
  ExceptFunctor = ?

  ExceptMonad : ∀ {E} → Monad (Either E)
  ExceptMonad = ?

-- https://hackage.haskell.org/package/mtl/docs/Control-Monad-Except.html#t:MonadError

throwError : ∀ {E A} → E → Either E A
throwError = ?

catchError : ∀ {E A} → Either E A → (E → Either E A) → Either E A
catchError = ?
