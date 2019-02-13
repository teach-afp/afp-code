module Data.Functor.Identity where

open import Prelude
open import Control.Monad

Identity : Set → Set
Identity = ?

instance
  IdentityFunctor : Functor Identity
  IdentityFunctor = ?

  IdentityMonad : Monad Identity
  IdentityMonad = ?
