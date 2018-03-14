module Control.Monad where

open import Prelude
open import Data.Monoid

-- https://hackage.haskell.org/package/base/docs/Control-Monad.html#t:Functor
record Functor (F : Set → Set) : Set₁ where

open Functor {{...}} public

-- https://hackage.haskell.org/package/base/docs/Control-Monad.html#t:Monad
record Monad (M : Set → Set) : Set₁ where
  infixl 3 _>>=_

  field
    {{functorM}}             : Functor M
    return                   : ∀ {A} → A → M A
    _>>=_                    : ∀ {A B} → M A → (A → M B) → M B
    monad-left-identity-law  : ?
    monad-right-identity-law : ?
    monad-associativity-law  : ?

open Monad {{...}} public

_>>_ : ∀ {M} {{_ : Monad M}} {A B} → M A → M B → M B
ma >> mb = ma >>= λ _ → mb

instance
  ListFunctor : Functor List
  ListFunctor = ?

  ListMonad : Monad List
  ListMonad = ?

  MaybeFunctor : Functor Maybe
  MaybeFunctor = ?

  MaybeMonad : Monad Maybe
  MaybeMonad = ?

fail : ∀ {A} → Maybe A
fail = ?
