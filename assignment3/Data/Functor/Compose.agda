module Data.Functor.Compose where

open import Prelude
open import Control.Monad

ComposeFunctor : ∀ {F G} {{_ : Functor F}} {{_ : Functor G}} → Functor (F ∘ G)
ComposeFunctor = ?
