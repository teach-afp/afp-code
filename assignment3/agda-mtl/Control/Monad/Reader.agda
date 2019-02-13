module Control.Monad.Reader where

open import Prelude
open import Control.Monad

Reader : ∀ (R : Set) → Set → Set
Reader = ?

runReader : ∀ {R A} → Reader R A → R → A
runReader = ?

instance
  ReaderFunctor : ∀ {R} → Functor (Reader R)
  ReaderFunctor = ?

  ReaderMonad : ∀ {R} → Monad (Reader R)
  ReaderMonad = ?

-- https://hackage.haskell.org/package/mtl/docs/Control-Monad-Reader-Class.html#t:MonadReader

ask : ∀ {R} → Reader R R
ask = ?

reader : ∀ {R A} → (R → A) → Reader R A
reader = ?

local : ∀ {R A} → (R → R) → Reader R A → Reader R A
local = ?
