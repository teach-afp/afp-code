module Control.Monad.State where

open import Prelude
open import Control.Monad

State : ∀ (S : Set) → Set → Set
State = ?

runState : ∀ {S A} → State S A → S → A × S
runState = ?

execState : ∀ {S A} → State S A → S → S
execState ma s = snd (runState ma s)

evalState : ∀ {S A} → State S A → S → A
evalState ma s = fst (runState ma s)

instance
  StateFunctor : ∀ {S} → Functor (State S)
  StateFunctor = ?

  StateMonad : ∀ {S} → Monad (State S)
  StateMonad = ?

-- https://hackage.haskell.org/package/mtl/docs/Control-Monad-State-Class.html#t:MonadState

get : ∀ {S} → State S S
get = ?

put : ∀ {S} → S → State S Unit
put = ?

state : ∀ {S A} → (S → A × S) → State S A
state = ?

gets : ∀ {S A} → (S → A) → State S A
gets f = get >>= return ∘ f

modify : ∀ {S} → (S → S) → State S Unit
modify f = state (λ s → ⟨⟩ , f s)
