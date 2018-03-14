module Control.Monad.Trans.Class where

open import Prelude
open import Control.Monad

-- https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Class.html#t:MonadTrans
record MonadTrans (T : (Set → Set) → Set → Set) : Set₁ where

open MonadTrans {{...}} public
