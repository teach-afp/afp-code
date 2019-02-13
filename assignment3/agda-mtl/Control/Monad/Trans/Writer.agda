module Control.Monad.Trans.Writer where

open import Prelude
open import Control.Monad.Trans.Class
open import Data.Monoid

WriterT : (W : Set) → (M : Set → Set) → (A : Set) → Set
WriterT = ?

runWriterT : ∀ {W M A} → WriterT W M A → M (A × W)
runWriterT = ?

instance
  WriterTMonadTrans : ∀ {W} {{_ : Monoid W}} → MonadTrans (WriterT W)
  WriterTMonadTrans = ?
