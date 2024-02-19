-- Hutton's razor
--
-- Removes all inessential and all essential aspects of the problem.

open import Agda.Primitive renaming (Set to Type)
open import Data.Nat.Base using (ℕ; zero; suc; _+_)

data Exp : Type where
  eNat  : ℕ → Exp
  ePlus : (e₁ e₂ : Exp) → Exp

Val = ℕ

eval : Exp → Val
eval (eNat  n)     = n
eval (ePlus e₁ e₂) = eval e₁ + eval e₂
