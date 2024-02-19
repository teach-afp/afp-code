open import Agda.Primitive renaming (Set to Type)

open import Data.Nat.Base using (ℕ; zero; suc; _+_)
open import Data.Bool.Base using (Bool; true; false; if_then_else_)
open import Data.Maybe using (Maybe; nothing; just)

data Exp : Type where
  eBool : Bool → Exp
  eNat  : ℕ → Exp
  ePlus : (e₁ e₂ : Exp) → Exp
  eIf   : (e e₁ e₂ : Exp) → Exp

data Val : Type where
  vBool : Bool → Val
  vNat  : ℕ    → Val

mutual

  eval : Exp → Maybe Val
  eval (eBool b)     = just (vBool b)
  eval (eNat  n)     = just (vNat  n)
  eval (ePlus e₁ e₂) = eval-plus (eval e₁) (eval e₂)
  eval (eIf e e₁ e₂) = eval-if   (eval e) e₁ e₂

  eval-plus : Maybe Val → Maybe Val → Maybe Val
  eval-plus (just (vNat n₁)) (just (vNat n₂)) = just (vNat (n₁ + n₂))
  eval-plus _ _ = nothing

  eval-if : Maybe Val → Exp → Exp → Maybe Val
  eval-if (just (vBool b)) e₁ e₂ = if b then eval e₁ else eval e₂
  eval-if _ _ _ = nothing

-- -}
-- -}
-- -}
-- -}
