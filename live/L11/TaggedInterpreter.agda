open import Agda.Primitive renaming (Set to Type)

open import Data.Nat.Base using (ℕ; zero; suc; _+_)
open import Data.Bool.Base using (Bool; true; false; if_then_else_)
open import Data.Product using (_,_)
open import Data.Maybe using (Maybe; nothing; just)
open import Function using (case_of_)

data Exp : Type where
  eBool : Bool → Exp
  eNat  : ℕ → Exp
  ePlus : (e₁ e₂ : Exp) → Exp
  eIf   : (e e₁ e₂ : Exp) → Exp

data Val : Type where
  vBool : Bool → Val
  vNat  : ℕ → Val

eval : Exp → Maybe Val
eval (eBool b) = just (vBool b)
eval (eNat  n) = just (vNat  n)

eval (eIf e e₁ e₂) = {!
  (λ{ (just (vBool b)) → if b then eval e₁ else eval e₂
    ; _ → nothing
    })
  (eval e)
!}

eval (ePlus e₁ e₂) = {!
   (λ{ (just (vNat n₁) , just (vNat n₂)) → just (vNat (n₁ + n₂))
     ; _ → nothing
     }) (eval e₁ , eval e₂)
!}


-- -}
-- -}
-- -}
-- -}
