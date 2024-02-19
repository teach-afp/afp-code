open import Agda.Primitive renaming (Set to Type)

open import Data.Nat.Base using (ℕ; zero; suc; _+_)
open import Data.Bool.Base using (Bool; true; false; if_then_else_)
open import Data.Product using (_,_)
open import Data.Maybe using (Maybe; nothing; just)

data Exp : Type where
  eBool : Bool → Exp
  eNat  : ℕ → Exp
  ePlus : (e₁ e₂ : Exp) → Exp
  eIf   : (e e₁ e₂ : Exp) → Exp

data Val : Type where
  vBool : Bool → Val
  vNat  : ℕ    → Val

apply : {A B : Type} → (A → B) → A → B
apply f a = f a

case_of_ : {A B : Type} → A → (A → B) → B
case a of f = f a

mutual

  eval : Exp → Maybe Val
  eval (eBool b)     = just (vBool b)
  eval (eNat  n)     = just (vNat  n)

  eval (ePlus e₁ e₂) =
    case (eval e₁ , eval e₂) of λ where
      (just (vNat n₁) , just (vNat n₂)) → just (vNat (n₁ + n₂))
      _ → nothing


  eval (eIf e e₁ e₂) = apply
    (λ { (just (vBool b)) → if b then eval e₁ else eval e₂
       ; _ → nothing
       })
    (eval e)

-- -}
-- -}
-- -}
-- -}
