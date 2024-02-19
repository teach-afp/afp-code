open import Agda.Primitive renaming (Set to Type)

open import Data.Nat.Base using (ℕ; zero; suc; _+_)
open import Data.Bool.Base using (Bool; true; false; if_then_else_)
open import Data.Product using (∃; _,_)
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

-- Maybe monad

return : {A : Type} (a : A) → Maybe A
return = just

-- _>>=_ : {A B : Type} (m : Maybe A) (k : A → Maybe B) → Maybe B
-- just a  >>= k = k a
-- nothing >>= k = nothing

-- Using do notation

module _ where
  open import Data.Maybe using (_>>=_)

  eval : Exp → Maybe Val
  eval (eBool b) = return (vBool b)
  eval (eNat  n) = return (vNat  n)

  -- simple do
  eval (eIf e e₁ e₂) = do
    v ← eval e
    case v of λ where
      (vBool b) → if b then eval e₁ else eval e₂
      _ → nothing

  -- do with matching and alternative branches
  eval (ePlus e₁ e₂) = do
    vNat n₁ ← eval e₁
      where
        (vBool _) → nothing
    vNat n₂ ← eval e₂ where _ → nothing
    return (vNat (n₁ + n₂))

foo = {! eval !}
{-
  λ{ (vNat n₂) →
   ; _ →

-- -}
-- -}
-- -}
-- -}
