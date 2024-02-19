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

_>>=_ : {A B : Type} (m : Maybe A) (k : A → Maybe B) → Maybe B
just a  >>= k = k a
nothing >>= k = nothing

-- Using do notation

eval : Exp → Maybe Val
eval (eBool b) = just (vBool b)
eval (eNat  n) = just (vNat  n)

-- simple do
eval (eIf e e₁ e₂) = ?

-- do with matching and alternative branches
eval (ePlus e₁ e₂) = ?


-- -}
-- -}
-- -}
-- -}
