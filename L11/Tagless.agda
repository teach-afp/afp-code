open import Agda.Primitive renaming (Set to Type)

open import Data.Nat.Base using (ℕ; zero; suc; _+_)
open import Data.Bool.Base using (Bool; true; false; if_then_else_)
open import Data.Product using (∃; _,_)
open import Data.Maybe using (Maybe; nothing; just)
open import Function using (case_of_)
open import Relation.Nullary using (Dec; yes; no)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

data Exp : Type where
  eBool : Bool → Exp
  eNat  : ℕ → Exp
  ePlus : (e₁ e₂ : Exp) → Exp
  eIf   : (e e₁ e₂ : Exp) → Exp

-- Types

data Ty : Type where
  bool : Ty
  nat  : Ty

variable
  a b : Ty

-- Tagless values

Val : Ty → Type
Val bool = Bool
Val nat  = ℕ

-- Typed terms

data Tm : Ty → Type where
  tLit  : (v : Val a) → Tm a
  tPlus : (t₁ t₂ : Tm nat) → Tm nat
  tIf   : (t : Tm bool) (t₁ t₂ : Tm a) → Tm a

-- Tagless evaluation

eval : Tm a → Val a
eval (tLit v) = v
eval (tPlus t₁ t₂) = eval t₁ + eval t₂
eval (tIf t t₁ t₂) = if eval t then eval t₁ else eval t₂

-- Decide equality of types

_≟_ : (a b : Ty) → Dec (a ≡ b)
bool ≟ bool = yes refl
nat  ≟ nat  = yes refl
bool ≟ nat  = no λ()
nat  ≟ bool = no λ()

-- Type checking

mutual
  infer : (e : Exp) → Maybe (∃ λ (a : Ty) → Tm a)
  infer (eBool b)     = just (bool , tLit b)
  infer (eNat n)      = just (nat  , tLit n)
  infer (ePlus e₁ e₂) =
    case (check e₁ nat , check e₂ nat) of λ where
      (just t₁ , just t₂) → just (nat , tPlus t₁ t₂)
      _ → nothing
  infer (eIf e e₁ e₂) =
    case (check e bool , infer e₁) of λ where
      (just t , just (a , t₁)) →
        case check e₂ a of λ where
          (just t₂) → just (_ , tIf t t₁ t₂)
          _ → nothing
      _ → nothing

  check : (e : Exp) (a : Ty) → Maybe (Tm a)
  check e a =
    case infer e of λ where
      nothing → nothing
      (just (b , t)) → cast t a

  cast : (t : Tm b) (a : Ty) → Maybe (Tm a)
  cast {b} t a = cast' t (b ≟ a)

  cast' : (t : Tm b) (d : Dec (b ≡ a)) → Maybe (Tm a)
  cast' t (yes refl) = just t
  cast' t (no _    ) = nothing

{-

  cast : (t : Tm b) (a : Ty) → Maybe (Tm a)
  cast {b} t a with b ≟ a
  ... | yes refl = just t
  ... | no  _    = nothing


-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
