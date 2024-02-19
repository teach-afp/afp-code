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

-- Tagged values

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

-- Type checking monad: Maybe

return : {A : Type} (a : A) → Maybe A
return = just

_>>=_ : {A B : Type} (m : Maybe A) (k : A → Maybe B) → Maybe B
just a  >>= k = k a
nothing >>= k = nothing

-- Type checking

mutual
  infer : (e : Exp) → Maybe (∃ λ (a : Ty) → Tm a)
  infer (eBool b)     = return (bool , tLit b)
  infer (eNat n)      = return (nat  , tLit n)
  infer (ePlus e₁ e₂) = do
    t₁ ← check e₁ nat
    t₂ ← check e₂ nat
    return (nat , tPlus t₁ t₂)
  infer (eIf e e₁ e₂) = do
    t ← check e bool
    (a , t₁) ← infer e₁
    t₂ ← check e₂ a
    return (a , tIf t t₁ t₂)

  check : (e : Exp) (a : Ty) → Maybe (Tm a)
  check e a =
    case infer e of λ where
      nothing → nothing
      (just (b , t)) → cast t a

  cast : (t : Tm b) (a : Ty) → Maybe (Tm a)
  cast {b} t a with b ≟ a
  ... | yes refl = just t
  ... | no  _    = nothing

-- Uniqueness and completeness of type checking

exp : Tm a → Exp
exp (tPlus t₁ t₂)       = ePlus (exp t₁) (exp t₂)
exp (tIf t t₁ t₂)       = eIf (exp t) (exp t₁) (exp t₂)
-- Need to be after tIf:
exp (tLit {a = bool} v) = eBool v
exp (tLit {a = nat } v) = eNat  v

mutual
  uniq-infer : (t : Tm a) → infer (exp t) ≡ just (a , t)
  uniq-infer {a = bool} (tLit {a = .bool} v) = refl
  uniq-infer {a = nat}  (tLit {a = .nat} v) = refl
  uniq-infer (tPlus t₁ t₂)
    rewrite uniq-check t₁
          | uniq-check t₂ = refl
  uniq-infer {a = a} (tIf t t₁ t₂)
    rewrite uniq-check t
          | uniq-infer t₁
          | uniq-check t₂ = refl

  uniq-check : (t : Tm a) → check (exp t) a ≡ just t
  uniq-check t rewrite uniq-infer t | uniq-cast t = refl

  uniq-cast : (t : Tm a) → cast t a ≡ just t
  uniq-cast {a = a} t with a ≟ a
  ... | yes refl = refl
  ... | no  p    = case p refl of λ()

-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
