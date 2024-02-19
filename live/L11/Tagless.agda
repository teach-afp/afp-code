open import Agda.Primitive renaming (Set to Type)

-- open import Data.Empty using (⊥)
open import Data.Nat.Base using (ℕ; zero; suc; _+_)
open import Data.Bool.Base using (Bool; true; false; if_then_else_)
open import Data.Product using (∃; _,_)
open import Data.Maybe using (Maybe; nothing; just; _>>=_)
open import Function using (case_of_)
-- open import Relation.Nullary using (Dec; yes; no; ¬_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- Monad Maybe

return : {A : Type} (a : A) → Maybe A
return = just

-- Expressions
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
eval (tLit v)       = v
eval (tPlus t t₁)   = eval t + eval t₁
eval (tIf t t₁ t₂) = if eval t then eval t₁ else eval t₂

-- Decide equality of types

data ⊥ : Type where

¬_ : Type → Type
¬ A = A → ⊥

data Dec (P : Type) : Type where
  yes : (p  : P  ) → Dec P
  no  : (¬p : ¬ P) → Dec P

_≟_ : (a b : Ty) → Dec (a ≡ b)
bool ≟ bool = yes refl
bool ≟ nat = no (λ{ () })
nat ≟ bool = no λ()
nat ≟ nat = yes refl

-- Type checking

mutual
  infer : (e : Exp) → Maybe (∃ λ (a : Ty) → Tm a)
  infer (eBool b)     = return (bool , tLit b)
  infer (eNat n)      = return  (nat , tLit n)
  infer (ePlus e e₁)  = do
    t  ← check e nat
    t₁ ← check e₁ nat
    return (nat , tPlus t t₁)
  infer (eIf e e₁ e₂) = do
    t      ← check e bool
    a , t₁ ← infer e₁
    t₂     ← check e₂ a
    return (a , tIf t t₁ t₂)

  check : (e : Exp) (a : Ty) → Maybe (Tm a)
  check e a = do
    b , t ← infer e
    cast t a

  cast : (t : Tm b) (a : Ty) → Maybe (Tm a)
  cast {b} t a = cast' t (b ≟ a)

  cast' : (t : Tm b) (d : Dec (b ≡ a)) → Maybe (Tm a)
  cast' t (yes refl) = return t
  cast' t (no ¬p)    = nothing


-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
