open import Agda.Primitive renaming (Set to Type)

open import Data.List.Base using (List; []; _∷_)
open import Data.Nat.Base using (ℕ; zero; suc; _+_)
open import Data.Bool.Base using (Bool; true; false; if_then_else_)
open import Data.Product using (∃; _×_; _,_)
open import Data.Maybe using (Maybe; nothing; just; _>>=_)
open import Function using (case_of_)
open import Relation.Nullary using (Dec; yes; no)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- Monad Maybe

return : {A : Type} (a : A) → Maybe A
return = just

-- Abstract type of variable names

postulate
  Name   : Type
  eqName : (x y : Name) → Dec (x ≡ y)

-- Expressions: add variables and let-binding

data Exp : Type where
  eBool : Bool → Exp
  eNat  : ℕ → Exp
  ePlus : (e₁ e₂ : Exp) → Exp
  eIf   : (e e₁ e₂ : Exp) → Exp
  eLet  : (x : Name) (e e₁ : Exp) → Exp
  eVar  : (x : Name) → Exp

------------------------------------------------------------------------
-- Typed terms

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

-- Contexts

Cxt = List Ty

variable
  Γ : Cxt

------------------------------------------------------------------------
-- Index into a list

data Any {A : Type} (P : A → Type) : List A → Type where
  here  : ∀{x xs} → P x      → Any P (x ∷ xs)
  there : ∀{x xs} → Any P xs → Any P (x ∷ xs)

-- Membership

_∈_ : {A : Type} → A → List A → Type
x ∈ xs = {!!}

pattern here! = here refl

-- de Bruijn index (pointer into the context)

Var : (Γ : Cxt) (a : Ty) → Type
Var Γ a = {!!}

-- Typed terms (indexed also by context)

data Tm (Γ : Cxt) : Ty → Type where
  tLit  : (v : Val a)                      → Tm Γ a
  tPlus : (t₁ t₂ : Tm Γ nat)               → Tm Γ nat
  tIf   : (t : Tm Γ bool) (t₁ t₂ : Tm Γ a) → Tm Γ a

  tLet  : (t : Tm Γ a) (t₁ : Tm (a ∷ Γ) b) → Tm Γ b
  tVar  : (x : Var Γ a)                    → Tm Γ a

------------------------------------------------------------------------
-- Decorating list elements with additional data

data All {A : Type} (P : A → Type) : List A → Type where
  []  : All P []
  _∷_ : ∀{x xs} → P x → All P xs → All P (x ∷ xs)

lookup : ∀ {A} {P : A → Type} {x xs} → All P xs → x ∈ xs → P x
lookup ps i = {!!}

------------------------------------------------------------------------
-- Tagless interpreter

-- Environment for evaluation

Env : (Γ : Cxt) → Type
Env Γ = {!!}

lookupEnv : Env Γ → Var Γ a → Val a
lookupEnv env x = {!!}

{-
-- Tagless evaluation

eval : Tm Γ a → Env Γ → Val a
eval (tLit v)      env = v
eval (tPlus t₁ t₂) env = eval t₁ env + eval t₂ env
eval (tIf t t₁ t₂) env = if eval t env then eval t₁ env else eval t₂ env
eval (tVar x)      env = lookupEnv env x
eval (tLet t t₁)   env = eval t₁ (eval t env ∷ env)

------------------------------------------------------------------------
-- Type inference

-- Decide equality of types

_≟_ : (a b : Ty) → Dec (a ≡ b)
bool ≟ bool = yes refl
nat  ≟ nat  = yes refl
bool ≟ nat  = no λ()
nat  ≟ bool = no λ()

-- Type checking environment

TC : (Γ : Cxt) → Type
TC Γ = {!!}

{-
lookupTC : TC Γ → Name → Maybe (∃ λ (a : Ty) → Var Γ a)
lookupTC [] x = nothing
lookupTC (y ∷ cxt) x =
  case eqName x y of λ where
    (yes _) → just (_ , here!)
    (no  _) → do
      a , i ← lookupTC cxt x
      return (a , there i)

-- Type checking

mutual
  infer : (e : Exp) → TC Γ → Maybe (∃ λ (a : Ty) → Tm Γ a)
  infer (eBool b)     cxt = return (bool , tLit b)
  infer (eNat n)      cxt = return (nat  , tLit n)

  infer (ePlus e₁ e₂) cxt = do
    t₁ ← check e₁ nat cxt
    t₂ ← check e₂ nat cxt
    return (nat , tPlus t₁ t₂)

  infer (eIf e e₁ e₂) cxt = do
    t ← check e bool cxt
    (a , t₁) ← infer e₁ cxt
    t₂ ← check e₂ a cxt
    return (a , tIf t t₁ t₂)

  infer (eLet x e e₁) cxt = do
    (a , t ) ← infer e cxt
    (b , t₁) ← infer e₁ (x ∷ cxt)
    return (b , tLet t t₁)

  infer (eVar x) cxt = do
    (a , i)  ← lookupTC cxt x
    return (a , tVar i)

  check : (e : Exp) (a : Ty) (cxt : TC Γ) → Maybe (Tm Γ a)
  check e a cxt =
    case infer e cxt of λ where
      nothing → nothing
      (just (b , t)) → cast t a

  cast : (t : Tm Γ b) (a : Ty) → Maybe (Tm Γ a)
  cast {b = b} t a with b ≟ a
  ... | yes refl = just t
  ... | no  _    = nothing



-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
