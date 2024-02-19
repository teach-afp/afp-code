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
  -- new:
  eLet  : (x : Name) (e e₁ : Exp) → Exp
  eVar  : (x : Name) → Exp

------------------------------------------------------------------------
-- Tagged interpreter

-- Environments mapping names to something

NEnv : (A : Type) → Type
NEnv A = List (Name × A)

lookupNEnv : {A : Type} → NEnv A → Name → Maybe A
lookupNEnv [] x = nothing
lookupNEnv ((y , a) ∷ as) x =
  case eqName x y of λ where
    (yes _) → just a
    (no  _) → lookupNEnv as x

-- Tagged values

data V : Type where
  vBool : (b : Bool) → V
  vNat  : (n : ℕ)    → V

-- Tagged interpreter

interp : Exp → (env : NEnv V) → Maybe V
interp (eBool b) env = just (vBool b)
interp (eNat  n) env = just (vNat  n)

interp (ePlus e₁ e₂) env = do
  vNat n₁ ← interp e₁ env where
    _ → nothing
  vNat n₂ ← interp e₂ env where
    _ → nothing
  return (vNat (n₁ + n₂))

interp (eIf e e₁ e₂) env = do
  vBool b ← interp e env where
    _ → nothing
  if b then interp e₁ env else interp e₂ env

interp (eVar x) env = lookupNEnv env x
interp (eLet x e e₁) env = do
  v ← interp e env
  interp e₁ ((x , v) ∷ env)

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

-- de Bruijn index (pointer into the context)

data Var : (Γ : Cxt) (a : Ty) → Type where
  here  : Var (a ∷ Γ) a
  there : (x : Var Γ a) → Var (b ∷ Γ) a

-- Typed terms (indexed also by context)

data Tm (Γ : Cxt) : Ty → Type where
  tLit  : (v : Val a)                      → Tm Γ a
  tPlus : (t₁ t₂ : Tm Γ nat)               → Tm Γ nat
  tIf   : (t : Tm Γ bool) (t₁ t₂ : Tm Γ a) → Tm Γ a

  tLet  : (t : Tm Γ a) (t₁ : Tm (a ∷ Γ) b) → Tm Γ b
  tVar  : (x : Var Γ a)                    → Tm Γ a

------------------------------------------------------------------------
-- Tagless interpreter

-- Environment for evaluation

data Env : (Γ : Cxt) → Type where
  []  : Env []
  _∷_ : (v : Val a) (env : Env Γ) → Env (a ∷ Γ)

lookupEnv : Env Γ → Var Γ a → Val a
lookupEnv vs i = {!!}

-- Tagless evaluation

eval : Tm Γ a → Env Γ → Val a
eval (tLit v)      env = v
eval (tPlus t₁ t₂) env = eval t₁ env + eval t₂ env
eval (tIf t t₁ t₂) env = if eval t env then eval t₁ env else eval t₂ env
eval (tVar x)      env = {!!}
eval (tLet t t₁)   env = {!!}

------------------------------------------------------------------------
-- Type inference

-- Decide equality of types

_≟_ : (a b : Ty) → Dec (a ≡ b)
bool ≟ bool = yes refl
nat  ≟ nat  = yes refl
bool ≟ nat  = no λ()
nat  ≟ bool = no λ()

-- Type checking environment

data TC : (Γ : Cxt) → Type where
  []  : TC []
  _∷_ : (x : Name) (cxt : TC Γ) → TC (a ∷ Γ)

lookupTC : TC Γ → Name → Maybe (∃ λ (a : Ty) → Var Γ a)
lookupTC [] x = nothing
lookupTC (y ∷ cxt) x =
  case eqName x y of λ where
    (yes _) → {!!}
    (no  _) → {!!}

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

  infer (eLet x e e₁) cxt = {!!}

  infer (eVar x) cxt = {!!}

  check : (e : Exp) (a : Ty) (cxt : TC Γ) → Maybe (Tm Γ a)
  check e a cxt = do
    (b , t) ← infer e cxt
    cast t a

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
