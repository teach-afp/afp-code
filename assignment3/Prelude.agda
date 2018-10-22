module Prelude where

open import Agda.Primitive        using (Level; _⊔_)         renaming (lzero to zero; lsuc to suc) public
open import Agda.Builtin.Bool     using (Bool; true; false)                                        public
import Agda.Builtin.Equality
open import Agda.Builtin.Equality using (refl)                                                     public
import Agda.Builtin.List
open import Agda.Builtin.List     using ([])                 renaming (_∷_ to _::_)                public
open import Agda.Builtin.Nat      using (Nat; _+_; _-_; _*_)                                       public
open import Agda.Builtin.Unit                                renaming (⊤ to Unit; tt to ⟨⟩)        public

-- Function
infix 0 case_return_of_ case_of_
case_return_of_ : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} → (x : A) → (B : A → Set ℓ₂) → ((x : A) → B x) → B x
case x return B of f = f x

case_of_ : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} → A → (A → B) → B
case x of f = case x return _ of f

infixr 0 _$_
_$_ : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : A → Set ℓ₂} → ((x : A) → B x) → (x : A) → B x
f $ x = f x

id : ∀ {ℓ} {A : Set ℓ} → A → A
id a = a

infixr 9 _∘_
_∘_ : ∀ {ℓ} {A B C : Set ℓ} → (B → C) → (A → B) → A → C
f ∘ g = λ a → f (g a)

const : ∀ {ℓ} {A B : Set ℓ} → A → B → A
const a = λ _ → a
-- /Function

-- Data.Product
infixr 2 _×_
infixr 4 _,_
record _×_ {ℓ₁ ℓ₂} (A : Set ℓ₁) (B : Set ℓ₂) : Set (ℓ₁ ⊔ ℓ₂) where
  constructor _,_
  field
    fst : A
    snd : B

open _×_ public
-- /Data.Product

-- Data.List
List : Set → Set
List = Agda.Builtin.List.List

[_] : ∀ {A : Set} → A → List A
[ x ] = x :: []

infixr 5 _++_
_++_ : ∀ {A : Set} → List A → List A → List A
[]        ++ ys = ys
(x :: xs) ++ ys = x :: (xs ++ ys)

map : ∀ {A B} → (A → B) → List A → List B
map f []        = []
map f (x :: xs) = f x :: map f xs

foldr : ∀ {A B : Set} → (A → B → B) → B → List A → B
foldr c n []        = n
foldr c n (x :: xs) = c x (foldr c n xs)

foldl : ∀ {A B : Set} → (A → B → A) → A → List B → A
foldl c n []        = n
foldl c n (x :: xs) = foldl c (c n x) xs

concat : ∀ {A} → List (List A) → List A
concat = foldr _++_ []

concatMap : ∀ {A B} → (A → List B) → List A → List B
concatMap f = concat ∘ map f
-- /Data.List

-- Data.Maybe
data Maybe (A : Set) : Set where
  Just    : (x : A) → Maybe A
  Nothing : Maybe A

maybe : ∀ {A B : Set} → B → (A → B) → Maybe A → B
maybe _ f (Just x) = f x
maybe b _ Nothing  = b

maybe´ : ∀ {A B} → (A → B) → Maybe A → Maybe B
maybe´ f = maybe Nothing (Just ∘ f)
-- /Data.Maybe

-- Data.Sum
data Either (A : Set) (B : Set) : Set where
  Left  : (x : A) → Either A B
  Right : (y : B) → Either A B

either : ∀ {A B C : Set} → (A → C) → (B → C) → Either A B → C
either f _ (Left x)  = f x
either _ g (Right y) = g y

either´ : ∀ {A B C D : Set} → (A → C) → (B → D) → Either A B → Either C D
either´ f g = either (Left ∘ f) (Right ∘ g)
-- /Data.Sum

-- Relation.Binary.PropositionalEquality
infix 2 _≡_
_≡_ : ∀ {ℓ} {A : Set ℓ} → A → A → Set ℓ
_≡_ = Agda.Builtin.Equality._≡_

sym : ∀ {ℓ} {A : Set ℓ} {x y : A} → x ≡ y → y ≡ x
sym refl = refl

trans : ∀ {ℓ} {A : Set ℓ} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl refl = refl

cong : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} (f : A → B) {x y} → x ≡ y → f x ≡ f y
cong f refl = refl

cong-app : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : A → Set ℓ₂} {f g : (x : A) → B x} → f ≡ g → (x : A) → f x ≡ g x
cong-app refl x = refl
-- /Relation.Binary.PropositionalEquality

infixr 1 _∧_

_∧_ = _×_

infixr 1 _∨_

_∨_ = Either

record ∃ {ℓ₁ ℓ₂} {A : Set ℓ₁} (P : A → Set ℓ₂) : Set (ℓ₁ ⊔ ℓ₂) where
  constructor _,_
  field
    witness : A
    proof   : P witness

open ∃ public

syntax ∃ (λ x → B) = ∃[ x ] B

infix 3 _↔_
record _↔_ {ℓ₁ ℓ₂} (A : Set ℓ₁) (B : Set ℓ₂) : Set (ℓ₁ ⊔ ℓ₂) where
  constructor _,_
  field
    ltr : A → B
    rtl : B → A

open _↔_ public

module ≡-Reasoning {ℓ} {A : Set ℓ} where
  infix  3 _∎
  infixr 2 _≡⟨⟩_ _≡⟨_⟩_
  infix  1 begin_

  begin_ : ∀ {x y : A} → x ≡ y → x ≡ y
  begin_ x≡y = x≡y

  _≡⟨⟩_ : ∀ (x {y} : A) → x ≡ y → x ≡ y
  _ ≡⟨⟩ x≡y = x≡y

  _≡⟨_⟩_ : ∀ (x {y z} : A) → x ≡ y → y ≡ z → x ≡ z
  _ ≡⟨ x≡y ⟩ y≡z = trans x≡y y≡z

  _∎ : ∀ (x : A) → x ≡ x
  _∎ _ = refl

open ≡-Reasoning public

postulate funExt : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : A → Set ℓ₂} {f g : (x : A) → B x} → (∀ x → f x ≡ g x) → f ≡ g

funExt2 : ∀ {ℓ₁ ℓ₂ ℓ₃} {A : Set ℓ₁} {B : A → Set ℓ₂} {C : (x : A) → B x → Set ℓ₃} {f g : (x : A) → (y : B x) → C x y} → (∀ x y → f x y ≡ g x y) → f ≡ g
funExt2 fxy≡gxy = funExt (λ x → funExt (λ y → fxy≡gxy x y))
