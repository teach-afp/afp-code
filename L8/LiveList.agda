-- AFP 2024-02-08 Live coding result

module LiveList where

open import Agda.Primitive renaming (Set to Type)

data List (A : Type) : Type where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

infixr 6 _∷_
infixl 4 _++_

_++_ : ∀{A : Type} (xs ys : List A) → List A
[]     ++ ys = ys
x ∷ xs ++ ys = x ∷ (xs ++ ys)

[_] : ∀{A} → A → List A
[ a ] = a ∷ []

reverse : ∀{A} → List A → List A
reverse [] = []
reverse (x ∷ xs) = reverse xs ++ [ x ]

infix 2 _≡_

data _≡_ {A : Type} (a : A) : A → Type where
  refl : a ≡ a

cong : {A B : Type} (f : A → B) {a₁ a₂ : A} (p : a₁ ≡ a₂) → f a₁ ≡ f a₂
cong f refl = refl

sym : {A : Type} {a₁ a₂ : A} (p : a₁ ≡ a₂) → a₂ ≡ a₁
sym refl = refl

trans : {A : Type} {a₁ a₂ a₃ : A} (p : a₁ ≡ a₂) (q : a₂ ≡ a₃) → a₁ ≡ a₃
trans refl q = q

-- Monoid laws for (List, [], ++)

[]-++ : ∀{A} (xs : List A) → [] ++ xs ≡ xs
[]-++ xs = refl

++-[] : ∀{A} (xs : List A) → xs ++ [] ≡ xs
++-[] [] = refl
++-[] (x ∷ xs) = cong (x ∷_) (++-[] xs)

++-assoc : ∀{A} (xs {ys zs} : List A) → (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)
++-assoc xs = {!!}

-- Involution laws

reverse-++ : ∀{A} (xs ys : List A) → reverse (xs ++ ys) ≡ reverse ys ++ reverse xs
reverse-++ xs ys = {!!}

reverse-reverse : ∀{A} (xs : List A) → reverse (reverse xs) ≡ xs
reverse-reverse xs = {!!}
