-- AFP 2025 Proving that lists form an involutive monoid under append and reverse

module List2025 where

open import Agda.Primitive renaming (Set to Type)

data List (A : Type) : Type where
    []  : List A
    _∷_ : (x : A) (xs : List A) → List A   -- A → List A → List A

infixr 6 _∷_
infixl 4 _++_

_++_ : ∀ {A : Type} (xs ys : List A) → List A
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

{-# BUILTIN EQUALITY _≡_ #-}

reverse-[] : {A : Type} → reverse {A} [] ≡ []
reverse-[] = refl

eta : {A B : Type} {f : A → B} → f ≡ (λ x → f x)
eta = refl

++-[] : {A : Type} (xs : List A) → xs ++ [] ≡ xs
++-[] [] = refl
++-[] (x ∷ xs) rewrite ++-[] xs = refl

cong : {A B : Type} (f : A → B) {a₁ a₂ : A} (p : a₁ ≡ a₂) → f a₁ ≡ f a₂
cong f {a₁} {.a₁} refl = refl

anti-cong : {A B : Type} (f : A → B) (g : B → A) {a₁ a₂ : A} (p : a₁ ≡ g (f a₂)) → a₁ ≡ g (f a₂)
anti-cong f g refl = {! !}

sym : {A : Type} {a₁ a₂ : A} (p : a₁ ≡ a₂) → a₂ ≡ a₁
sym refl = refl

trans : {A : Type} {a₁ a₂ a₃ : A} (p : a₁ ≡ a₂) (q : a₂ ≡ a₃) → a₁ ≡ a₃
trans p q = {!   !}

{-
-- Monoid laws for (List, [], ++)

++-[] : ∀{A} (xs : List A) → xs ++ [] ≡ xs
++-[] xs = ?

++-assoc : ∀{A} (xs {ys zs} : List A) → (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)
++-assoc xs = ?

-- Involution laws

reverse-++ : ∀{A} (xs ys : List A) → reverse (xs ++ ys) ≡ reverse ys ++ reverse xs
reverse-++ xs ys = ?
--   where
--     step₁ : reverse (xs ++ ys) ++ [ x ] ≡ (reverse ys ++ reverse xs) ++ [ x ]
--     step₁ = ?

--     step₂ : (reverse ys ++ reverse xs) ++ [ x ] ≡ reverse ys ++ (reverse xs ++ [ x ])
--     step₂ = ?

reverse-reverse : ∀{A} (xs : List A) → reverse (reverse xs) ≡ xs
reverse-reverse xs = ?
--   where
--     step₁ : reverse (reverse xs ++ [ x ]) ≡ reverse [ x ] ++ reverse (reverse xs)
--     step₁ = ?

--     step₂ : reverse [ x ] ++ reverse (reverse xs) ≡ reverse [ x ] ++ xs
--     step₂ = ?

-- -}
-- -}
-- -}
-- -}
