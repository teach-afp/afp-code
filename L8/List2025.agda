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


cong : {A B : Type} (f : A → B) {a₁ a₂ : A} (p : a₁ ≡ a₂) → f a₁ ≡ f a₂
cong f {a₁} {.a₁} refl = refl

-- anti-cong : {A B : Type} (f : A → B) (g : B → A) {a₁ a₂ : A} (p : a₁ ≡ g (f a₂)) → a₁ ≡ g (f a₂)
-- anti-cong f g refl = {! !}

sym : {A : Type} {a₁ a₂ : A} (p : a₁ ≡ a₂) → a₂ ≡ a₁
sym refl = refl

trans : {A : Type} {a₁ a₂ a₃ : A} (p : a₁ ≡ a₂) (q : a₂ ≡ a₃) → a₁ ≡ a₃
trans refl q = q

-- Monoid laws for (List, [], ++)

[]-++ : {A : Type} (xs : List A) → [] ++ xs ≡ xs
[]-++ xs = refl

-- {-# NON_TERMINATING #-}
++-[] : {A : Type} (xs : List A) → xs ++ [] ≡ xs
++-[] [] = refl
++-[] (x ∷ xs) -- = ++-[] (x ∷ xs)
  rewrite ++-[] xs = refl


++-assoc : ∀{A} (xs {ys zs} : List A) → (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)
++-assoc [] = refl
++-assoc (x ∷ xs) = cong (x ∷_) (++-assoc xs)


-- Involution laws

reverse-++ : ∀{A} (xs ys : List A) → reverse (xs ++ ys) ≡ reverse ys ++ reverse xs
reverse-++ []       ys = sym (++-[] (reverse ys))
reverse-++ (x ∷ xs) ys = trans step₁ step₂
  where
    step₁ : reverse (xs ++ ys) ++ [ x ] ≡ (reverse ys ++ reverse xs) ++ [ x ]
    step₁ = cong (_++ [ x ]) (reverse-++ xs ys)

    step₂ : (reverse ys ++ reverse xs) ++ [ x ] ≡ reverse ys ++ (reverse xs ++ [ x ])
    step₂ = ++-assoc (reverse ys)
    -- step₂ = ++-assoc (reverse ys) {zs = [ x ]}
    -- f _x = f _y  ==> _x = _y
    -- ys ++ _x = ys ++ _y ==> _x = _y

reverse-reverse : ∀{A} (xs : List A) → reverse (reverse xs) ≡ xs
reverse-reverse [] = refl
reverse-reverse (x ∷ xs) = trans step₁ step₂
  where
    step₁ : reverse (reverse xs ++ [ x ]) ≡ reverse [ x ] ++ reverse (reverse xs)
    step₁ = reverse-++ (reverse xs) [ x ]

    step₂ : reverse [ x ] ++ reverse (reverse xs) ≡ reverse [ x ] ++ xs
    step₂ = cong (x ∷_) (reverse-reverse xs)

-- -}
-- -}
-- -}
-- -}
