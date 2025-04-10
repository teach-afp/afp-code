-- AFP 2025 Proving that lists form an involutive monoid under append and reverse.

-- This version uses the standard library for lists and equality chains.

module ListStdLib where

open import Agda.Primitive renaming (Set to Type)

open import Data.List.Base using (List; []; _∷_; [_]; _++_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; cong; module ≡-Reasoning)
open ≡-Reasoning

variable
  A : Type

-- We use our own definition of reverse,
-- because the standard library's reverse is defined in terms of foldl.

reverse : List A → List A
reverse [] = []
reverse (x ∷ xs) = reverse xs ++ [ x ]

-- Monoid laws for (List, [], ++)

++-[] : (xs : List A) → xs ++ [] ≡ xs
++-[] [] = refl
++-[] (x ∷ xs) = cong (x ∷_) (++-[] xs)

++-assoc : (xs {ys zs} : List A) → (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)
++-assoc [] = refl
++-assoc (x ∷ xs) = cong (x ∷_) (++-assoc xs)

-- Involution laws

reverse-++ : (xs ys : List A) → reverse (xs ++ ys) ≡ reverse ys ++ reverse xs

reverse-++ []       ys = sym (++-[] (reverse ys))

reverse-++ (x ∷ xs) ys = begin
  reverse (x ∷ xs ++ ys)              ≡⟨ refl ⟩
  reverse (xs ++ ys) ++ [ x ]         ≡⟨ cong (_++ [ x ]) (reverse-++ xs ys) ⟩
  (reverse ys ++ reverse xs) ++ [ x ] ≡⟨ ++-assoc (reverse ys) ⟩
  reverse ys ++ (reverse xs ++ [ x ]) ≡⟨ refl ⟩
  reverse ys ++ reverse (x ∷ xs)
    ∎

reverse-reverse : (xs : List A) → reverse (reverse xs) ≡ xs
reverse-reverse [] = refl
reverse-reverse (x ∷ xs) = begin
  reverse (reverse (x ∷ xs))            ≡⟨ refl ⟩
  reverse (reverse xs ++ [ x ])         ≡⟨ reverse-++ (reverse xs) [ x ] ⟩
  reverse [ x ] ++ reverse (reverse xs) ≡⟨ refl ⟩
  [ x ] ++ reverse (reverse xs)         ≡⟨ cong ([ x ] ++_) (reverse-reverse xs) ⟩
  [ x ] ++ xs                           ≡⟨ refl ⟩
  x ∷ xs                                ∎

-- -}
-- -}
-- -}
-- -}
