module Data.Monoid where

open import Prelude

-- https://hackage.haskell.org/package/base/docs/Data-Monoid.html#t:Monoid
record Monoid (M : Set) : Set where
  field
    mempty                    : M
    mappend                   : M → M → M
    monoid-left-identity-law  : ∀ {x} → mappend mempty x ≡ x
    monoid-right-identity-law : ∀ {x} → mappend x mempty ≡ x
    monoid-associativity-law  : ∀ {x y z} → mappend x (mappend y z) ≡ mappend (mappend x y) z

open Monoid {{...}} public

++-right-identity-law : ∀ {A : Set} → (xs : List A) → xs ++ [] ≡ xs
++-right-identity-law []        = [] ++ []
                                    ≡⟨⟩
                                  [] ∎
++-right-identity-law (x :: xs) = (x :: xs) ++ []
                                    ≡⟨⟩
                                  x :: (xs ++ [])
                                    ≡⟨ cong (_::_ x) (++-right-identity-law xs) ⟩
                                  x :: xs ∎

++-associativity-law : ∀ {A : Set} → (xs ys zs : List A) → xs ++ (ys ++ zs) ≡ (xs ++ ys) ++ zs
++-associativity-law []        ys zs = [] ++ (ys ++ zs)
                                         ≡⟨⟩
                                       ys ++ zs
                                         ≡⟨⟩
                                       ([] ++ ys) ++ zs ∎
++-associativity-law (x :: xs) ys zs = (x :: xs) ++ (ys ++ zs)
                                         ≡⟨⟩
                                       x :: (xs ++ (ys ++ zs))
                                         ≡⟨ cong (_::_ x) (++-associativity-law xs ys zs) ⟩
                                       x :: ((xs ++ ys) ++ zs)
                                         ≡⟨⟩
                                       (x :: (xs ++ ys)) ++ zs
                                         ≡⟨⟩
                                       ((x :: xs) ++ ys) ++ zs ∎

instance
  ListMonoid : ∀ {A} → Monoid (List A)
  ListMonoid {A} = let
                     mempty : List A
                     mempty = []

                     mappend : List A → List A → List A
                     mappend = _++_
                   in
                   record
                     { mempty                    = mempty
                     ; mappend                   = mappend
                     ; monoid-left-identity-law  = λ {xs} →
                         mappend mempty xs
                           ≡⟨⟩
                         [] ++ xs
                           ≡⟨⟩
                         xs ∎
                     ; monoid-right-identity-law = λ {xs} →
                         mappend xs mempty
                           ≡⟨⟩
                         xs ++ []
                           ≡⟨ ++-right-identity-law xs ⟩
                         xs ∎
                     ; monoid-associativity-law  = λ {xs} {ys} {zs} →
                         mappend xs (mappend ys zs)
                           ≡⟨⟩
                         xs ++ (ys ++ zs)
                           ≡⟨ ++-associativity-law xs ys zs ⟩
                         (xs ++ ys) ++ zs
                           ≡⟨⟩
                         mappend (mappend xs ys) zs ∎
                     }
