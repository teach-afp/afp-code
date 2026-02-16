```
{-# OPTIONS --no-keep-pattern-variables #-}

module IntroTT where

open import Agda.Primitive
```

Introduction to Type Theory and Dependent Types
===============================================

In dependently typed programming languages such as Agda, Idris, Lean, and Rocq,
types can depend on values.

- Types can be computed from values.
- Types can be indexed from values.

Types are first-class citizens, and there is a type `Set` of types.

Inductive types
---------------

Data types in Agda are similar to data types in Haskell.

In contrast to Haskell, they do not permit infinite inhabitants.
E.g. there is no infinite list in this list type:

```
data List (A : Set) : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A   -- \ : :  =  ∷

infixr 8 _∷_
```

Even basic types like the type of natural numbers or the type of tuples
are not hardwired into Agda, but can be defined from scratch.

Type of natural numbers.
```
data Nat : Set where
  zero : Nat
  suc  : (n : Nat) → Nat

{-# BUILTIN NATURAL Nat #-}
```
The `BUILTIN NATURAL` pragma enables decimal notation for values in `Nat`.
```
three : Nat
three = 3
```

Cartesian product: type of pairs.
```
record _×_ (A B : Set) : Set where
  constructor _,_
  field
    fst : A
    snd : B
open _×_ using (fst; snd)
infixr 4 _,_ _×_
```
The `open` statement brings the projections `fst` and `snd` into scope.


The unit type: the type of the empty tuple.
```
record ⊤ : Set where
  constructor tt
```

Types can be computed from values
---------------------------------

Local modules like `ComputedTypes` help us manage our name space.
```
module ComputedTypes where
```

We define the type of vectors of length `n` computed by recursion on `n`.
```
  Vec : (A : Set) (n : Nat) → Set
  Vec A zero    = ⊤
  Vec A (suc n) = A × Vec A n
```

Example.
```
  down3 : Vec Nat 3
  down3 = 2 , 1 , 0 , tt
```

Now we define the type `Fin n` of numbers strictly below `n` by recursion on `n`.

There are no numbers strictly below `0`, so we produce an empty type for `Fin 0`.
```
  data ⊥ : Set where
    -- no constructors!
```

In case `Fin (suc n)` there we shift all numbers in `Fin n` up by one
and add a new zero.
This is done by auxiliary type `FNat` which is isomorphic to `Maybe`.
```
  data FNat (A : Set) : Set where
    fzero : FNat A
    fsuc : (i : A) → FNat A
```

Now `Fin` is defined by cases
```
  Fin : (n : Nat) → Set
  Fin zero = ⊥
  Fin (suc n) = FNat (Fin n)
```

Examples.
```
  one3 : Fin 3
  one3 = fsuc fzero

  two3 : Fin 3
  two3 = fsuc (fsuc fzero)
```

We can implement a safe indexing function for our vector where the index is never out of bounds.
The indices into a `Vec A n` live in `Fin n`.
```
  lookup : {A : Set} (n : Nat) (xs : Vec A n) (i : Fin n) → A
  lookup zero tt ()
  lookup (suc n) (x , xs) fzero    = x
  lookup (suc n) (x , xs) (fsuc i) = lookup n xs i
```

Examples.
```
  lone3 = lookup 3 down3 one3
  ltwo3 = lookup 3 down3 two3

  -- hole = {!ltwo3!}
```

Types can be indexed by values
------------------------------

```
module IndexedTypes where
```

Agda's `data` supports also the definition of _inductive families_
which are _indexed inductive types_.
These are similar to Haskell's GADTs (generalized algebraic data types),
but indices need not live on the type level as in Haskell,
but can be values.

Let's define the tyype of vectors indexed by their length:
```
  data Vec (A : Set) : (n : Nat) → Set where
    vnil  : Vec A zero
    vcons : {n : Nat} (x : A) (xs : Vec A n) → Vec A (suc n)
```
In a data type signature like `data Vec (A : Set) : (n : Nat) → Set`
we distinguish arguments to the data type before the colon (here: `A : Set`),
and those after the colon (here: `n : Nat`).
The former are called _parameters_ and the latter _indices_.
Indices can change in the target of the constructor (here: `zero` and `suc n` over `n`),
whereas parameters (like `A`) are usually fixed, but could change in the arguments of the constructor
(e.g. we could have `(xs : Vec (A × A) n)`, but this would give us a very different type).

Example.
```
  down3 : Vec Nat 3
  down3 = vcons 2 (vcons 1 (vcons 0 vnil))
```

The type `Fin n` of numbers below `n` can also be given as inductive family.
```
  data Fin : (n : Nat) → Set where
    fzero : {n : Nat} → Fin (suc n)
    fsuc  : {n : Nat} (i : Fin n) → Fin (suc n)
```

Example.
```
  one3 : Fin 3
  one3 = fsuc {n = 2} fzero

  two3 : Fin 3
  two3 = fsuc (fsuc fzero)
```

Safe vector lookup.
```
  lookup : {A : Set} {n : Nat} (xs : Vec A n) (i : Fin n) → A
  -- lookup vnil ()
  lookup (vcons x xs) fzero = x
  lookup (vcons x xs) (fsuc i) = lookup xs i
```

Examples.
```
  lone3 = lookup {n = 3} down3 one3
  ltwo3 = lookup {n = 3} down3 two3

  -- hole = {!lone3!}
```

The Equality Type
-----------------

Agda automatically unfolds definitions.
We say two terms `t` and `u` are _definitionally equal_, written `t = u`,
when `t` and `u` can be converted into each other by unfolding definitions.

Importantly, we can also define a _type_ `t ≡ u` that asserts that `t` and `u` are equal.
This type is e.g. used when proving an equality by induction.
It also allows us to _assume_ that certain terms are equal,
something that is not possible with definitional equality.

The equality type is an indexed inductive type with just one constructor for trivial equality,
`refl`exivity.
```
data _≡_ {A : Set} (x : A) : (y : A) → Set where
  refl : x ≡ x

infix 6 _≡_
```

Example.
```
eqlone3 : ComputedTypes.lone3 ≡ IndexedTypes.lone3
eqlone3 = refl
```
Here `refl` constructs this equality because both sides are _definitionally equal_.

We can thus use `refl` to write down simple unit tests.

Assumed equalities can be used by matching on `refl`.
Thus, we can establish that equality is symmetric and transitive.
```
sym' : {A : Set} (a b : A) (eq : a ≡ b) → b ≡ a
sym' a .a refl = refl

trans : {A : Set} {a b c : A} (eq1 : a ≡ b) (eq2 : b ≡ c) → a ≡ c
trans refl eq2 = eq2
```

For reasons of practicality, let's define `sym` with more hidden arguments:
```
sym : {A : Set} {a b : A} (eq : a ≡ b) → b ≡ a
sym refl = refl
```

Equality is also a congruence; all functions respect equality.
```
cong : {A B : Set} (f : A → B) {a a' : A} (eq : a ≡ a') → f a ≡ f a'
cong f refl = refl
```


Proof by induction
------------------

Let us define addition of natural numbers and prove that is forms a commutative monoid.
```
_+_ : (n m : Nat) → Nat
zero + m = m
suc n + m = suc (n + m)

infixl 8 _+_
```

The left unit law holds by definition:
```
zero-plus : {m : Nat} → 0 + m ≡ m
zero-plus = refl
```

The right unit law needs proof by induction on `n`:
```
plus-zero : {n : Nat} → n + 0 ≡ n
plus-zero {n = zero} = refl
plus-zero {n = suc n} = cong suc (plus-zero {n = n})
```

Associativity is similar:
```
plus-assoc : {n m l : Nat} → (n + m) + l ≡ n + (m + l)
plus-assoc {n = zero} = refl
plus-assoc {n = suc n} = cong suc (plus-assoc {n = n})
```

Commutativity needs an auxility lemma.
```
plus-suc : {n m : Nat} → n + suc m ≡ suc (n + m)
plus-suc {n = zero} = refl
plus-suc {n = suc n} = cong suc plus-suc
```

The proof of commutativity is best done by induction on `m`,
because then our lemmata are oriented in the right way.
Otherwise, we would need to invoke `sym`.
```
plus-comm : {n m : Nat} → n + m ≡ m + n
plus-comm {m = zero} = plus-zero
plus-comm {m = suc m} = trans plus-suc (cong suc (plus-comm {m = m}))
```

A syntax for equality chains
----------------------------

```
-- Identity as prefix operator
infix 1 begin_
begin_  : {A : Set} {x y : A} → x ≡ y → x ≡ y
begin eq = eq

-- Transitivity as infix operator
infixr 2 _≡⟨_⟩_
_≡⟨_⟩_  : {A : Set} (x : A) {y z : A} → x ≡ y → y ≡ z → x ≡ z
x ≡⟨ refl ⟩ eq = eq

infixr 2 _≡⟨⟩_
_≡⟨⟩_  : {A : Set} (x : A) {z : A} → x ≡ z → x ≡ z
x ≡⟨⟩ eq = eq

-- Reflexivity as postfix operator
infix 3 _∎
_∎ : {A : Set} (x : A) → x ≡ x
x ∎ = refl
```

Proving list properties by induction
------------------------------------

Concatenation of lists.
```
_++_ : {A : Set} (xs ys : List A) → List A
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

infixl 8 _++_
```

Concatenation is associative.
```
++-assoc : {A : Set} (xs ys zs : List A) → (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)
++-assoc [] ys zs = refl
++-assoc (x ∷ xs) ys zs =
  ((x ∷ xs) ++ ys) ++ zs    ≡⟨ refl ⟩
  (x ∷ (xs ++ ys)) ++ zs    ≡⟨ refl ⟩
  x ∷ ((xs ++ ys) ++ zs)    ≡⟨ cong (x ∷_) (++-assoc xs ys zs) ⟩
  x ∷ (xs ++ (ys ++ zs))    ≡⟨ refl ⟩
  (x ∷ xs) ++ (ys ++ zs)    ∎
```

Concatenations makes a monoid.
```
++-[] : {A : Set} (xs : List A) → xs ++ [] ≡ xs
++-[] [] = refl
++-[] (x ∷ xs) = cong (x ∷_) (++-[] xs)
```

List reversal.
```
reverse : {A : Set} (xs : List A) → List A
reverse [] = []
reverse (x ∷ xs) = reverse xs ++ (x ∷ [])
```

Reverse is an involution.

Reverse distributes over concat, commuting the concatenated lists.
```
reverse-++ : {A : Set} (xs ys : List A) → reverse (xs ++ ys) ≡ reverse ys ++ reverse xs
reverse-++ [] ys = sym (++-[] (reverse ys))
reverse-++ (x ∷ xs) ys = begin
  reverse ((x ∷ xs) ++ ys)                ≡⟨ refl ⟩
  reverse (x ∷ (xs ++ ys))                ≡⟨ refl ⟩
  reverse (xs ++ ys) ++ (x ∷ [])          ≡⟨ cong (_++ (x ∷ [])) (reverse-++ xs ys) ⟩
  (reverse ys ++ reverse xs) ++ (x ∷ [])  ≡⟨  ++-assoc (reverse ys) (reverse xs) _ ⟩
  reverse ys ++ (reverse xs ++ (x ∷ []))  ≡⟨ refl ⟩
  reverse ys ++ reverse (x ∷ xs)          ∎
```

Reversal is an operation of order 2, i.e., it is its own inverse.
```
reverse-reverse : {A : Set} (xs : List A) → reverse (reverse xs) ≡ xs
reverse-reverse [] = refl
reverse-reverse (x ∷ xs) = begin
  reverse (reverse (x ∷ xs))                ≡⟨ refl ⟩
  reverse (reverse xs ++ (x ∷ []))          ≡⟨ reverse-++ (reverse xs) (x ∷ []) ⟩
  reverse (x ∷ []) ++ reverse (reverse xs)  ≡⟨ refl ⟩
  (x ∷ []) ++ reverse (reverse xs)          ≡⟨ refl ⟩
  x ∷ reverse (reverse xs)                  ≡⟨ cong (x ∷_) (reverse-reverse xs) ⟩
  x ∷ xs                                    ∎
```
