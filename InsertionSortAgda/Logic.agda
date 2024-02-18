-- Advanced Functional Programming course 2016 Chalmers/GU
--
-- 2016-02-25 Guest lecture by Andreas Abel
--
-- Introduction to Agda
--
-- File 4: Logic via the Curry-Howard isomorphism

module Logic where

-- Agda is based on constructive type theory à la Martin-Löf.
-- It relies on the Curry-Howard corresponence between programming and proving.

-- A proposition is a set (or type).
-- A proof of a proposition is an element of that set (type).

prop = Set

-- The proposition that is unconditionally true is a non-empty set
-- whose in habitant can be constructed effortlessly.

data ⊤ : prop where
  trivial : ⊤

-- The proposition that is unconditionally false is the empty set.
-- The data type has no constructors.

data ⊥ : prop where

-- "Ex falsum quod libet":
-- Assuming the absurd statement everything is provable.
-- We have reached an impossible situation, so we do need to continue our proof.

⊥-elim : ∀{A : prop} → ⊥ → A
⊥-elim ()

-- Implication is just the function space.
-- A proof of A → B turns a proof of A into a proof of B.

-- Implication is transitive.

comp : ∀{A B C : prop} → (A → B) → (B → C) → (A → C)
comp f g = λ a → g (f a)

-- Negation is implication of absurdity.

¬ : prop → prop
¬ A = A → ⊥

-- The contraposition law becomes a special case of transitivity.

contraposition : ∀{A B : prop} → (A → B) → ¬ B → ¬ A
contraposition = comp

-- To prove a conjunction, we need to provide proofs of both conjuncts.
-- Thus, conjunction is pairing.

data _∧_ (A B : prop) : prop where
  pair : (p : A) (q : B) → A ∧ B

-- Conjunction is commutative (of course).

commute : ∀{A B : prop} → A ∧ B → B ∧ A
commute (pair p q) = pair q p

-- A and ¬ A cannot hold at the same time

contradiction : ∀{A : prop} → ¬(A ∧ ¬ A)
contradiction (pair a ¬a) = ¬a a

-- To prove a disjunction A ∨ B,
-- we need to provide either a proof of A or a proof of B.

data _∨_ (A B : prop) : prop where
  inl : (q : A) → A ∨ B
  inr : (q : B) → A ∨ B

-- If either ¬A or B, then the implication A → B must hold

impl : ∀{A B} → ¬ A ∨ B → (A → B)
impl (inl q) a = ⊥-elim (q a)
impl (inr q) a = q

-- The converse is not provable constructively, as A → B neither implies ¬ A nor B.

-- The excluded middle ¬ A ∨ A is also not provable in general.

-- Propositions that fulfill the excluded middle are called "decidable".

-- Exercise: prove the double negation of the excluded middle:
-- ¬ (¬ (¬ A ∨ A))
