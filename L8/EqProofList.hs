{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Some equality proofs for list properties.

module EqProofList where

import Prelude
  ( ($)
  , Bool(True, False), (&&), otherwise
  , Eq(..), Ord((<), (<=))
  , Int, Num(..)
  , return
  )
import Test.QuickCheck
import EqualityProof

length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + length xs

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

-- (List a, [], ++) is a monoid

-- length is a monoid homomorphism from (List a, [], ++) to (Nat, 0, +)
--
-- 1. length []         = 0
-- 2. length (xs ++ ys) = length xs + length ys
--
-- Prove 2. by induction on xs:

-- Case [].
--
prop_length_append_nil :: [a] -> Proof _
prop_length_append_nil ys = proof
  (length ([] ++ ys))    =< Def (++)   >=
  (length ys)            =< Def "+"    >=
  (0 + length ys)        =< Def length >=
  (length [] + length ys)

-- Case (x:xs).
--
prop_length_append_cons :: a -> [a] -> [a] -> Proof _
prop_length_append_cons x xs ys = proof
  (length ((x:xs) ++ ys))      =< Def (++)   >=
  (length (x : (xs ++ ys)))    =< Def length >=
  (1 + length (xs ++ ys))      =< IH         >=
  (1 + length xs + length ys)  =< Def length >=
  (length (x:xs) + length ys)

-- Unit testing the proofs.

point_tests =
  [ check $ prop_length_append_nil  "foo"
  , check $ prop_length_append_cons 'H' "ello, " "world!"
  ]

{-

Proof by induction
==================

Lists as predicates on values
-----------------------------

List_A : Value -> Prop  for A some type.

1. "nil" is a list.

   -------
   List_A []

2. If x is a value of type A and xs a list then (x:xs) is a list.

   A x    List_A xs
   ----------------
   List_A (x:xs)

There are no other rules to generate list values, so List_A is the _least_ predicate closed under these rules.
We also say that List_A is an _inductive_predicate.

So if we have any other predicate P : Value -> Prop closed under these rules, we know that

   List xs => P xs

This is called the induction principle for lists:

> If P [] and P xs implies P (x:xs) for all x and xs, then P xs holds for all lists xs.

Instantiating the induction principle
-------------------------------------

We want to prove length (xs ++ ys) = length xs ++ length ys for all lists xs and ys.

We want induction on xs because (++) is defined by cases on its first argument.

There are two ways to instantiate the induction principle.

1. P xs = ∀ ys → length (xs ++ ys) = length xs ++ length ys

   (Here, P xs is a ∀-quantified formula.)

2. We fix some arbitrary ys and pick

         P xs = length (xs ++ ys) = length xs ++ length ys

   (Here, P xs is just an equation.)

In our case, both work, but 2. is conceptually simpler.
Sometimes the generality of 1. is needed.

So for arbitrary ys, we wish to prove ∀ xs. P xs.
By induction we have to show.

1. P []  which means length ([] ++ ys) = length [] ++ length ys
2. P (x:xs) under the induction hypothesis P xs.

-}


-- length is an involutive-monoid homomorphism
-- from (List a, [], ++, reverse) to (Nat, 0, +, id)
--
-- 3. length (reverse xs) = id (length xs)
--
-- Proof by induction on xs.
-- Case xs = [] is trivial by definition.

prop_length_reverse_cons x xs = proof
  (length (reverse (x:xs)))            =< Def reverse         >=
  (length (reverse xs ++ [x]))         =< Thm "length_append" >=
  (length (reverse xs) + length [x])   =< Conv                >=
  (length (reverse xs) + 1)            =< IH                  >=
  (length xs + 1)                      =< Def length          >=
  (length (x:xs))

-- reverse is involutive
--
-- 1. reverse (reverse xs) = xs
--
-- By induction on xs, case [] is trivial.

prop_reverse_reverse_cons x xs = proof
  (reverse (reverse (x:xs)))             =< Def reverse          >=
  (reverse (reverse xs ++ [x]))          =< Thm "reverse_append" >=
  (reverse [x] ++ reverse (reverse xs))  =< IH                   >=
  (reverse [x] ++ xs)                    =< Def reverse          >=
  ([x] ++ xs)                            =< Def (++)             >=
  (x:xs)

-- 2. reverse (xs ++ ys) = reverse ys ++ reverse xs

prop_reverse_append_cons x xs ys = proof
  (reverse ((x:xs) ++ ys))             =< Def (++)          >=
  (reverse (x : (xs ++ ys)))           =< Def reverse       >=
  (reverse (xs ++ ys) ++ [x])          =< IH                >=
  ((reverse ys ++ reverse xs) ++ [x])  =< Thm "List monoid" >=
  (reverse ys ++ (reverse xs ++ [x]))  =< Def reverse       >=
  (reverse ys ++ reverse (x:xs))

-- Insertion sort

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x < y     = x : y : ys
  | otherwise = y : insert x ys

sort :: Ord a => [a] -> [a]
sort []     = []
sort (x:xs) = insert x (sort xs)

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted (x:xs) = sorted1 x xs

sorted1 :: Ord a => a -> [a] -> Bool
sorted1 x [] = True
sorted1 x (y:ys) = x <= y && sorted1 y ys

-- sort sorts.
--
-- sorted (sort xs) = True

-- prop_sorted_sort_nil = proof
--   (sorted (sort [])) =< Def "sort" >=
--   (sorted [])        =< Def "sorted" >=
--   True

prop_sorted_sort_cons x xs = proof
  (sorted (sort (x:xs)))        =< Def "sort" >=
  (sorted (insert x (sort xs))) =< Thm "sorted_insert" >=
  (sorted (sort xs))            =< IH >=
  True

-- Inserting into sorted list
--
-- sorted xs = sorted (insert x xs)

-- Case x < y
prop_sorted_insert_cons_lt x y ys = proof
  (sorted (insert x (y:ys))) =< Def "insert"  >=
  (sorted (x : y : ys))      =< Def "sorted"  >=
  (sorted1 x (y : ys))       =< Def "sorted1" >=
  (x <= y && sorted1 y ys)   =< Hyp "x < y" >=
  (sorted1 y ys)             =< Def "sorted1" >=
  (sorted (y:ys))

-- Case x >= y
prop_sorted_insert_cons_ge x y ys = proof
  (sorted (insert x (y:ys))) =< Def "insert" >=
  (sorted (y : insert x ys)) =< Def "sorted" >=
  (sorted1 y (insert x ys))  =< Thm "sorted_insert" >=
  (sorted (y:ys))

-- Quickchecking the proofs.

return []

tests = $quickCheckAll
