{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Some equality proofs for list properties.

module EqProofList where

import Prelude
  ( ($)
  , Bool(True, False), (&&)
  , Eq(..)
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



-- Quickchecking the proofs.

return []

tests = $quickCheckAll
