{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeAbstractions #-}  -- not in GHC 9.4

-- | Using predefined type representation.

module ExprTypeRep where

import Data.Kind (Type)
import Data.Type.Equality (testEquality)
import Type.Reflection (Typeable, typeRep, pattern Refl)

-- Indexing user-level terms by Haskell types

data Tm a where                              -- GADTs
  TLit  :: a -> Tm a
  TPlus :: Tm Int -> Tm Int -> Tm Int
  TIf   :: Tm Bool -> Tm a -> Tm a -> Tm a

deriving instance Show a => Show (Tm a)

eval :: Tm a -> a
eval (TLit v)      = v
eval (TPlus t1 t2) = eval t1 + eval t2
eval (TIf t t1 t2) = if eval t then eval t1 else eval t2

data Exp where
  EInt  :: Int -> Exp
  EBool :: Bool -> Exp
  EPlus :: Exp -> Exp -> Exp
  EIf   :: Exp -> Exp -> Exp -> Exp
  deriving Show

------------------------------------------------------------------------
-- Type inference

-- Existential type (general form)

data Ex (b :: a -> Type) where               -- KindSignatures (::), PolyKinds (a)
  Wrap :: (Show a, Typeable a) => b a -> Ex b

deriving instance (forall a. Show a => Show (b a)) => Show (Ex b)   -- QuantifiedConstraints

-- Type inference.
--
-- The type applications here are just for clarity, Haskell infers them.

infer :: Exp -> Maybe (Ex Tm)
infer (EInt i)  = return $ Wrap @Int  $ TLit i
infer (EBool b) = return $ Wrap @Bool $ TLit b
infer (EPlus e1 e2) = do
  t1 <- check @Int e1                        -- TypeApplications
  t2 <- check @Int e2
  return $ Wrap @Int $ TPlus t1 t2
infer (EIf e e1 e2) = do
  t          <- check @Bool e
  Wrap @a t1 <- infer e1                     -- TypeAbstractions
  t2         <- check @a e2
  return $ Wrap $ TIf t t1 t2

check :: forall a. Typeable a => Exp -> Maybe (Tm a)
check e = do
  Wrap @b t <- infer e
  cast @b @a t

-- Use the type equality from "Data.Type.Equality".

cast :: forall a b. (Typeable a, Typeable b) => Tm a -> Maybe (Tm b)
cast t = do
  Refl <- testEquality (typeRep @a) (typeRep @b)
  return t


erase :: forall a. Typeable a => Tm a -> Exp
erase = \case
  TLit v
    | Just Refl <- testEquality (typeRep @a) (typeRep @Int)  -> EInt v
    | Just Refl <- testEquality (typeRep @a) (typeRep @Bool) -> EBool v
  TPlus t1 t2 -> EPlus (erase t1) (erase t2)
  TIf t t1 t2 -> EIf (erase t) (erase t1) (erase t2)
