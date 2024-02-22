{-# LANGUAGE GADTs #-}

-- | GADTs: Indexing data types by types.

module ExprTypeIndexed where

-- Indexing user-level terms by Haskell types

data Tm a where
  TLit  :: a -> Tm a
  TPlus :: Tm Int -> Tm Int -> Tm Int
  TIf   :: Tm Bool -> Tm a -> Tm a -> Tm a

deriving instance Show a => Show (Tm a)

-- Tagless interpreter

eval :: Tm a -> a
eval (TLit v)      = undefined
eval (TPlus t1 t2) = undefined
eval (TIf t t1 t2) = undefined

-- Untyped user-level terms

data Exp where
  EInt  :: Int -> Exp
  EBool :: Bool -> Exp
  EPlus :: Exp -> Exp -> Exp
  EIf   :: Exp -> Exp -> Exp -> Exp
  deriving Show

------------------------------------------------------------------------
-- Type inference

-- Representing a type at runtime.  "Singleton type".

data RTy a where
  RInt  :: RTy Int
  RBool :: RTy Bool

deriving instance Show (RTy a)

-- Existential type for the result of type inference.
--
-- Packs a typed term with its type in runtime representation.
-- Also packs a 'Show' dictionary to make the pack printable.

data TmX where
  (:::) :: Show a => Tm a -> RTy a -> TmX

deriving instance Show TmX

-- Type inference

infer :: Exp -> Maybe TmX

infer (EInt i)      = undefined
infer (EBool b)     = undefined
infer (EPlus e1 e2) = undefined
infer (EIf e e1 e2) = undefined

-- Type checking

check :: Exp -> RTy a -> Maybe (Tm a)
check e a = undefined

-- Type conversion needs the runtime representation.

cast :: RTy a -> RTy b -> Tm a -> Maybe (Tm b)
cast = undefined

-- Erasing type information.
--
-- Needs the runtime representation for the 'TLit' case.
erase :: Tm a -> RTy a -> Exp
erase (TLit i)     RInt  = EInt i
erase (TLit b)     RBool = EBool b
erase (TPlus t1 t2) RInt = EPlus (erase t1 RInt) (erase t2 RInt)
erase (TIf t t1 t2) a    = EIf (erase t RBool) (erase t1 a) (erase t2 a)

-- Factoring out type equality from 'cast'.

data Equal a b where
  Refl :: Equal a a

equal :: RTy a -> RTy b -> Maybe (Equal a b)
equal = undefined

cast' :: RTy a -> RTy b -> Tm a -> Maybe (Tm b)
cast' a b t = undefined
