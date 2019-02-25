{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Version 2 of the interpreter
module Basic where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Expr_Parser as P (parseExpr, Language (..))

-- | Even more interesting stuff: mutable references!
data Expr = Lit Integer
          | Expr :+: Expr
          | Var Name
          | Let Name Expr Expr
          | NewRef Expr
          | Deref Expr
          | Expr := Expr
  deriving (Show)

-- | Preliminaries for (immutable) local bindings
type Name   = String
type Value  = Integer

-- | An environment maps variables to values.
type Env = Map Name Value

emptyEnv :: Env
emptyEnv = Map.empty

-- | Preliminaries for mutuable references
type Ptr    = Value
  -- ^ dangerous language: any 'Value' can be used as a 'Ptr'

-- | We need to keep track of the store containing the values of
-- our references. We also remember the next unused pointer.
data Store = Store { nextPtr :: Ptr
                   , heap    :: Map Ptr Value
                   }

emptyStore :: Store
emptyStore = Store 0 Map.empty

-- | The store needs to be updated globally in a program so we
-- use a state monad to pass the store around.

newtype Eval a = MkEval (StateT Store (ReaderT Env Identity) a)
  deriving (Functor, Applicative,
            Monad, MonadState Store, MonadReader Env)

runEval :: Eval a -> a
runEval (MkEval st) = runIdentity
                      (runReaderT
                          (evalStateT st emptyStore)
                       emptyEnv)


-- * Environment manipulation (no changes from Interpreter1)
lookupVar :: Name -> Eval Value
lookupVar x = do
  env <- ask
  case Map.lookup x env of
    Nothing -> fail $ "Variable " ++ x ++ " not found."
    Just v  -> return v

localScope :: Name -> Value -> Eval a -> Eval a
localScope n v = local (Map.insert n v)

-- | Create a new reference containing the given value.
newRef :: Value -> Eval Ptr
newRef v = do
              store <- get
              let ptr      = nextPtr store
                  ptr'     = 1 + ptr
                  newHeap  = Map.insert ptr v (heap store)
              put (Store ptr' newHeap)
              return ptr

-- | Get the value of a reference. Crashes with our own
-- "segfault" if given a non-existing pointer.
deref :: Ptr -> Eval Value
deref p = do st <- get
             let h = heap st
             case Map.lookup p h of
               Nothing -> fail ("Segmentation fault: "++show p++" is not bound")
               Just v  -> return v

-- | Updating the value of a reference. Has no effect if the
-- reference doesn't exist. (Exercise: Maybe that's not the best
-- semantics... what would be a better one?)
-- Map.adjust :: (Ord k) => (a -> a) -> k -> Map k a -> Map k a

(=:) :: MonadState Store m => Ptr -> Value -> m Value -- change
p =: v = do store <- get
            let heap' = Map.adjust (const v) p (heap store)
            put (store {heap = heap'})
            return v

-- | As before we only need to add cases for the new con-
-- structors to the evaluator. No need to change the old stuff.
eval :: Expr -> Eval Value
eval (Lit n)        = return n
eval (a :+: b)      = (+) <$> eval a <*> eval b
eval (Var x)        = lookupVar x
eval (Let n e1 e2) = do v <- eval e1
                        localScope n v (eval e2)
eval (NewRef e)     = do v <- eval e
                         newRef v

eval (Deref e)      = do p <- eval e
                         deref p

eval (pe := ve)     = do p <- eval pe
                         v <- eval ve
                         p =: v

-- * Utilities: testing and parsing

-- Test for variables declaration
test1 :: Expr
test1 = parse "let x=10; x"
runtest1 = runEval $ eval test1

test2 :: Expr
test2 = parse "let x=secret 10; x"
runtest2 = runEval $ eval test2

test3 :: Expr
test3 = parse "let x=secret 10; let y = 20; x+y"
runtest3 = runEval $ eval test3

test4 :: Expr
test4 = parse "let x= 10; let y = 20; x+y"
runtest4 = runEval $ eval test4

-- Test for references

test5 :: Expr
test5 = parse "let p=new 1; !p"
runtest5 = runEval $ eval test5

test6 :: Expr
test6 = parse "let p=secret 42; !p"
runtest6 = runEval $ eval test6

test7 :: Expr
test7 = parse "let s=secret 42; let p=new 10; !s+!p"
runtest7 = runEval $ eval test7

-- It should fail!
test8 :: Expr
test8 = parse "let s=secret 42; let p=new 10; p := !s"
runtest8 = runEval $ eval test8

-- It should fail!
test9 :: Expr
test9 = parse "let s=secret 42; let p=new 10; s := !p"
runtest9 = runEval $ eval test9


-- | The parser is parameterised over the abstract syntax.
language :: P.Language Expr
language = P.Lang
  { P.lLit    = Lit
  , P.lPlus   = (:+:)
  , P.lLet    = Let
  , P.lVar    = Var
  , P.lNewref = NewRef
  , P.lNewSecretref = NewRef
  , P.lDeref  = Deref
  , P.lAssign = (:=)
  , P.lCatch  = \_ _ -> Var "language: not implemented: catch"
  }

parse :: String -> Expr
parse s = case P.parseExpr language s of
  Left err -> error (show err)
  Right x  -> x
