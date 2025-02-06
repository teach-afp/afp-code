-- Live coding 2024-02-05

module LiveInterpreter2025 where

import Control.Applicative
import Control.Monad

import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Expr_Parser as P (parseExpr, Language(..))

-- * Abstract syntax
------------------------------------------------------------------

data Expr
  -- Pure
  = Lit Integer
  | Expr :+: Expr
  -- Environment
  | Var Name
  | Let Name Expr Expr
  -- State
  | NewRef Expr
  | Deref Expr
  | Expr := Expr
  -- Exception
  | Catch Expr Expr
  -- Output
  | Print String
  deriving (Show)

type Name  = String
type Value = Integer

-- * Monad
------------------------------------------------------------------

type Eval a = IO a

runEval :: Eval a -> IO a
runEval m = m

-- * Evaluation
------------------------------------------------------------------

eval :: Expr -> Eval Value
-- Arithmetic
eval (Lit n)        = return n
eval (a :+: b)      = (+) <$> eval a <*> eval b

-- Printing
eval (Print m)      = 0 <$ putStrLn m

-- Local variables
eval (Var x)        = undefined
eval (Let x e1 e2)  = undefined

-- Pointers
eval (NewRef e)     = undefined
eval (Deref pe)     = undefined
eval (pe := ve)     = undefined

-- Exceptions
eval (Catch e1 e2)  = undefined

-- * Examples
------------------------------------------------------------------

e0 :: Expr
e0 = parse "1 + 2 + 3"

test0 = runEval $ eval e0

e01 :: Expr
e01 = parse "42 + print \"hello\""

test01 = runEval $ eval e01

e11 :: Expr
e11 = parse "let x = 5; (let x = 7; x) + x"

test11 = runEval $ eval e11

e1 :: Expr
e1 = parse "let x = print \"hello\" ; 42"

test1 = runEval $ eval e1

e21 :: Expr
e21 = parse "let x = new 42; !x + (x := 1)"

test21 = runEval $ eval e21

-- It recovers and produces and output
e2 :: Expr
e2 = parse "let one = new 1; \
                 \ let dummy = (try ((one := 2) + !7) catch 0) \
                 \             + print \"hello!\"; \
                 \ !one"
test2 = runEval $ eval e2

-- The side-effect in I/O is not executed
e3 :: Expr
e3 = parse "let one = new 1; \
                 \ let dummy = (try ( print \"hello!\" + (one := 2) + !7) \
                 \             catch 0); \
                 \ !one"

test3 = runEval $ eval e3

{-

-- * Environment
------------------------------------------------------------------

-- | An environment maps variables to values.
type Env = Map Name Value

emptyEnv :: Env
emptyEnv = Map.empty

lookupVar :: Name -> Eval Value
lookupVar x = do
  env <- ask
  case Map.lookup x env of
    Just v -> return v
    Nothing -> throwError $ UnboundVariable x

localScope :: Name -> Value -> Eval a -> Eval a
localScope x v m = local (\ env -> Map.insert x v env) m

-- * State
------------------------------------------------------------------

-- | Preliminaries for mutuable references
type Ptr = Integer

-- | Store
data Store = Store
  { nextPtr :: Ptr
  , heap    :: Map Ptr Value
  }

emptyStore :: Store
emptyStore = Store 0 Map.empty

-- | Create a new reference containing the given value.
newRef :: Value -> Eval Ptr
newRef v = do
  Store{ nextPtr = p, heap = h } <- get
  put Store{  nextPtr = p+1, heap = Map.insert p v h }
  return p

-- | Get the value of a reference.
deref :: Ptr -> Eval Value
deref p = do
  Store{ heap = h } <- get
  case Map.lookup p h of
    Just v -> return v
    Nothing -> throwError $ UnallocatedPointer p

-- | Assign a new value to a reference.
(=:) :: MonadState Store m => Ptr -> Value -> m Value
p =: v = do
  s@Store{ heap = h } <- get
  put $ s{ heap = Map.adjust (const v) p h }
  return v

-- * Exception
------------------------------------------------------------------

-- | Exception type
data Err
  = UnboundVariable Name
  | UnallocatedPointer Ptr
  | OtherError String
  deriving Show


-- * Output
------------------------------------------------------------------

msg :: String -> Eval ()
msg s = liftIO $ putStrLn s

-}


-- * Parser stuff
------------------------------------------------------------------------

language :: P.Language Expr
language = P.Lang
  { P.lLit    = Lit
  , P.lPlus   = (:+:)
  , P.lLet    = Let
  , P.lVar    = Var
  , P.lNewref = NewRef
  , P.lDeref  = Deref
  , P.lAssign = (:=)
  , P.lCatch  = Catch
  , P.lPrint  = Print
  }

parse :: String -> Expr
parse s = case P.parseExpr language s of
  Left err -> error (show err)
  Right x  -> x
