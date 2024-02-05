-- Live coding 2024-02-05

module LiveInterpreter where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Expr_Parser as P (parseExpr, Language(..))

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

-- * Evaluation
------------------------------------------------------------------

-- type Eval a = StateT Store (ReaderT Env Identity) a
-- s → r → Identity (a, s)

type Eval a = ReaderT Env (StateT Store (ExceptT Err IO)) a
-- r → s → Identity (a, s)

runEval :: Eval a -> IO (Either Err a)
runEval m = m & (`runReaderT` emptyEnv) & (`evalStateT` emptyStore) & runExceptT

eval :: Expr -> Eval Value
eval (Lit n)        = return n
eval (a :+: b)      = (+) <$> eval a <*> eval b
eval (Var x)        = lookupVar x
eval (Let x e1 e2)  = do
  v1 <- eval e1
  localScope x v1 $ eval e2
eval (NewRef e)     = do
  v <- eval e
  newRef v
eval (Deref pe)     = do
  p <- eval pe
  deref p
eval (pe := ve)     = do
  p <- eval pe
  v <- eval ve
  p =: v
eval (Catch e1 e2)  = do
  catchError (eval e1) \ _ -> eval e2
eval (Print m)      = 0 <$ msg m

-- eval :: Env -> Expr -> Eval Value
-- eval env (Lit n)        = return n
-- eval env (a :+: b)      = (+) <$> eval env a <*> eval env b
-- eval env (Var x)        = return $ Map.findWithDefault undefined x env
-- eval env (Let x e1 e2)  = do
--   v1 <- eval env e1
--   eval (Map.insert x v1 env) e2
-- eval env (NewRef e)     = undefined
-- eval env (Deref pe)     = undefined
-- eval env (pe := ve)     = undefined
-- eval env (Catch e1 e2)  = undefined
-- eval env (Print m)      = undefined

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


-- * Examples
------------------------------------------------------------------

testExpr0 :: Expr
testExpr0 = parse "1 + 2 + 3"

test0 = runEval $ eval testExpr0

testExpr11 :: Expr
testExpr11 = parse "let x = 5; (let x = 7; x) + x"

test11 = runEval $ eval testExpr11

testExpr1 :: Expr
testExpr1 = parse "let x = print \"hello\" ; 42"

-- test1 :: IO (Either Err Value)
test1 = runEval $ eval testExpr1

testExpr21 :: Expr
testExpr21 = parse "let x = new 42; !x + (x := 1)"

test21 = runEval $ eval testExpr21

-- It recovers and produces and output
testExpr2 :: Expr
testExpr2 = parse "let one = new 1; \
                 \ let dummy = (try ((one := 2) + !7) catch 0) \
                 \             + print \"hello!\"; \
                 \ !one"
-- test2 :: IO (Either Err Value)
test2 = runEval $ eval testExpr2

-- The side-effect in I/O is not executed
testExpr3 :: Expr
testExpr3 = parse "let one = new 1; \
                 \ let dummy = (try ( print \"hello!\" + (one := 2) + !7) \
                 \             catch 0); \
                 \ !one"
-- test3 :: IO (Either Err Value)
test3 = runEval $ eval testExpr3


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
