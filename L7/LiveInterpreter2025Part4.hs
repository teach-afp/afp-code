{-# LANGUAGE InstanceSigs #-}
-- Live coding 2025-02-06

module LiveInterpreter2025Part4 where

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

newtype Eval a = Eval { unEval :: Env -> StateT Store IO a }

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
newtype StateT  s m a = StateT  { runStateT  :: s -> m (a, s) }

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m s = do
  (a, _s) <- runStateT m s
  return a

instance Monad m => Monad (StateT s m) where
  return = pure
  m >>= k = StateT \ s -> do
    (a, s') <- runStateT m s
    runStateT (k a) s'

instance Monad m => Applicative (StateT s m) where
  pure a = StateT \ s -> return (a, s)
  (<*>) = ap

instance Monad m => Functor  (StateT s m) where
  fmap = liftM

instance Monad Eval where
  return   = pure
  m >>= k  = Eval \ env -> do
    v <- unEval m env
    unEval (k v) env

instance Applicative Eval where
  (<*>) = ap
  pure v = Eval \ _env -> return v

instance Functor Eval where
  fmap  = liftM

ask :: Eval Env
ask = Eval \ env -> return env

local :: (Env -> Env) -> Eval a -> Eval a
local f m = Eval \ env -> unEval m (f env)

class Monad m => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO IO where
  liftIO m = m

instance MonadIO m => MonadIO (StateT s m) where
  liftIO m = StateT \ s -> do
    a <- liftIO m
    return (a, s)

instance MonadIO Eval where
  liftIO :: IO a -> Eval a
  liftIO m = Eval \ _env -> do
    a <- liftIO m
    return a

modify :: (Store -> Store) -> Eval ()
modify f = Eval \ env -> StateT \ store ->
  return ((), f store)

get :: Eval Store
get = Eval \ env -> StateT \ store -> return (store, store)

put :: Store -> Eval ()
-- put store = Eval \ env _store -> return ((), store)
put store = modify $ const store
-- put = modify . const

runEval :: Eval a -> IO a
runEval m = do
  v <- evalStateT (unEval m emptyEnv) emptyStore
  return v

-- * Evaluation
------------------------------------------------------------------

eval :: Expr -> Eval Value
-- Pure
eval (Lit n)        = return n
eval (a :+: b)      = (+) <$> eval a <*> eval b
  -- do i <- eval a
  --    j <- eval b
  --    return (i + j)

-- Printing
eval (Print m)      = 0 <$ liftIO (putStrLn m)
  -- (<$>) :: (a -> b) -> m a -> m b
  -- (<$)  ::       b  -> m a -> m b
  -- do liftIO $ putStrLn m
  --    return 0

-- Local variables
eval (Var x)        = do
  env <- ask
  return $ Map.findWithDefault err x env
  where err = error $ "unbound variable: " ++ x
eval (Let x e1 e2)  = do
  v1 <- eval e1
  v2 <- local (Map.insert x v1) $ eval e2
  return v2

-- Pointers
eval (NewRef e)     = do
  v <- eval e
  Store p heap <- get
  put $ Store (p+1) $ Map.insert p v heap
  return p

eval (Deref pe)     = do
  p <- eval pe
  Store _ heap <- get
  let err = error $ "Unallocated pointer: " ++ show p
  return $ Map.findWithDefault err p heap

eval (pe := ve)     = do
  v <- eval ve
  p <- eval pe
  modify \ (Store n heap) -> Store n $ Map.adjust (const v) p heap
  return v

-- -- Exceptions
-- eval (Catch e1 e2)  = undefined

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

-- It recovers and produces and output
e20 :: Expr
e20 = parse "let one = new 1; \
                 \ let dummy = ((one := 2) + !one)  \
                 \             + print \"hello!\"; \
                 \ !one"
test20 = runEval $ eval e20

-- The side-effect in I/O is not executed
e3 :: Expr
e3 = parse "let one = new 1; \
                 \ let dummy = (try ( print \"hello!\" + (one := 2) + !7) \
                 \             catch 0); \
                 \ !one"

test3 = runEval $ eval e3


-- * Environment
------------------------------------------------------------------

-- | An environment maps variables to values.
type Env = Map Name Value

emptyEnv :: Env
emptyEnv = Map.empty

{-
lookupVar :: Name -> Eval Value
lookupVar x = do
  env <- ask
  case Map.lookup x env of
    Just v -> return v
    Nothing -> throwError $ UnboundVariable x

localScope :: Name -> Value -> Eval a -> Eval a
localScope x v m = local (\ env -> Map.insert x v env) m
-}

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

{-

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
