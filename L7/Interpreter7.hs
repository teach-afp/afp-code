{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

-- | Interpreter written with service monads.

module Interpreter7 where

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

-- | Exceptions that can be thrown during evaluation.
data Err
  = UnboundVariable Name
  | UnallocatedPointer Ptr
  | OtherError String
  deriving Show

-- * Evaluation
------------------------------------------------------------------


type Eval a = EnvT (StoreT (ErrT IO)) a

runEval :: Eval a -> IO (Either Err a)
runEval m = m & runEnv & evalStore & runErr

eval :: Expr -> Eval Value
eval (Lit n)        = return n
eval (a :+: b)      = (+) <$> eval a <*> eval b
eval (Var x)        = maybeErr (UnboundVariable x) $ lookupVar x
eval (Let x e1 e2)  = do
  v1 <- eval e1
  localScope x v1 $ eval e2
eval (NewRef e)     = do
  v <- eval e
  newRef v
eval (Deref pe)     = do
  p <- eval pe
  maybeErr (UnallocatedPointer p) $ deref p
eval (pe := ve)     = do
  p <- eval pe
  v <- eval ve
  maybeErr (UnallocatedPointer p) $ p =: v
eval (Catch e1 e2)  = do
  catchErr (eval e1) \ _ -> eval e2
eval (Print m)      = 0 <$ msg m


-- * Environment
------------------------------------------------------------------

-- | An environment maps variables to values.
type Env = Map Name Value

emptyEnv :: Env
emptyEnv = Map.empty

-- | A service class for local variables.
class Monad m => MonadEnv m where
  lookupVar :: Name -> m (Maybe Value)
  localScope :: Name -> Value -> m a -> m a

-- | A monad transformer implementing the environment service.
newtype EnvT m a = EnvT { runEnvT :: ReaderT Env m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadError e, MonadState s)

-- | Running the environment service on an empty environment.
runEnv :: Monad m => EnvT m a -> m a
runEnv m = runReaderT (runEnvT m) emptyEnv

instance Monad m => MonadEnv (EnvT m) where
  lookupVar :: Monad m => Name -> EnvT m (Maybe Value)
  lookupVar x = EnvT do
    env <- ask
    return $ Map.lookup x env

  localScope :: Monad m => Name -> Value -> EnvT m a -> EnvT m a
  localScope x v m = EnvT $ local (\ env -> Map.insert x v env) $ runEnvT m

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

-- | A service class for mutable references.
class Monad m => MonadStore m where
  newRef :: Value -> m Ptr
  deref  :: Ptr -> m (Maybe Value)
  (=:)   :: Ptr -> Value -> m (Maybe Value)

-- | A monad transformer implementing the store service.
newtype StoreT m a = StoreT { runStoreT :: StateT Store m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadReader r, MonadError e)

-- | Running the store service on an empty store.
runStore :: Monad m => StoreT m a -> m (a, Store)
runStore m = runStateT (runStoreT m) emptyStore

evalStore :: Monad m => StoreT m a -> m a
evalStore m = evalStateT (runStoreT m) emptyStore

-- | Store service instance.
instance Monad m => MonadStore (StoreT m) where
  newRef :: Monad m => Value -> StoreT m Ptr
  newRef v = StoreT do
    s@Store{ nextPtr = p, heap = h } <- get
    put $ s{ nextPtr = p+1, heap = Map.insert p v h }
    return p

  deref :: Monad m => Ptr -> StoreT m (Maybe Value)
  deref p = StoreT do
    Store{ heap = h } <- get
    return $ Map.lookup p h

  (=:) :: Monad m => Ptr -> Value -> StoreT m (Maybe Value)
  p =: v = StoreT do
    s@Store{ heap = h } <- get
    if not (Map.member p h) then return Nothing else do
      put $ s{ heap = Map.adjust (const v) p h }
      return $ Just v

-- * Exception
------------------------------------------------------------------

-- | A service class for exceptions.
class Monad m => MonadErr m where
  throwErr :: Err -> m a
  catchErr :: m a -> (Err -> m a) -> m a

  maybeErr  :: Err -> m (Maybe a) -> m a
  maybeErr e m = m >>= \case
    Just x  -> return x
    Nothing -> throwErr e

-- | A monad transformer implementing the exception service.
newtype ErrT m a = ErrT { runErrT :: ExceptT Err m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadReader r, MonadState s)

-- | Running the exception service.
runErr :: Monad m => ErrT m a -> m (Either Err a)
runErr m = runExceptT (runErrT m)

instance Monad m => MonadErr (ErrT m) where
  throwErr :: Monad m => Err -> ErrT m a
  throwErr = ErrT . throwError

  catchErr :: Monad m => ErrT m a -> (Err -> ErrT m a) -> ErrT m a
  catchErr m h = ErrT $ catchError (runErrT m) (runErrT . h)


-- * Output
------------------------------------------------------------------

msg :: String -> Eval ()
msg s = liftIO $ putStrLn s


-- * Monad lifting
------------------------------------------------------------------

{-}
-- ** Lifting MonadEnv

instance MonadEnv m => MonadEnv (StoreT m) where
  lookupVar :: Monad m => Name -> StoreT m (Maybe Value)
  lookupVar = lift . lookupVar

  localScope :: Monad m => Name -> Value -> StoreT m a -> StoreT m a
  localScope x v m = StoreT do
    mapStateT (localScope x v) (runStoreT m)
-}

-- ** Lifting MonadStore

instance MonadStore m => MonadStore (EnvT m) where
  newRef :: Monad m => Value -> EnvT m Ptr
  newRef = lift . newRef

  deref :: Monad m => Ptr -> EnvT m (Maybe Value)
  deref = lift . deref

  (=:) :: Monad m => Ptr -> Value -> EnvT m (Maybe Value)
  (=:) p v = lift $ p =: v

-- ** Lifting MonadErr

instance MonadErr m => MonadErr (EnvT m) where
  throwErr :: Monad m => Err -> EnvT m a
  throwErr = lift . throwErr

  catchErr :: Monad m => EnvT m a -> (Err -> EnvT m a) -> EnvT m a
  catchErr m h = EnvT $
    runEnvT m `catchErr` \ e -> runEnvT (h e)

instance MonadErr m => MonadErr (ReaderT r m) where
  throwErr :: Monad m => Err -> ReaderT r m a
  throwErr = lift . throwErr

  catchErr :: Monad m => ReaderT r m a -> (Err -> ReaderT r m a) -> ReaderT r m a
  catchErr m h = ReaderT \ r ->
    runReaderT m r `catchErr` \ e -> runReaderT (h e) r


instance MonadErr m => MonadErr (StoreT m) where
  throwErr :: Monad m => Err -> StoreT m a
  throwErr = lift . throwErr

  catchErr :: Monad m => StoreT m a -> (Err -> StoreT m a) -> StoreT m a
  catchErr m h = StoreT $
    runStoreT m `catchErr` \ e -> runStoreT (h e)

instance MonadErr m => MonadErr (StateT s m) where
  throwErr :: Monad m => Err -> StateT s m a
  throwErr = lift . throwErr

  catchErr :: Monad m => StateT s m a -> (Err -> StateT s m a) -> StateT s m a
  catchErr m h = StateT \ s ->
    runStateT m s `catchErr` \ e -> runStateT (h e) s

-- * Examples
------------------------------------------------------------------

e0 :: Expr
e0 = parse "1 + 2 + 3"

test0 = runEval $ eval e0

e11 :: Expr
e11 = parse "let x = 5; (let x = 7; x) + x"

test11 = runEval $ eval e11

e1 :: Expr
e1 = parse "let x = print \"hello\" ; 42"

-- test1 :: IO (Either Err Value)
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
-- test2 :: IO (Either Err Value)
test2 = runEval $ eval e2

-- The side-effect in I/O is not executed
e3 :: Expr
e3 = parse "let one = new 1; \
                 \ let dummy = (try ( print \"hello!\" + (one := 2) + !7) \
                 \             catch 0); \
                 \ !one"
-- test3 :: IO (Either Err Value)
test3 = runEval $ eval e3


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
