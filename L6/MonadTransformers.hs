{-# LANGUAGE QuantifiedConstraints #-}

-- | A reimplementation of standard monad transformers.

module MonadTransformers where

import Control.Monad
import Control.Applicative

import Data.Function

-- * Reader monad

class Monad m => MonadReader r m where
  ask :: m r
  local :: (r -> r) -> m a -> m a

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Monad m => Functor (ReaderT r m) where
  fmap = liftM

instance Monad m => Applicative (ReaderT r m) where
  pure a = ReaderT \ r -> pure a
  (<*>)  = ap

instance Monad m => Monad (ReaderT r m) where
  m >>= k = ReaderT \ r -> do
    a <- runReaderT m r
    b <- runReaderT (k a) r
    return b

instance Monad m => MonadReader r (ReaderT r m) where
  ask :: ReaderT r m r
  ask = ReaderT \ r -> pure r

  local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
  local f m = ReaderT \ r -> runReaderT m (f r)

-- * State monad

class Monad m => MonadState s m where
  get :: m s
  put :: s -> m ()

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Monad m => Functor (StateT s m) where fmap = liftM

instance Monad m => Applicative (StateT s m) where
  pure a = StateT \ s -> pure (a, s)
  (<*>)  = ap

instance Monad m => Monad (StateT s m) where
  m >>= k = StateT \ s -> do
    (a, s1) <- runStateT m s
    (b, s2) <- runStateT (k a) s1
    return (b, s2)

instance Monad m => MonadState s (StateT s m) where
  get :: StateT s m s
  get = StateT \ s -> pure (s, s)

  put :: s -> StateT s m ()
  put s = StateT \ _ -> pure ((), s)

-- * Liftings

instance MonadState s m => MonadState s (ReaderT r m) where
  get :: ReaderT r m s
  -- get = ReaderT \ r -> get
  get = lift get

  put :: s -> ReaderT r m ()
  -- put s = ReaderT \ r -> put s
  put s = lift $ put s

instance MonadReader r m => MonadReader r (StateT s m) where
  ask :: StateT s m r
  -- ask = StateT \ s -> do
  --   r <- ask
  --   return (r, s)
  ask = lift ask

  local :: (r -> r) -> StateT s m a -> StateT s m a
  local f m = StateT \ s -> do
    local f $ runStateT m s

-- * Monad transformer class

class (forall m. Monad m => Monad (t m)) => MonadTrans t where
  lift :: Monad m => m a -> t m a

instance MonadTrans (ReaderT r) where
  lift :: m a -> ReaderT r m a
  lift m = ReaderT \ r -> m

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift m = StateT \ s -> do
    a <- m
    return (a, s)

-- * Exception

class Monad m => MonadError e m where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

instance Monad m => Functor (ExceptT e m) where fmap = liftM

instance Monad m => Applicative (ExceptT e m) where
  pure a = ExceptT $ pure $ Right a
  (<*>)  = ap

instance Monad m => Monad (ExceptT e m) where
  m >>= k = ExceptT do
    r <- runExceptT m
    case r of
      Left e  -> pure $ Left e
      Right a -> runExceptT $ k a

instance Monad m => MonadError e (ExceptT e m) where
  throwError e = ExceptT $ pure $ Left e
  catchError m h = ExceptT do
    r <- runExceptT m
    case r of
      Left e -> runExceptT $ h e
      Right a -> pure $ Right a

-- ** Liftings

instance MonadTrans (ExceptT e) where
  lift :: Monad m => m a -> ExceptT e m a
  lift m = ExceptT do
    r <- m
    pure $ Right r

instance MonadError e m => MonadError e (ReaderT r m) where
  throwError :: e -> ReaderT r m a
  throwError e = lift $ throwError e

  catchError :: ReaderT r m a -> (e -> ReaderT r m a) -> ReaderT r m a
  catchError m h = ReaderT \ r ->
    catchError (runReaderT m r) \ e ->
      runReaderT (h e) r

instance MonadError e m => MonadError e (StateT e m) where
  throwError e = lift $ throwError e
  catchError m h = StateT \ s -> do
    catchError (runStateT m s) \ e ->
      runStateT (h e) s  -- discards changes to the state

instance MonadState s m => MonadState s (ExceptT e m) where
  get = lift get
  put s = lift $ put s

instance MonadReader r m => MonadReader r (ExceptT e m) where
  ask = lift ask
  local :: (r -> r) -> ExceptT e m a -> ExceptT e m a
  local f m = ExceptT do
    local f $ runExceptT m

-- * What about IO?

-- There is no runIOT :: IOT m a -> m a

class Monad m => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO m = lift $ liftIO m

instance MonadIO m => MonadIO (StateT s m) where
  liftIO m = lift $ liftIO m

instance MonadIO m => MonadIO (ExceptT e m) where
  liftIO m = lift $ liftIO m

-- * Commutation

-- ReaderT adds an input
-- StateT adds an input and an output
-- ExceptT modifies the output

-- ReaderT and StateT commute with themselves and each other

-- ReaderT commutes with ExceptT

-- StateT and ExceptT

-- StateT s m a  ≅ s -> m (a, s)
-- ExceptT e m a ≅ m (Either e a)

-- Variant 1.
--
-- StateT s (ExceptT e m) a
--  ≅ s -> ExceptT e m (a, s)
--  ≅ s -> m (Either e (a, s))
--
-- Discards state changes when exception is thrown.

-- Variant 2.
--
-- ExceptT e (StateT s m) a
-- ≅ StateT s m (Either e a)
-- ≅ s -> m (Either e a, s)
--
-- Keeps state changes when exceptions is thrown.

-- Often we want variant 1 (roll-back state changes when error occurs).
