{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module MT
  (  MyStateT (MyStateT)
   , get
   , put
   , runST
   , runSTInt
   , MyEnvT (MyEnvT)
   , ask
   , local
   , runEnv
   , MyExceptT
   , throwError
   , catchError
   , runErr
   , MT (lift)
  )
where

import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Applicative
import Control.Monad.Reader(MonadReader(ask, local, reader))


-- | Monad transformers support lifting of existing operations
class Monad m1 => MT m1 m2 | m2 -> m1 where
  lift :: m1 a -> m2 a


{--------------------------
  State monad transformer
----------------------------}

-- | State monad transformer
data MyStateT s m a = MyStateT { st :: s -> m (a,s) }

instance Monad m => Functor (MyStateT s m) where
     fmap = liftM

instance (Monad m) => Applicative (MyStateT s m) where
     pure  = return
     (<*>) = ap

-- | (MyStateT s m a) is a monad
instance Monad m => Monad (MyStateT s m) where
    return x          = MyStateT $ \s -> return (x,s)
    MyStateT  f >>= k = MyStateT $ \s -> f s >>= \(a, s') -> st (k a) s'

-- | (MyStateT s m a) is a state monad
instance Monad m => MonadState s (MyStateT s m) where
   get     = MyStateT (\s -> return (s,s))
   put s'  = MyStateT (\_s -> return ((),s'))


-- | Run function
runST :: Monad m => MyStateT s m a -> s -> m a
runST (MyStateT m) = \s -> m s >>= \(a,_) -> return a

runSTInt :: Monad m => MyStateT s m a -> s -> m (a,s)
runSTInt (MyStateT m) =  m

-- | (MyStateT s) is a monad transformer
instance Monad m => MT m (MyStateT s m) where
    lift m = MyStateT $ \s -> m >>= \a -> return (a,s)


{--------------------------
  Error monad transformer
----------------------------}

-- | Error monad transformer
data MyExceptT e m a = MyExceptT { except :: m (Either e a) }

instance Monad m => Functor (MyExceptT e m) where
    fmap = liftM

instance Monad m => Applicative (MyExceptT e m) where
    pure  = return
    (<*>) = ap

-- | (ExceptT e m a) is a monad
instance Monad m => Monad (MyExceptT e m) where
   return              = MyExceptT . return . Right
   (MyExceptT m) >>= k = MyExceptT $ m >>= \a -> case a of
                                                    Left e  -> return (Left e)
                                                    Right x -> except (k x)

-- | (ExceptT e m a) is a error monad
instance Monad m => MonadError e (MyExceptT e m) where
   throwError              = MyExceptT . return . Left
   catchError (MyExceptT m)
              h            = MyExceptT $ do a <- m
                                            case a of
                                                Left e  -> except (h e)
                                                Right x -> return (Right x)

-- | (ExceptT e) is a monad transformer
instance Monad m => MT m (MyExceptT e m) where
    lift m = MyExceptT $ m >>= return . Right

-- | Run function
runErr :: MyExceptT e m a -> m (Either e a)
runErr (MyExceptT m) = m


{--------------------------------
  Environment monad transformer
----------------------------------}

-- | Environment monad transformer
data MyEnvT r m a = MyEnvT {env :: r -> m a}

instance Monad m => Functor (MyEnvT r m) where
     fmap = liftM

instance (Monad m) => Applicative (MyEnvT r m) where
     pure  = return
     (<*>) = ap

-- | (MyEnvT r m a) is a monad
instance Monad m => Monad (MyEnvT r m) where
   return x          = MyEnvT $ \_r -> return x
   (MyEnvT m) >>= k  = MyEnvT $ \r -> m r >>= flip (env . k) r

-- | (MyEnvT r m a) is an environment monad
instance Monad m => MonadReader r (MyEnvT r m) where
    ask       = MyEnvT return
    local f m = MyEnvT $ env m . f

-- | (MyEnvT s) is a monad transformer
instance Monad m => MT m (MyEnvT r m) where
    lift m = MyEnvT $ const m

runEnv :: MyEnvT r m a -> r -> m a
runEnv (MyEnvT m) = m
