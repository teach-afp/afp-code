{-# LANGUAGE GADTs #-}
module Resumption where


{- Literature:

- A library for Removing Cache-based Attacks in Concurrent Information Flow Systems
  by Buiras et al. (Section 3 and 4 of the paper)
  http://www.cse.chalmers.se/~russo/publications_files/tgc2013-extended.pdf
-}



-- DSL for concurrency 
{- 
data Conc m a 
Monad (Conc m)

atom :: Monad m => m a -> Conc m a
fork :: Monad m => Conc m () -> Conc m ()

run :: [Conc m ()] -> m ()
-}


data Conc m a where 
    Done :: a -> Conc m a
    Atom :: m (Conc m a) -> Conc m a 
    Fork :: Conc m () -> Conc m a -> Conc m a 


instance Functor (Conc m) where
    fmap = undefined 

instance Applicative (Conc m) where
    pure = undefined 
    (<*>) = undefined 

instance Monad m => Monad (Conc m) where
    return = Done  
    (Done a)     >>= f = f a   
    
    (Atom m)     >>= f = Atom $ do 
                           next <- m
                           return $ next >>= f 

    (Fork cf m)  >>= f = Fork cf (m >>= f) 

atom :: Monad m => m a -> Conc m a
atom m = Atom $ do 
            a <- m 
            return $ Done a 

fork :: Monad m => Conc m () -> Conc m ()
fork m = Fork m (return ())

run :: Monad m => [Conc m ()] -> m ()
run [] = return ()
run (c : cs) = do 
    case c of 
        Done ()   -> run cs 
        Fork cf m -> run (cs ++ [cf,m])
        Atom m    -> do 
            next <- m 
            run (cs ++ [next])

type Code a = IO a


thread1 :: Conc IO () 
thread1 = do 
    atom $ putStrLn "Hello"
    atom $ putStrLn "Bye"
    return () 

thread2 :: Conc IO () 
thread2 = do 
    atom $ putStrLn "Hola" >> putStrLn "Hola2!"
    atom $ putStrLn "Adios"
    return ()

test = run [thread1,thread2]



