{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

{-|
  Several interpreters for a simple arithmetic expression language (inspired by
  the paper Monads for Functional Programming by Philip Wadler)

-}
module Interpr where

import Control.Applicative

-- | Abstract syntax
data Expr
  = Con Int         -- ^ Integer literal.
  | Div Expr Expr   -- ^ Division.

-- | Simple interpreter
interp :: Expr -> Int
interp (Con i)     = i
interp (Div e1 e2) = i1 `div` i2
  where
    i1 = interp e1
    i2 = interp e2

-- | Successful division.
ex_ok    = Div (Div (Con 10) (Con 5)) (Con 2)

-- | Crashing 'interp'!
ex_crash = Div (Con 1) (Con 0)

{-
  --------------
  Error handling
  --------------

  If something fails, the evaluation returns Wrong.
-}

data E a
  = Value a   -- ^ Regular result.
  | Wrong     -- ^ Exception.

interpE :: Expr -> E Int
interpE (Con i)     = Value i
interpE (Div e1 e2) =
  case interpE e1 of
    Wrong -> Wrong
    Value i1 ->
      case interpE e2 of
        Wrong -> Wrong
        Value i2
          | i2 == 0   -> Wrong
          | otherwise -> Value $ i1 `div` i2

runE :: Expr -> IO ()
runE e = case interpE e of
    Wrong   -> putStrLn "Something went wrong!"
    Value i -> putStrLn $ show i


{-
   What if you have many other constructors in the language which you also check
   for errors? Can we automatize that?

   data Expr = ...
             | Mult Expr Expr
             | Exp Expr
-}

instance Monad E where
    return = Value

    Wrong   >>= f = Wrong
    Value a >>= f = f a

-- | Non-proper morphism that makes the computation fail.
abort :: E a
abort = Wrong

m_interpE :: Expr -> E Int
m_interpE (Con i)     = return i
m_interpE (Div e1 e2) =
  m_interpE e1 >>= \ i1 ->
  m_interpE e2 >>= \ i2 ->
  if i2 == 0 then abort
  else return (i1 `div` i2)
    -- No more boring pattern matching!

m_runE :: Expr -> IO ()
m_runE e =
  case m_interpE e of
    Wrong   -> putStrLn "Something went wrong!"
    Value i -> putStrLn $ show i



-- Using the do-notation
m_interpE' :: Expr -> E Int
m_interpE' (Con i)     = return i
m_interpE' (Div e1 e2) = do
   i1 <- m_interpE' e1
   i2 <- m_interpE' e2
   if i2 == 0 then abort
   else return (i1 `div` i2)

m_runE' :: Expr -> IO ()
m_runE' e =
  case m_interpE' e of
    Wrong   -> putStrLn "Something went wrong!"
    Value i -> putStrLn $ show i


{-
  --------------
  Simple logging
  --------------

  We should like to send messages to a log. This information could be later used
  to evaluate some optimizations.
-}


data L a = L a [String]

interpL :: Expr -> L Int
interpL (Con i)     = L i ["-- Hit Con --"]
interpL (Div e1 e2) =
  L (i1 `div` i2) $ concat
    [ [ "-- Hit a Div --" ]
    , [ "** Left recursive call **" ]
    , msgs1
    , [ "** Right recursive call **" ]
    , msgs2
    ]
  where
    L i1 msgs1 = interpL e1
    L i2 msgs2 = interpL e2

runL :: Expr -> IO ()
runL e = do
    putStr "The result is: "
    putStrLn $ show i
    putStrLn "Log:"
    mapM_ putStrLn msgs
  where
    L i msgs = interpL e

{-
   What if you have many other constructors in the language which you want to log
   some information to? Can we automatize that?

   data Expr = ...
             | Mult Expr Expr
             | Exp Expr
-}


instance Monad L where
  return x       = L x []  -- recall the identity laws!
  L x msgs >>= f =
    case f x of
      L y msgs' -> L y (msgs ++ msgs')

-- | Non-proper morphism
msg :: String -> L ()
msg m = L () [m]

m_interpL :: Expr -> L Int

m_interpL (Con i) = do
  msg "-- Hit Con --"
  return i

m_interpL (Div e1 e2) =
  msg "-- Hit a Div --"            >>= \ _  ->
  msg "** Left recursive call **"  >>= \ _  ->
  m_interpL e1                     >>= \ i1 ->
  msg "** Right recursive call **" >>= \ _  ->
  m_interpL e2                     >>= \ i2 ->
  return (i1 `div` i2)

m_runL :: Expr -> IO ()
m_runL e = do
    putStr "Result: "
    putStrLn $ show result
    putStrLn "Messages:"
    mapM_ putStrLn log
  where
    L result log = m_interpL e


-- | Using the do notation
m_interpL' :: Expr -> L Int
m_interpL' (Con i)     = do
   msg "-- Hit Con --"
   return i

m_interpL' (Div e1 e2) = do
   msg "-- Hit a Div --"
   msg "** Left recursive call **"
   i1 <- m_interpL' e1
   msg "** Right recursive call **"
   i2 <- m_interpL' e2
   return (i1 `div` i2)


{--
  --------------------------------------------
  Deep embedding of a monad for error handling
  --------------------------------------------

--}

-- For this, we require existential types.
data E_deep' b
  -- Constructors
  = Return' b
  | Abort'
  -- Combinators
  | forall a. Bind' (E_deep' a) (a -> E_deep' b)

-- The data syntax provided by GADTs is more readable.
data E_deep b where
   -- Constructors
   Return :: b -> E_deep b
   Abort  :: E_deep b
   -- Combinators
   Bind   :: E_deep a -> (a -> E_deep b) -> E_deep b

instance Monad E_deep where
   return = Return
   (>>=)  = Bind

abort_deep :: E_deep a
abort_deep = Abort

interp_deep :: Expr -> E_deep Int
interp_deep (Con i)     = return i
interp_deep (Div e1 e2) = do
   i1 <- interp_deep e1
   i2 <- interp_deep e2
   if i2 == 0 then abort_deep
   else return (i1 `div`i2)

run_deep :: Expr -> IO ()
run_deep e =
  case (to_semantics (interp_deep e)) of
    Wrong   -> putStrLn "Something went wrong!"
    Value i -> putStrLn $ show i
  where
    -- It will not accept E_deep Int -> E Int due to Bind
    to_semantics :: E_deep a -> E a
    to_semantics (Return i) = Value i
    to_semantics Abort      = Wrong
    to_semantics (Bind m f) =
      case to_semantics m of
        Wrong   -> Wrong
        Value i -> to_semantics (f i)


{--
  ---------------
  The state monad
  ---------------
--}

newtype St s a = MkSt (s -> (a, s))

get :: St s s
get = MkSt \ s -> (s, s)

put :: s -> St s ()
put s = MkSt \ _ -> ((), s)

instance Monad (St s) where
  return x     = MkSt \ s -> (x,s)
  MkSt f >>= k = MkSt \ s -> let
      (a, s') = f s
      MkSt ff = k a
    in ff s'

interpS :: Expr -> St Int Int
interpS (Con i)     = return i
interpS (Div e1 e2) = do
   i1 <- interpS e1
   i2 <- interpS e2
   dvs <- get
   put (dvs + 1)
   return (i1 `div` i2)

m_runS :: Expr -> IO ()
m_runS e = do
   putStr "Result: "
   putStrLn $ show result
   putStrLn "Number divisions:"
   putStrLn $ show final_st

   where MkSt f = interpS e
         (result, final_st) = f 0



{--
    The next lines are needed to satisfy the class hierarchy
    Functor < Applicative < Monad.
    You do not need to understand the code until the next lecture.
--}

-- Functor
instance Functor E where
   fmap f m = m >>= \ a -> return (f a)

instance Functor L where
   fmap f m = m >>= \ a -> return (f a)

instance Functor E_deep where
   fmap f m = m >>= \ a -> return (f a)

instance Functor (St s) where
   fmap f m = m >>= \ a -> return (f a)

-- Applicative
instance Applicative E where
   pure  = return
   (<*>) af ax = af >>= \ f -> ax >>= \ x -> pure $ f x

instance Applicative L where
   pure  = return
   (<*>) af ax = af >>= \ f -> ax >>= \ x -> pure $ f x

instance Applicative E_deep where
   pure  = return
   (<*>) af ax = af >>= \ f -> ax >>= \ x -> pure $ f x

instance Applicative (St s) where
   pure  = return
   (<*>) af ax = af >>= \ f -> ax >>= \ x -> pure $ f x
