{-|
  Several interpreters for a simple arithmetic expression language (inspired by
  the paper Monads for Functional Programming by Philip Wadler)

  Note: This code works with GHC 7.8 and lower versions.
-}
module Interpr where


-- | Abstract syntax
data Expr = Con Int | Div Expr Expr

-- | Simple interpreter
interp :: Expr -> Int
interp (Con i)     = i
interp (Div e1 e2) = i1 `div` i2
    where i1 = interp e1
          i2 = interp e2

-- | Succesful division
ex_ok    = Div (Con 10) (Con 5)

-- | Crashing!
ex_crash = Div (Con 1) (Con 0)

{-
  --------------
  Error handling
  --------------

  If something fails, the evaluation returns Nothing.
-}

data E a = Value a | Wrong

interpE :: Expr -> E Int
interpE (Con i)     = Value i
interpE (Div e1 e2) = case maybe_i1 of
                        Wrong -> Wrong
                        Value i1 -> case maybe_i2 of
                                    Wrong -> Wrong
                                    Value i2 -> Value $ i1 `div` i2
    where maybe_i1 = interpE e1
          maybe_i2 = interpE e2

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

m_interpE :: Expr -> E Int
m_interpE (Con i)     = return i
m_interpE (Div e1 e2) = m_interpE e1 >>= \i1 ->
                          m_interpE e2 >>= \i2 ->
                            return (i1 `div` i2)
                        -- No more borring pattern matching!

-- Using the do-notation
m_interpE' :: Expr -> E Int
m_interpE' (Con i)     = return i
m_interpE' (Div e1 e2) =
   do i1 <- m_interpE' e1
      i2 <- m_interpE' e2
      return (i1 `div` i2)

{-
  --------------------
  Debuggin information
  --------------------

  We should like to know how many division are performed. This information could
  be later used to evaluate some optimizations.
-}


data L a = L (a, Int)

interpL :: Expr -> L Int
interpL (Con i)      = L (i, 0)
interpL (Div e1 e2)  = L (i1 `div` i2, divs1 + divs2)
   where L (i1, divs1) = interpL e1
         L (i2, divs2) = interpL e2

{-
   What if you have many other constructors in the language which you also need
   to add up the number of divisions? Can we automatize that?

   data Expr = ...
             | Mult Expr Expr
             | Exp Expr
-}

instance Monad L where
  return x = L (x, 0)
  L (x,ds) >>= f = case f x of
                           L (y, ds') -> L (y, ds+ds')


m_interpL :: Expr -> L Int
m_interpL (Con i)     = return i
m_interpL (Div e1 e2) = m_interpL e1 >>= \i1 ->
                          m_interpL e2 >>= \i2 ->
                            return (i1 `div` i2)
                        -- The same code as error handling!
                        -- Different effects though!


-- Using the do-notation
m_interpL' :: Expr -> L Int
m_interpL' (Con i)     = return i
m_interpL' (Div e1 e2) =
   do i1 <- m_interpL' e1
      i2 <- m_interpL' e2
      return (i1 `div` i2)
