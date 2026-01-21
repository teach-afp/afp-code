-- | Call-by-need interpreter for simple functional language.

module Interpreter where

import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import System.Exit (exitFailure)

import Lazy.Abs
import Lazy.Print (printTree)

-- | Values.
data Val
  = VInt Integer         -- ^ Numeric value.
  | VFun Ident Exp Env   -- ^ Function value.

-- | Environments map variables to cell pointers.
type Env = Map Ident (IORef Cell)

-- | A cell either contains closure (upon creation) or a value (after first access)
data Cell
  = Clos Exp Env  -- ^ Closure.
  | Val Val       -- ^ Value.

-- | Evaluation of an expression in an environment returns a value.
-- As a side effect, it allocates, reads, and modifies pointers.
eval :: Env -> Exp -> IO Val
eval env = \case
  EInt i       -> return $ VInt i
  EVar x       -> evalVar env x
  EApp e1 e2   -> do
    VFun x e env1 <- eval env e1
    env2 <- extend env1 x $ Clos e2 env
    eval env2 e
  EPlus e1 e2  -> do
    VInt i1 <- eval env e1
    VInt i2 <- eval env e2
    return $ VInt $ i1 + i2
  EAbs x e     -> return $ VFun x e env
  ELet x e1 e2 -> do
    env2 <- extend env x $ Clos e1 env
    eval env2 e2

-- | Allocate the given cell and store its pointer in the given environment.
extend :: Env -> Ident -> Cell -> IO Env
extend env x c = do
  p <- newIORef c
  return $ Map.insert x p env

-- | Lookup a pointer in an enviroment.
-- If it points to a value, return it.
-- If it points to a closure, compute its value and return the value
-- after updating the pointer to the value.
evalVar :: Env -> Ident -> IO Val
evalVar env x = case Map.lookup x env of
  Just p -> readIORef p >>= \case
    Val v -> return v
    Clos e env1 -> do
      v <- eval env1 e
      writeIORef p $ Val v
      return v
  Nothing -> die $ "unbound variable " ++ printTree x

die :: String -> IO a
die msg = do
  putStrLn msg
  exitFailure
