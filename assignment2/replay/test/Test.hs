module Main where

import Data.IORef  (newIORef, readIORef, modifyIORef)
import System.Exit (exitSuccess, exitFailure)

import Replay

-- | Runs the test suite for the replay library
main :: IO ()
main = do
  results <- runTests
  if and results
    then exitSuccess
    else exitFailure

-- | Programs are parameterised over a 'tick' action.
--   Questions are () and answers are integers.
type Program = IO () -> Replay () Int Int

-- | A result is a pair of the final result of the program
--   and the number of 'ticks' executed.
type Result  = (Int, Int)
type Input   = [Int]

-- | A test case collects a program and a list of answers together
--   with its expected result.
data TestCase = TestCase
  { testName    :: String
  , testInput   :: Input
  , testResult  :: Result
  , testProgram :: Program
  }

-- | Running a program.
runProgram :: Program -> Input -> IO Result
runProgram p inp = do
    counter <- newIORef 0
    let tick = modifyIORef counter (+1)
    x <- play (p tick) emptyTrace inp
    n <- readIORef counter
    return (x, n)
  where
    play prog t inp = do
      r <- run prog t
      case r of
        Right x      -> case inp of
          []       -> return x
          _        -> error "too many inputs"
        Left (_, t') -> case inp of
          []       -> error "too few inputs"
          a : inp' -> play prog (addAnswer t' a) inp'

-- | Checking a test case. Compares expected and actual results.
checkTestCase :: TestCase -> IO Bool
checkTestCase (TestCase name i r p) = do
  putStr $ name ++ ": "
  r' <- runProgram p i
  let success = r == r'
  if success
    then putStrLn "ok"
    else putStrLn $ unwords [ "FAIL: expected", show r, "instead of", show r' ]
  return success

-- | List of interesting test cases.
testCases :: [TestCase]
testCases =
  [ TestCase
    { testName    = "example"
    , testInput   = [59,65]
    , testResult  = (2^59 * 3^65, 6)
    , testProgram = \ tick -> do
        io tick        -- getCurrentTime
        io tick        -- putStrLn "Hello!"
        age  <- ask () -- "What is your age?"
        io tick        -- putStrLn ("You are " ++ age)
        name <- ask () -- "What is your name?"
        io tick        -- putStrLn (name ++ " is " ++ age ++ " years old")
        io tick        -- getCurrentTime
        io tick        -- putStrLn ("Total time: " ++ show (diffUTCTime t1 t0))
        return (2^age * 3^name)
    }
  , TestCase
    { testName    = "test1"
    , testInput   = [3,4]
    , testResult  = (8, 1)
    , testProgram = \ tick -> do
        io tick
        a <- ask () -- should be 3
        b <- io (return 1)
        c <- ask () -- should be 4
        return (a + b + c)
    }
  ]

-- | Running all the test cases.
runTests :: IO [Bool]
runTests = mapM checkTestCase testCases
