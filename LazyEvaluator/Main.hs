module Main where

import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )

import Lazy.Abs   ( Program(Prog) )
import Lazy.Lex   ( Token, mkPosToken )
import Lazy.Par   ( pProgram, myLexer )

import Interpreter( Val(VInt, VFun), eval )

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> FilePath -> IO ()
runFile v f = readFile f >>= run v

run :: Verbosity -> String -> IO ()
run v s =
  case pProgram ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      mapM_ (putStrV v . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right (Prog e) -> do
      eval mempty e >>= \case
        VInt i -> print i
        VFun{} -> putStrLn "<<function>>"
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Interpret stdin verbosely."
    , "  (files)         Interpret content of files verbosely."
    , "  -s (files)      Silent mode. Interpret content of files silently."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run 2
    "-s":fs    -> mapM_ (runFile 0) fs
    fs         -> mapM_ (runFile 2) fs
