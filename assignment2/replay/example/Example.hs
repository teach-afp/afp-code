module Main where

import Control.Monad (void)
import Data.Time     (getCurrentTime, diffUTCTime)
import System.IO     (BufferMode( NoBuffering ),  hSetBuffering, stdout)

import Replay

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  void $ running example

running :: Replay String String a -> IO a
running prog = play emptyTrace
 where
  play t = do
    r <- run prog t -- this is the same prog every time!
    case r of
      Left (q,t2) -> do
        putStr ("Question: " ++ q ++ " ")
        r <- getLine
        play (addAnswer t2 r)
      Right x -> return x

example :: Replay String String Int
example = do
  t0 <- io getCurrentTime
  io (putStrLn "Hello!")
  age <- ask "What is your age?"
  io (putStrLn ("You are " ++ age))
  name <- ask "What is your name?"
  io (putStrLn (name ++ " is " ++ age ++ " years old"))
  t1 <- io getCurrentTime
  io (putStrLn ("Total time: " ++ show (diffUTCTime t1 t0)))
  return (read age)
