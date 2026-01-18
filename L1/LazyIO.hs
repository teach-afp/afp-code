-- | Example for pitfalls of lazy I/O.

module LazyIO where

import System.IO
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

-- * File closed too early
------------------------------------------------------------------------

readFile1 :: FilePath -> IO String
readFile1 f = do
  h <- openFile f ReadMode
  contents <- hGetContents h
  hClose h
  return contents

head100 :: IO ()
head100 = do
  contents <- readFile1 "LazyIO.hs"
  putStrLn $ take 100 contents

main = head100

-- * File open for too long
------------------------------------------------------------------------
-- In the wild: <https://github.com/haskell/tar/issues/100>

readManyFiles :: Int -> IO [String]
readManyFiles n = mapM (\ i -> readFile ("files/" ++ show i ++ ".txt")) [1..n]
  -- files/2558.txt: openFile: resource exhausted (Too many open files)

manyFiles :: IO ()
manyFiles = do
  xs <- readManyFiles 10000
  print $ sum $ map read xs

readManyFilesT :: Int -> IO [Text]
readManyFilesT n = mapM (\ i -> Text.readFile ("files/" ++ show i ++ ".txt")) [1..n]
  -- files/2558.txt: openFile: resource exhausted (Too many open files)

manyFilesT :: IO ()
manyFilesT = do
  xs <- readManyFilesT 10000
  print $ sum $ map (read . Text.unpack) xs

-- main = manyFilesT
