{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Control.Monad (zipWithM)
import qualified Control.Monad.Parallel as PMon
import Data.List.Extra (isSuffixOf, sortBy, stripSuffix)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import System.Clock (Clock (Monotonic), getTime, toNanoSecs)
import System.Directory
import System.Exit
import qualified System.IO.Strict as StrictIO
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

type Testcase = (String, String)

judgeBinaryTCs :: String -> Integer -> [[Testcase]] -> IO [[Bool]]
judgeBinaryTCs progName timeLimit = PMon.mapM $ PMon.mapM $ judge progName
  where
    judge progName t = do
      t0 <- getTime Monotonic
      (e, out, err) <- readProcessWithExitCode progName [] (fst t)
      t1 <- getTime Monotonic
      return $
        e == ExitSuccess
          && out == snd t
          && toNanoSecs (t1 - t0) <= timeLimit * 1000000

{-
  testcase directory structure:
  [problem]
    1.1.in
    1.2.out
    2.1.in
    2.2.out
    ...
    i.N.in
    i.N.out
-}

readTCDir :: String -> IO [Testcase]
readTCDir dirName = do
  tcfs <- catch (listDirectory dirName) (\(_ :: SomeException) -> return [])
  -- not the most efficient (amortised quadratic), but it doesn't really matter
  let nosufs =
        let xs = mapMaybe (stripSuffix ".in") tcfs
         in filter (\x -> x ++ ".out" `elem` tcfs) xs
  let ins = map (\x -> dirName ++ "/" ++ x ++ ".in") nosufs
  let outs = map (\x -> dirName ++ "/" ++ x ++ ".out") nosufs

  zipWithM ftoTC ins outs
  where
    ftoTC i o = (,) <$> StrictIO.readFile i <*> StrictIO.readFile o
