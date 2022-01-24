{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import qualified Control.Monad.Parallel as PMon
import System.Clock (Clock (Monotonic), getTime, toNanoSecs)
import System.Exit
import System.IO
import System.Directory
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

type Testcase = (String, String)

judgeBinaryTCs :: String -> Integer -> [Testcase] -> IO [Bool]
judgeBinaryTCs progName timeLimit = PMon.mapM (judge progName)
  where
    judge progName t = do
      t0 <- getTime Monotonic
      (e, out, err) <- readProcessWithExitCode progName [] (fst t)
      t1 <- getTime Monotonic
      return $
        e == ExitSuccess
          && out == snd t
          && toNanoSecs (t1 - t0) <= (timeLimit * 1000000)

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
    2
    ...
    N
-}

parseTCDir :: String -> IO [Testcase]
parseTCDir dirName = do return []
