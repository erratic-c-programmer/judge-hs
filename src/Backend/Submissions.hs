{-# LANGUAGE ScopedTypeVariables #-}

-- |
--  Everything to do with testcases, running submissions, judging code etc.
module Backend.Submissions where

import Control.Exception (SomeException, catch)
import Control.Monad (zipWithM)
import Control.Monad.Extra (concatMapM)
import qualified Control.Monad.Parallel as PMon
import Data.List.Extra (groupBy, isSuffixOf, sortOn, stripSuffix, chunksOf)
import Data.Maybe (mapMaybe)
import System.Clock (Clock (Monotonic), getTime, toNanoSecs)
import System.Directory
import System.Exit (ExitCode (ExitSuccess))
import qualified System.IO.Strict as StrictIO
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

-- | Alias for a tuple representing a testcase: (input, output).
type Testcase = (String, String)

-- | Datatype representing some runnable thing.
data Runnable = SchemeFile String | PythonFile String | GenericBinary String

-- |
--  Parses a directory of testcases into a @[[`Testcase`]]@. Takes a path to a directory and
--  returns a list of subtasks, each containing seperate testcases.
--
--  Testcase directories should have @\<subtask\>.\<testcase\>.{in,out}@, where @subtask@ and
--  @testcase@ are natural numbers. All input\/output files without corresponding output\/input
--  files will be ignored, as well as all other files in different formats in the filename.
readTCDir :: String -> IO [[Testcase]]
readTCDir dirName = do
  tcfs <- catch (listDirectory dirName) (\(_ :: SomeException) -> return [])
  -- not the most efficient (amortised quadratic), but it doesn't really matter
  let nosufs =
        let xs = mapMaybe (stripSuffix ".in") tcfs
         in filter (\x -> x ++ ".out" `elem` tcfs) xs
  let ins = map (\x -> dirName ++ "/" ++ x ++ ".in") nosufs
  let outs = map (\x -> dirName ++ "/" ++ x ++ ".out") nosufs

  map (map snd) . groupBy getSubTask . sortOn fst . zip ins <$> zipWithM ftoTC ins outs
  where
    ftoTC i o = (,) <$> StrictIO.readFile i <*> StrictIO.readFile o
    getSubTask (s1, _) (s2, _) = takeWhile (/= '.') s1 == takeWhile (/= '.') s2

-- |
--  Pattern-matches on a `Runnable` to determine how to run it. Takes a `Runnable` and returns
--  a function that, when given a String, runs the `Runnable` with the given String as an stdin.
runSubmission :: Runnable -> [String] -> (String -> IO (ExitCode, String, String))
runSubmission r options =
  case r of
    SchemeFile fp -> readProcessWithExitCode "scheme" $ options ++ [fp]
    PythonFile fp -> readProcessWithExitCode "python" $ options ++ [fp]
    GenericBinary fp -> readProcessWithExitCode fp options

-- | Evaluates a submission with testcases. Runs a submission on testcases in parallel, and returns
--   a Boolean result per testcase.
evalSubmission :: Runnable -> [String] -> Int -> Int -> [[Testcase]] -> IO [[Bool]]
evalSubmission prog options maxBatch timeLimit tcs = concatMapM (PMon.mapM $ PMon.mapM $ judge prog) (chunksOf maxBatch tcs)
  where
    judge prog t = do
      t0 <- getTime Monotonic
      (e, out, err) <- runSubmission prog options (fst t)
      t1 <- getTime Monotonic
      return $
        e == ExitSuccess
          && out == snd t
          && toNanoSecs (t1 - t0) <= fromIntegral timeLimit * 1000000
