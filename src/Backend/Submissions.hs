{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
--  Everything to do with running submissions, judging code etc.
module Backend.Submissions where

import Backend.Problems
import Control.Monad.Extra (concatMapM)
import qualified Control.Monad.Parallel as PMon
import Data.List.Extra (chunksOf)
import System.Clock (Clock (Monotonic), getTime, toNanoSecs)
import System.Exit (ExitCode (ExitSuccess))
import System.Process.Text (readProcessWithExitCode)
import qualified Data.Text as T

-- | Datatype representing some runnable thing.
data Runnable = SchemeFile String | PythonFile String | GenericBinary String

-- | Pattern-matches on a `Runnable` to determine how to run it. Takes a `Runnable` and returns
--   a function that, when given a String, runs the `Runnable` with the given String as an stdin.
runSubmission :: Runnable -> [String] -> (T.Text -> IO (ExitCode, T.Text, T.Text))
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
    judge :: Runnable -> Testcase -> IO Bool
    judge prog t = do
      t0 <- getTime Monotonic
      (e, out, err) <- runSubmission prog options (fst t)
      t1 <- getTime Monotonic
      return $
        e == ExitSuccess
          && out == snd t
          && toNanoSecs (t1 - t0) <= fromIntegral timeLimit * 1000000
