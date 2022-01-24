{-# LANGUAGE ScopedTypeVariables #-}

import qualified Control.Monad.Parallel as PMon
import System.Clock (Clock (Monotonic), getTime, toNanoSecs)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

type Testcase = (String, String)

judgeBinary :: String -> Integer -> [Testcase] -> IO [Bool]
judgeBinary progName timeLimit = PMon.mapM (judge progName)
  where
    judge progName t = do
      t0 <- getTime Monotonic
      (e, out, err) <- readProcessWithExitCode progName [] (fst t)
      t1 <- getTime Monotonic
      return $
        e == ExitSuccess
          && out == snd t
          && toNanoSecs (t1 - t0) <= (timeLimit * 1000000)
