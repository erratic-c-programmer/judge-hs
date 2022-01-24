import Debug.Trace
import System.Clock
import System.Exit
import System.Process

type Testcase = (String, String)

judgeBinary :: String -> Integer -> [Testcase] -> IO [Bool]
judgeBinary progName timeLimit = mapM (judge progName)
  where
    judge progName t = do
      t0 <- getTime Monotonic
      (e, out, err) <- readProcessWithExitCode progName [] (fst t)
      t1 <- getTime Monotonic
      return $
        e == ExitSuccess
          && out == snd t
          && toNanoSecs (t1 - t0) <= (timeLimit * 1000)
