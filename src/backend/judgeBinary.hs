import Debug.Trace
import System.Clock
import System.Exit
import System.Process

type Testcase = (String, String)

judgeBinary :: String -> Int -> [Testcase] -> IO [Bool]
judgeBinary progName timeLimit = mapM (judge progName)
  where
    judge progName t = do
      TimeSpec t0_s t0_n <- getTime Realtime
      (e, out, err) <- readProcessWithExitCode progName [] (fst t)
      TimeSpec t1_s t1_n <- getTime Realtime
      return $
        e == ExitSuccess
          && out == snd t
          && ((t1_s - t0_s) * 1000 + (t1_n - t0_n) `div` 1000000) <= fromIntegral timeLimit
