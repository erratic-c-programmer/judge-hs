import Debug.Trace
import System.Clock
import System.Exit
import System.Process

type Testcase = (String, String)

judgeBinary :: String -> Integer -> [Testcase] -> IO [Bool]
judgeBinary progName timeLimit = mapM (judge progName)
  where
    judge progName t = do
      t0 <- getTime Realtime
      let t0_s = sec t0
      let t0_n = nsec t0
      (e, out, err) <- readProcessWithExitCode progName [] (fst t)
      t1 <- getTime Realtime
      let t1_s = sec t1
      let t1_n = nsec t1
      return $
        e == ExitSuccess
          && out == snd t
          && ((t1_s - t0_s) * 1000 + (t1_n - t0_n) `div` 1000000) <= fromIntegral timeLimit
