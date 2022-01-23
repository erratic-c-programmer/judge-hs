import System.Process
import System.Exit
import System.TimeIt

type Testcase = (String, String)

judgeBinary :: String -> Float -> [Testcase] -> IO [Bool]
judgeBinary progName timeLimit = mapM (judge progName)
  where
    judge progName t = do
      (e, out, err) <- readProcessWithExitCode progName [] (fst t)
      return $ e == ExitSuccess && out == snd t
