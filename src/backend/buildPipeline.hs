import System.Process
import System.Exit

type BatchStatus = (ExitCode, String, String)
type BatchProg = (String, [String], String)

runBatchProg :: BatchProg -> IO BatchStatus
runBatchProg (x, y, z) = readProcessWithExitCode x y z

-- we are lazy people
-- and we shall leave build stuffs up to Make :)

buildFile :: String -> IO BatchStatus
buildFile f = runBatchProg ("make", [f], "")
