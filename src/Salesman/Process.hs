module Salesman.Process
    ( readCommand
    ) where

import System.Process (createProcess, shell, waitForProcess, StdStream(..), CreateProcess(..))
import System.IO (hGetContents)
import System.Exit (ExitCode(..))

readCommand
    :: String
    -> IO String
readCommand cmd = do
    (_, Just hout, _, ph) <- createProcess (shell cmd){std_out=CreatePipe}

    exitCode <- waitForProcess ph
    stdout <- hGetContents hout

    case exitCode of
        ExitSuccess -> return ()
        ExitFailure errCode -> error $ "Command: \"" ++ cmd ++ "\" failed with error code " ++ show errCode

    return stdout
