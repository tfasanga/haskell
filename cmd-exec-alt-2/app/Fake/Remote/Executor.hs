module Fake.Remote.Executor
  ( executeFakeRemoteShellCmdIO,
    runFakeRemoteShellCmdIO,
  )
where

import Executor
import Ssh

executeFakeRemoteShellCmdIO :: SshCredentials -> ExecuteCmd
executeFakeRemoteShellCmdIO creds cmd = return (ExitSuccess, actualCmd)
  where
    actualCmd = show (Ssh creds cmd)

runFakeRemoteShellCmdIO :: SshCredentials -> RunCmd
runFakeRemoteShellCmdIO creds cmd = do
  putStrLn actualCmd
  return ExitSuccess
  where
    actualCmd = show (Ssh creds cmd)
