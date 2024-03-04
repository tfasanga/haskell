module Fake.Remote.Executor
  ( executeFakeRemoteShellCmdIO,
    runFakeRemoteShellCmdIO,
  )
where

import Executor
import Ssh

executeFakeRemoteShellCmdIO :: SshCommand -> IO (ExitCode, String)
executeFakeRemoteShellCmdIO ssh = return (ExitSuccess, show ssh)

runFakeRemoteShellCmdIO :: SshCommand -> IO ExitCode
runFakeRemoteShellCmdIO ssh = do
  putStrLn (show ssh)
  return ExitSuccess
