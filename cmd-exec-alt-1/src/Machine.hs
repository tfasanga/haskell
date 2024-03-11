module Machine (Machine (..)) where

import Executor
import Local.Executor
import Remote.Executor
import Ssh

data Machine = LocalMachine | RemoteMachine SshCredentials
  deriving (Show, Eq)

instance CommandExecutor Machine where
  executeCmdIO LocalMachine = executeLocalShellCmdIO
  executeCmdIO (RemoteMachine creds) = executeRemoteShellCmdIO creds

  runCmdIO LocalMachine = runLocalShellCmdIO
  runCmdIO (RemoteMachine creds) = runRemoteShellCmdIO creds
