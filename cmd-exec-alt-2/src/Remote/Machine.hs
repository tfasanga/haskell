module Remote.Machine (RemoteMachine (..)) where

import Machine
import Remote.Executor (executeRemoteShellCmdIO, runRemoteShellCmdIO)
import Ssh (SshCredentials (..))

data RemoteMachine = RemoteMachine SshCredentials
  deriving (Show, Eq)

instance Machine RemoteMachine where
  getSshCredentials (RemoteMachine creds) = Just creds

instance CommandExecutor RemoteMachine where
  executeCmdIO (RemoteMachine creds) = executeRemoteShellCmdIO creds
  runCmdIO (RemoteMachine creds) = runRemoteShellCmdIO creds
