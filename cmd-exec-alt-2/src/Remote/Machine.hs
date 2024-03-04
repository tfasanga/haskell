module Remote.Machine(RemoteMachine(..)) where

import Machine
import Ssh (SshCredentials(..), SshCommand(..))
import Remote.Executor (executeRemoteShellCmdIO, runRemoteShellCmdIO)

data RemoteMachine = RemoteMachine SshCredentials
  deriving (Show, Eq)

instance Machine RemoteMachine where
  getSshCredentials (RemoteMachine creds) = Just creds

instance CommandExecutor RemoteMachine where
  executeIO (RemoteMachine creds) command = executeRemoteShellCmdIO (Ssh creds command)
  runIO (RemoteMachine creds) command = runRemoteShellCmdIO (Ssh creds command)
