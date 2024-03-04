module Alternative.Remote where

import Alternative.Machine
import Ssh (SshCredentials(..), SshCommand(..))
import Remote.Executor (executeRemoteShellCmdIO, runRemoteShellCmdIO)

data RemoteMachine = RemoteMachine SshCredentials
  deriving (Show, Eq)

instance Machine RemoteMachine where
  executeCmdIO machine command = executeIO machine command
  runCmdIO machine command = runIO machine command
  getHostname (RemoteMachine SshCredentials {hostname = s}) = s
  getUsername (RemoteMachine SshCredentials {username = s}) = s

instance CommandExecutor RemoteMachine where
  executeIO (RemoteMachine creds) command = executeRemoteShellCmdIO (Ssh creds command)
  runIO (RemoteMachine creds) command = runRemoteShellCmdIO (Ssh creds command)
