{-# LANGUAGE NamedFieldPuns #-}

module Machine (Machine (..), isLocalMachine) where

import Data.Maybe (isNothing)
import Executor
import Local.Executor
import Remote.Executor
import Ssh

data Machine = CustomMachine
  { executor :: Maybe CustomExecutor,
    sshCredentials :: Maybe SshCredentials
  }

instance Show Machine where
  show CustomMachine {sshCredentials = Nothing} = "localhost"
  show CustomMachine {sshCredentials = creds} = show creds

instance CommandExecutor Machine where
  executeCmdIO CustomMachine {executor, sshCredentials} command = case sshCredentials of
    Just creds -> executeRemoteCmdIO executor creds command
    Nothing -> executeLocalCmdIO executor command

  runCmdIO CustomMachine {executor, sshCredentials} command = case sshCredentials of
    Just creds -> runRemoteCmdIO executor creds command
    Nothing -> runLocalCmdIO executor command

executeLocalCmdIO :: Maybe CustomExecutor -> ExecuteCmd
executeLocalCmdIO (Just CustomExecutor {executeCmd}) = executeCmd
executeLocalCmdIO Nothing = executeLocalShellCmdIO

runLocalCmdIO :: Maybe CustomExecutor -> RunCmd
runLocalCmdIO (Just CustomExecutor {runCmd}) = runCmd
runLocalCmdIO Nothing = runLocalShellCmdIO

executeRemoteCmdIO :: Maybe CustomExecutor -> SshCredentials -> ExecuteCmd
executeRemoteCmdIO (Just CustomExecutor {executeCmd}) _ = executeCmd
executeRemoteCmdIO Nothing creds = executeRemoteShellCmdIO creds

runRemoteCmdIO :: Maybe CustomExecutor -> SshCredentials -> RunCmd
runRemoteCmdIO (Just CustomExecutor {runCmd}) _ = runCmd
runRemoteCmdIO Nothing creds = runRemoteShellCmdIO creds


isLocalMachine :: Machine -> Bool
isLocalMachine CustomMachine {sshCredentials} = isNothing sshCredentials
