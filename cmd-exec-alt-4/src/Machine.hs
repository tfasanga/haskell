{-# LANGUAGE NamedFieldPuns #-}

module Machine (MachineContext (..), isLocalMachine, Machine (..)) where

import Executor
import Local.Executor
import Remote.Executor
import Ssh

data Machine = LocalMachine | RemoteMachine SshCredentials
  deriving (Show, Eq)

data MachineContext = MachineContext
  { executor :: Maybe CustomExecutor,
    machine :: Machine
  }

instance Show MachineContext where
  show MachineContext {machine = LocalMachine} = "localhost"
  show MachineContext {machine = m} = show m

instance CommandExecutor MachineContext where
  executeCmdIO MachineContext {executor, machine} command = case machine of
    RemoteMachine creds -> executeRemoteCmdIO executor creds command
    LocalMachine -> executeLocalCmdIO executor command

  runCmdIO MachineContext {executor, machine} command = case machine of
    RemoteMachine creds -> runRemoteCmdIO executor creds command
    LocalMachine -> runLocalCmdIO executor command

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

isLocalMachine :: MachineContext -> Bool
isLocalMachine MachineContext {machine} = case machine of
  LocalMachine -> True
  _ -> False
