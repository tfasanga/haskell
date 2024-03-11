module Machine (Machine (..), runLocalShellCmdExecIO) where

import Executor
import Local.Executor
import Ssh
import Data.Maybe (fromMaybe)

data Machine = LocalMachine | RemoteMachine SshCredentials
  deriving (Show, Eq)

instance CommandExecutor Machine where
  executeCmdIO LocalMachine = executeLocalShellCmdExecIO
  executeCmdIO (RemoteMachine creds) = executeRemoteShellCmdExecIO creds

  runCmdIO LocalMachine = runLocalShellCmdExecIO
  runCmdIO (RemoteMachine creds) = runRemoteShellCmdExecIO creds

-- Execute an external command and return its output combined with error
executeLocalShellCmdExecIO :: Command -> ExecIO (ExitCode, String)
executeLocalShellCmdExecIO cmd = do
  executeCmdOpt <- askExecuteCmd
  let exec = fromMaybe executeLocalShellCmdIO executeCmdOpt
  liftIO $ exec cmd

-- Run an external command
runLocalShellCmdExecIO :: String -> ExecIO ExitCode
runLocalShellCmdExecIO cmd = do
  runCmdOpt <- askRunCmd
  let run = fromMaybe runLocalShellCmdIO runCmdOpt
  liftIO $ run cmd

executeRemoteShellCmdExecIO :: SshCredentials -> Command -> ExecIO (ExitCode, String)
executeRemoteShellCmdExecIO creds cmd = executeLocalShellCmdExecIO localCmd
  where
    localCmd = show (Ssh creds cmd)

runRemoteShellCmdExecIO :: SshCredentials -> Command -> ExecIO ExitCode
runRemoteShellCmdExecIO creds cmd = runLocalShellCmdExecIO localCmd
  where
    localCmd = show (Ssh creds cmd)
