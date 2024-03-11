module Remote.Executor
  ( executeRemoteShellCmdIO,
    runRemoteShellCmdIO,
  )
where

import Executor (ExecuteCmd, RunCmd)
import Local.Executor
import Ssh

executeRemoteShellCmdIO :: SshCredentials -> ExecuteCmd
executeRemoteShellCmdIO creds cmd = executeLocalShellCmdIO localCmd
  where
    localCmd = show (Ssh creds cmd)

runRemoteShellCmdIO :: SshCredentials -> RunCmd
runRemoteShellCmdIO creds cmd = runLocalShellCmdIO localCmd
  where
    localCmd = show (Ssh creds cmd)
