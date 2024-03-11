module Remote.Executor
  ( executeRemoteShellCmdIO,
    runRemoteShellCmdIO,
  )
where

import Executor (ExitCode, Command)
import Local.Executor
import Ssh

executeRemoteShellCmdIO :: SshCredentials -> Command -> IO (ExitCode, String)
executeRemoteShellCmdIO creds command = executeLocalShellCmdIO (wrapSshCommand creds command)

runRemoteShellCmdIO :: SshCredentials -> Command -> IO ExitCode
runRemoteShellCmdIO creds command = runLocalShellCmdIO (wrapSshCommand creds command)
