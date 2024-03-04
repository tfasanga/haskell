module Alternative.Machine (Machine (..), CommandExecutor (..)) where

import Executor (Command, ExitCode)

class CommandExecutor a where
  executeIO :: a -> Command -> IO (ExitCode, String)
  runIO :: a -> Command -> IO ExitCode

class CommandExecutor a => Machine a where
  executeCmdIO :: a -> Command -> IO (ExitCode, String)
  runCmdIO :: a -> Command -> IO ExitCode
  getHostname :: a -> String
  getUsername :: a -> String
