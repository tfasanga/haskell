module Local.Machine(LocalMachine(..)) where

import Machine
import Local.Executor (executeLocalShellCmdIO, runLocalShellCmdIO)

data LocalMachine = LocalMachine
  deriving (Show, Eq)

instance CommandExecutor LocalMachine where
  executeCmdIO LocalMachine = executeLocalShellCmdIO
  runCmdIO LocalMachine = runLocalShellCmdIO

instance Machine LocalMachine where
  getSshCredentials _ = Nothing

