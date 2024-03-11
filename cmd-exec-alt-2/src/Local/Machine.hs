module Local.Machine(LocalMachine(..)) where

import Machine
import Local.Executor (executeLocalShellCmdIO, runLocalShellCmdIO)

data LocalMachine = LocalMachine
  deriving (Show, Eq)

instance Machine LocalMachine where
  getSshCredentials _ = Nothing

instance CommandExecutor LocalMachine where
  executeCmdIO LocalMachine = executeLocalShellCmdIO
  runCmdIO LocalMachine = runLocalShellCmdIO
