module Local.Local(LocalMachine(..)) where

import Machine
import Local.Executor (executeLocalShellCmdIO, runLocalShellCmdIO)

data LocalMachine = LocalMachine
  deriving (Show, Eq)

instance Machine LocalMachine where
  getSshCredentials _ = Nothing

instance CommandExecutor LocalMachine where
  executeIO LocalMachine command = executeLocalShellCmdIO command
  runIO LocalMachine command = runLocalShellCmdIO command
