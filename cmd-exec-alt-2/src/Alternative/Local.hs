module Alternative.Local where

import Alternative.Machine
import Local.Executor (executeLocalShellCmdIO, runLocalShellCmdIO)

data LocalMachine = LocalMachine
  deriving (Show, Eq)

instance Machine LocalMachine where
  executeCmdIO machine command = executeIO machine command
  runCmdIO machine command = runIO machine command
  getHostname _ = "localhost"
  getUsername _ = ""

instance CommandExecutor LocalMachine where
  executeIO LocalMachine command = executeLocalShellCmdIO command
  runIO LocalMachine command = runLocalShellCmdIO command
