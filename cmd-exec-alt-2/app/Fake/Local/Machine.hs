module Fake.Local.Machine (FakeLocalMachine (..)) where

import Fake.Local.Executor
import Machine

data FakeLocalMachine = FakeLocalMachine
  deriving (Show, Eq)

instance CommandExecutor FakeLocalMachine where
  executeIO _ command = executeFakeLocalShellCmdIO command
  runIO _ command = runFakeLocalShellCmdIO command

instance Machine FakeLocalMachine where
  getSshCredentials _ = Nothing
