module Fake.Local.Machine (FakeLocalMachine (..)) where

import Fake.Local.Executor
import Machine

data FakeLocalMachine = FakeLocalMachine
  deriving (Show, Eq)

instance CommandExecutor FakeLocalMachine where
  executeCmdIO _ command = executeFakeLocalShellCmdIO command
  runCmdIO _ command = runFakeLocalShellCmdIO command

instance Machine FakeLocalMachine where
  getSshCredentials _ = Nothing
