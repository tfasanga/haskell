module Fake(FakeMachine(..)) where

import Machine

data FakeMachine = FakeMachine
  deriving (Show, Eq)

instance CommandExecutor FakeMachine where
  executeIO _ _command = undefined
  runIO _ _command = undefined

instance Machine FakeMachine where
  getSshCredentials _ = Nothing
