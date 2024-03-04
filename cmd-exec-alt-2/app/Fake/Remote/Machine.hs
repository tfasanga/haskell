module Fake.Remote.Machine (FakeRemoteMachine (..)) where

import Fake.Remote.Executor
import Machine
import Ssh

data FakeRemoteMachine = FakeRemoteMachine SshCredentials
  deriving (Show, Eq)

instance CommandExecutor FakeRemoteMachine where
  executeIO (FakeRemoteMachine creds) command = executeFakeRemoteShellCmdIO (Ssh creds command)
  runIO (FakeRemoteMachine creds) command = runFakeRemoteShellCmdIO (Ssh creds command)

instance Machine FakeRemoteMachine where
  getSshCredentials (FakeRemoteMachine creds) = Just creds
