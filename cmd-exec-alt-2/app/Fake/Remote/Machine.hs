module Fake.Remote.Machine (FakeRemoteMachine (..)) where

import Fake.Remote.Executor
import Machine
import Ssh

data FakeRemoteMachine = FakeRemoteMachine SshCredentials
  deriving (Show, Eq)

instance CommandExecutor FakeRemoteMachine where
  executeCmdIO (FakeRemoteMachine creds) = executeFakeRemoteShellCmdIO creds
  runCmdIO (FakeRemoteMachine creds) = runFakeRemoteShellCmdIO creds

instance Machine FakeRemoteMachine where
  getSshCredentials (FakeRemoteMachine creds) = Just creds
