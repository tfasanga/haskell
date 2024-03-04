module Machines
  ( Machines (..),
    MachineName (..),
    getMachine,
    getMachines,
  )
where

import Fake.Local.Machine
import Fake.Remote.Machine
import Local.Machine
import Machine
import Remote.Machine
import Ssh

data MachineName
  = DevelopmentMachine
  | BuildMachine
  | TestLocalMachine
  | TestRemoteMachine
  deriving (Show)

data Machines = Machines
  { development :: SomeMachine,
    build :: SomeMachine,
    testLocal :: SomeMachine,
    testRemote :: SomeMachine
  }

getMachines :: Machines
getMachines =
  Machines
    { development = SomeMachine LocalMachine,
      build = SomeMachine getBuildMachine,
      testLocal = SomeMachine FakeLocalMachine,
      testRemote = SomeMachine getFakeRemoteMachine
    }

getMachine :: Machines -> MachineName -> SomeMachine
getMachine Machines {development = m} DevelopmentMachine = m
getMachine Machines {build = m} BuildMachine = m
getMachine Machines {testLocal = m} TestLocalMachine = m
getMachine Machines {testRemote = m} TestRemoteMachine = m

getBuildMachine :: RemoteMachine
getBuildMachine = RemoteMachine creds
  where
    creds =
      SshCredentials
        { username = "user1",
          hostname = "host1",
          port = Just 22,
          privateKeyFile = Nothing
        }

getFakeRemoteMachine :: FakeRemoteMachine
getFakeRemoteMachine = FakeRemoteMachine creds
  where
    creds =
      SshCredentials
        { username = "user2",
          hostname = "host2",
          port = Just 830,
          privateKeyFile = Nothing
        }
