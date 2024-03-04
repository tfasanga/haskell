module Machines
  ( Machines (..),
    MachineName (..),
    getMachine,
    getMachines,
  )
where

import Fake
import Local.Local
import Machine
import Remote.Remote
import Ssh

data MachineName
  = DevelopmentMachine
  | BuildMachine
  | TestMachine
  deriving (Show)

data Machines = Machines
  { development :: SomeMachine,
    build :: SomeMachine,
    test :: SomeMachine
  }

getMachines :: Machines
getMachines =
  Machines
    { development = SomeMachine LocalMachine,
      build = SomeMachine getBuildMachine,
      test = SomeMachine FakeMachine
    }

getMachine :: Machines -> MachineName -> SomeMachine
getMachine Machines {development = m} DevelopmentMachine = m
getMachine Machines {build = m} BuildMachine = m
getMachine Machines {test = m} TestMachine = m

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
