module Machines
  ( Machines (..),
    MachineName (..),
    getMachines,
    getMachine,
  )
where

import Machine
import Ssh

data MachineName
  = DevelopmentMachine
  | BuildMachine
  | TestMachine
  deriving (Show)

data Machines = Machines
  { development :: Machine,
    build :: Machine,
    test :: Machine
  }
  deriving (Show)

getMachines :: Machines
getMachines =
  Machines
    { development = createDevelopmentMachine,
      build = createBuildMachine,
      test = createTestMachine
    }

getMachine :: Machines -> MachineName -> Machine
getMachine Machines {development = m} DevelopmentMachine = m
getMachine Machines {build = m} BuildMachine = m
getMachine Machines {test = m} TestMachine = m

createDevelopmentMachine :: Machine
createDevelopmentMachine = LocalMachine

createBuildMachine :: Machine
createBuildMachine =
  RemoteMachine
    SshCredentials
      { username = "user1",
        hostname = "host1",
        port = Just 22,
        privateKeyFile = Nothing
      }

createTestMachine :: Machine
createTestMachine = LocalMachine
