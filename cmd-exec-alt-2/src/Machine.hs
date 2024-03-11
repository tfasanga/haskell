{-# LANGUAGE ExistentialQuantification #-}

module Machine (Machine (..), CommandExecutor (..), SomeMachine (..)) where

import Data.Maybe (isJust)
import Executor (CommandExecutor (..))
import Ssh (SshCredentials)

class (CommandExecutor a) => Machine a where
  getSshCredentials :: a -> Maybe SshCredentials
  isLocal :: a -> Bool
  isLocal = isJust . getSshCredentials

-- https://blog.sumtypeofway.com/posts/existential-haskell.htm
-- SomeMachine is a constructor that takes,
-- for all types a such that a implements Machine,
-- an a value, and returns a value of type SomeMachine,
-- the internal a value of which is no longer visible to the world once it’s been applied
data SomeMachine = forall a. (Machine a) => SomeMachine a

instance Show SomeMachine where
  show (SomeMachine m) = show m

instance CommandExecutor SomeMachine where
  runCmdIO (SomeMachine m) c = runCmdIO m c
  executeCmdIO (SomeMachine m) c = executeCmdIO m c

instance Machine SomeMachine where
  getSshCredentials (SomeMachine m) = getSshCredentials m
