{-# LANGUAGE ExistentialQuantification #-}

module Machine (Machine (..), CommandExecutor (..), SomeMachine(..)) where

import Executor (Command, ExitCode)
import Ssh (SshCredentials)
import Data.Maybe (isJust)

class Show a => CommandExecutor a where
  executeIO :: a -> Command -> IO (ExitCode, String)
  runIO :: a -> Command -> IO ExitCode

class (CommandExecutor a) => Machine a where
  getSshCredentials :: a -> Maybe SshCredentials
  isLocal :: a -> Bool
  isLocal = isJust . getSshCredentials


-- https://blog.sumtypeofway.com/posts/existential-haskell.htm
-- SomeMachine is a constructor that takes,
-- for all types a such that a implements Machine,
-- an a value, and returns a value of type SomeMachine,
-- the internal a value of which is no longer visible to the world once it’s been applied
data SomeMachine = forall a . (Machine a) => SomeMachine a

instance Show SomeMachine where
  show (SomeMachine m) = show m

instance CommandExecutor SomeMachine where
  runIO (SomeMachine m) c = runIO m c
  executeIO (SomeMachine m) c = executeIO m c

instance Machine SomeMachine where
  getSshCredentials (SomeMachine m) = getSshCredentials m
