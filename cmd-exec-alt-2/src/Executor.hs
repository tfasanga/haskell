module Executor
  ( CommandExecutor (..),
    Command,
    ExecuteCmd,
    RunCmd,
    ExitCode (..),
  )
where

type Command = String

type ExecuteCmd = Command -> IO (ExitCode, String)

type RunCmd = Command -> IO ExitCode

data ExitCode
  = ExitSuccess
  | ExitFailure Int
  deriving (Eq, Ord, Read, Show)

class Show a => CommandExecutor a where
  executeCmdIO :: a -> ExecuteCmd
  runCmdIO :: a -> RunCmd
