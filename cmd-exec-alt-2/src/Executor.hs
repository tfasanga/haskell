module Executor
  ( CommandExecutor (..),
    Command,
    ExecuteCmd,
    RunCmd,
    ExitCode (..),
  )
where

data ExitCode
  = ExitSuccess
  | ExitFailure Int
  deriving (Eq, Ord, Read, Show)

type Command = String

type ExecuteCmd = Command -> IO (ExitCode, String)

type RunCmd = Command -> IO ExitCode

class Show a => CommandExecutor a where
  executeCmdIO :: a -> ExecuteCmd
  runCmdIO :: a -> RunCmd
