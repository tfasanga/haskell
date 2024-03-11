module Executor
  ( CustomExecutor (..),
    CommandExecutor (..),
    Command,
    ExitCode (..),
    ExecuteCmd,
    RunCmd
  )
where

type Command = String

type ExecuteCmd = Command -> IO (ExitCode, String)

type RunCmd = Command -> IO ExitCode

data ExitCode
  = ExitSuccess
  | ExitFailure Int
  deriving (Eq, Ord, Read, Show)

class CommandExecutor a where
  executeCmdIO :: a -> ExecuteCmd
  runCmdIO :: a -> RunCmd

data CustomExecutor = CustomExecutor
  { executeCmd :: ExecuteCmd,
    runCmd :: RunCmd
  }

instance Show CustomExecutor where
  show _ = "CustomExecutor"
