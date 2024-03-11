{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Executor
  ( CommandExecutor (..),
    ExecutorContext (..),
    Command,
    ExitCode (..),
    ExecuteCmd,
    RunCmd,
    ExecIO (..),
    ExecReader (..),
    runExec,
    liftIO,
  )
where

import Control.Monad.Reader

type Command = String

type ExecuteCmd = Command -> IO (ExitCode, String)

type RunCmd = Command -> IO ExitCode

data ExitCode
  = ExitSuccess
  | ExitFailure Int
  deriving (Eq, Ord, Read, Show)

class CommandExecutor a where
  executeCmdIO :: a -> Command -> ExecIO (ExitCode, String)
  runCmdIO :: a -> Command -> ExecIO ExitCode

data ExecutorContext = EC (Maybe ExecuteCmd) (Maybe RunCmd)

instance Show ExecutorContext where
  show _ = "ExecutorContext"

newtype ExecIO a = ExecStack {_getReaderT :: ReaderT ExecutorContext IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader ExecutorContext
    )

class MonadReader r m => ExecReader m r where
  askExecuteCmd :: m (Maybe ExecuteCmd)
  askRunCmd :: m (Maybe RunCmd)

instance ExecReader ExecIO ExecutorContext where
  askExecuteCmd = do
    (EC e _) <- ask
    return e

  askRunCmd = do
    (EC _ r) <- ask
    return r

-- private

getReaderT :: ExecIO a -> ReaderT ExecutorContext IO a
getReaderT (ExecStack r) = r

-- public

-- Function that runs the application with Reader returning the ProgramContext
runExec :: ExecIO a -> ExecutorContext -> IO a
runExec = runReaderT . getReaderT
