module Fake.Local.Executor
  ( executeFakeLocalShellCmdIO,
    runFakeLocalShellCmdIO,
  )
where

import Executor

executeFakeLocalShellCmdIO :: String -> IO (ExitCode, String)
executeFakeLocalShellCmdIO cmd = return (ExitSuccess, cmd)

runFakeLocalShellCmdIO :: String -> IO ExitCode
runFakeLocalShellCmdIO cmd = do
  putStrLn cmd
  return ExitSuccess
