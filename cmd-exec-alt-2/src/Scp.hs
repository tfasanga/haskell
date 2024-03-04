{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Scp (scp, ScpFilePath, scpFp) where

import Core.Common
import Executor
import Local.Executor
import Machine
import Ssh

data ScpFilePath = ScpFp SomeMachine FilePath

instance Show ScpFilePath where
  show = showScpFp

-- | Constructor
scpFp :: SomeMachine -> FilePath -> ScpFilePath
scpFp = ScpFp

-- | scp command
scp :: ScpFilePath -> ScpFilePath -> IO ExitCode
scp src dst = case buildScpCmd src dst of
  (Just cmd) -> runLocalShellCmdIO cmd
  Nothing -> skipScpIO

skipScpIO :: IO ExitCode
skipScpIO = do
  println "skipping scp, both source and destination machines are local"
  return ExitSuccess

buildScpCmd :: ScpFilePath -> ScpFilePath -> Maybe String
buildScpCmd src@(ScpFp (SomeMachine m1) _) dst@(ScpFp (SomeMachine m2) _) = if
  | isLocal m1 && isLocal m2 -> Nothing
  | otherwise -> Just ("scp " <> show src <> " " <> show dst)

showScpFp :: ScpFilePath -> String
showScpFp (ScpFp sm@(SomeMachine m) fp) = if
  | isLocal m -> fp
  | otherwise -> sshCredentialsToScpUri sm <> fp

sshCredentialsToScpUri :: SomeMachine -> String
sshCredentialsToScpUri (SomeMachine m) = case (getSshCredentials m) of
  Nothing -> ""
  (Just SshCredentials{username, hostname, port}) ->  "scp://" <> username <> "@" <> hostname <> showPortOpt port

showPortOpt :: Maybe PortNum -> String
showPortOpt Nothing = ""
showPortOpt (Just port) = ":" <> show port
