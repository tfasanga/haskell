{-# LANGUAGE MultiWayIf #-}

module Rsync
  ( RsyncSource (..),
    RsyncDestination (..),
    RsyncOption (..),
    rsync,
    buildRsyncCmd,
  )
where

import Core.Common
import Executor
import Local.Executor
import Machine
import Ssh
import System.FilePath ((</>))

type RootDir = FilePath

type RelativeDir = FilePath

data RsyncSource = RsyncSrc RootDir RelativeDir
  deriving (Eq)

instance Show RsyncSource where
  show (RsyncSrc rootDir relativeDir) = rootDir </> "." </> relativeDir

data RsyncDestination = RsyncDst SomeMachine FilePath

instance Show RsyncDestination where
  show (RsyncDst (SomeMachine m) fp) = case (getSshCredentials m) of
    Nothing -> fp
    (Just SshCredentials{username = u, hostname = h}) -> u <> "@" <> h <> ":" <> fp

data RsyncOption = Exclude String
  deriving (Eq)

instance Show RsyncOption where
  show (Exclude s) = "--exclude " <> s

-- TODO Custom SSH port:
-- scp -e 'ssh -p 2222'
-- scp -P 2222

rsync :: RsyncSource -> RsyncDestination -> [RsyncOption] -> IO ExitCode
rsync src dst opts = case buildRsyncCmd src dst opts of
  (Just cmd) -> runLocalShellCmdIO cmd
  Nothing -> skipRsyncIO

buildRsyncCmd :: RsyncSource -> RsyncDestination -> [RsyncOption] -> Maybe String
buildRsyncCmd src dst@(RsyncDst (SomeMachine m) _) opts = if
  | isLocal m -> Nothing
  | otherwise -> Just ("rsync" <> showDstOpts dst <> showDefaultOpts <> showOpts opts <> " " <> show src <> " " <> show dst)

showDstOpts :: RsyncDestination -> String
showDstOpts (RsyncDst (SomeMachine m) _) = case (getSshCredentials m) of
  Nothing -> ""
  (Just creds) -> showPortOpt creds <> showKeyOpt creds

showPortOpt :: SshCredentials -> String
showPortOpt (SshCredentials _ _ (Just port) _) = " -P " <> show port
showPortOpt _ = ""

showKeyOpt :: SshCredentials -> String
showKeyOpt (SshCredentials _ _ _ (Just keyFile)) = " -i " <> keyFile
showKeyOpt _ = ""

showOpts :: [RsyncOption] -> String
showOpts [] = ""
showOpts opts = " " <> join list
  where
    join = unwords
    list = map show opts

showDefaultOpts :: String
showDefaultOpts = " --archive --relative --delete --verbose -o"

skipRsyncIO :: IO ExitCode
skipRsyncIO = do
  println "skipping rsync, both source and destination machines are local"
  return ExitSuccess
