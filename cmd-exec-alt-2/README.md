# cmd-exec-alt-2

Alternative 2:

Use typeclass for `CommandExecutor` and `Machine`.
`Machine` "extends" `CommandExecutor`.

```haskell
class Show a => CommandExecutor a where
  executeCmdIO :: a -> ExecuteCmd
  runCmdIO :: a -> RunCmd

class (CommandExecutor a) => Machine a where
  getSshCredentials :: a -> Maybe SshCredentials
  isLocal :: a -> Bool
  isLocal = isJust . getSshCredentials
```

Use existential data type `SomeMachine`:

```haskell
data SomeMachine = forall a . (Machine a) => SomeMachine a
```

```haskell
instance Show SomeMachine where
  show (SomeMachine m) = show m

instance CommandExecutor SomeMachine where
  runCmdIO (SomeMachine m) c = runCmdIO m c
  executeCmdIO (SomeMachine m) c = executeCmdIO m c

instance Machine SomeMachine where
  getSshCredentials (SomeMachine m) = getSshCredentials m
```

Local machine implementation:

```haskell
data LocalMachine = LocalMachine
  deriving (Show, Eq)

instance CommandExecutor LocalMachine where
  executeCmdIO LocalMachine = executeLocalShellCmdIO
  runCmdIO LocalMachine = runLocalShellCmdIO

instance Machine LocalMachine where
  getSshCredentials _ = Nothing
```

Remote machine implementation:

```haskell
data RemoteMachine = RemoteMachine SshCredentials
  deriving (Show, Eq)

instance CommandExecutor RemoteMachine where
  executeCmdIO (RemoteMachine creds) = executeRemoteShellCmdIO creds
  runCmdIO (RemoteMachine creds) = runRemoteShellCmdIO creds

instance Machine RemoteMachine where
  getSshCredentials (RemoteMachine creds) = Just creds

```

# Libraries

- https://hackage.haskell.org/package/process-1.6.17.0/docs/System-Process.html

# Building

```shell
stack build
```

# Testing

```shell
stack test
# same as
stack build --test
```
