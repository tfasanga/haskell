# cmd-exec-alt-3

Alternative 3:

Uses sum type for local and remote machine:

```haskell
data Machine = LocalMachine | RemoteMachine SshCredentials
  deriving (Show, Eq)
```

The `ExecutorContext` in `ExecIO` can be used to define custom `ExecuteCmd` and/or `RunCmd` functions.

Use `ExecIO` reader monad with `ExecutorContext`:

```haskell
data ExecutorContext = EC (Maybe ExecuteCmd) (Maybe RunCmd)

newtype ExecIO a = ExecStack {_getReaderT :: ReaderT ExecutorContext IO a}
```

Define `CommandExecutor` typeclass with commands (side effects) running in `ExecIO` (instead of `IO`):

```haskell
class CommandExecutor a where
  executeCmdIO :: a -> Command -> ExecIO (ExitCode, String)
  runCmdIO :: a -> Command -> ExecIO ExitCode
```

Then implements `CommandExecutor` typeclass for local and remote machine: 

```haskell
instance CommandExecutor Machine where
  executeCmdIO LocalMachine = executeLocalShellCmdExecIO
  executeCmdIO (RemoteMachine creds) = executeRemoteShellCmdExecIO creds

  runCmdIO LocalMachine = runLocalShellCmdExecIO
  runCmdIO (RemoteMachine creds) = runRemoteShellCmdExecIO creds
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
