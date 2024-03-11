# cmd-exec-alt-4

Alternative 4:

Define `CustomExecutor` data type with custom `ExecuteCmd` and `RunCmd`:

```haskell
data CustomExecutor = CustomExecutor
  { executeCmd :: ExecuteCmd,
    runCmd :: RunCmd
  }
```

Uses sum type for local and remote machine:

```haskell
data Machine = LocalMachine | RemoteMachine SshCredentials
  deriving (Show, Eq)
```

Define `MachineContext` data type:

```haskell
data MachineContext = MachineContext
  { executor :: Maybe CustomExecutor,
    machine :: Machine
  }
```

Define `CommandExecutor` typeclass with commands (side effects) running in `IO`:

```haskell
class CommandExecutor a where
  executeCmdIO :: a -> Command -> IO (ExitCode, String)
  runCmdIO :: a -> Command -> IO ExitCod
```

Implement `CommandExecutor` for `MachineContext` data type:

```haskell
instance CommandExecutor MachineContext where
  executeCmdIO MachineContext {executor, machine} command = case machine of
    RemoteMachine creds -> executeRemoteCmdIO executor creds command
    LocalMachine -> executeLocalCmdIO executor command

  runCmdIO MachineContext {executor, machine} command = case machine of
    RemoteMachine creds -> runRemoteCmdIO executor creds command
    LocalMachine -> runLocalCmdIO executor command
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
