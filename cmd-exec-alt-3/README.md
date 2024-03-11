# cmd-exec-alt-1

Alternative 1:

Uses sum type for local and remote machine:

```haskell
data Machine = LocalMachine | RemoteMachine SshCredentials
  deriving (Show, Eq)
```

Then implements `CommandExecutor` typeclass for local and remote machine: 

```haskell
instance CommandExecutor Machine where
  executeCmdIO LocalMachine command = executeLocalShellCmdIO command
  executeCmdIO (RemoteMachine creds) command = executeRemoteShellCmdIO (Ssh creds command)

  runCmdIO LocalMachine command = runLocalShellCmdIO command
  runCmdIO (RemoteMachine creds) command = runRemoteShellCmdIO (Ssh creds command)
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
