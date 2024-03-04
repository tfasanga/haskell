# cmd-exec-alt-2

Alternative 2:

Use typeclass for Machine.

```haskell
class CommandExecutor a where
executeIO :: a -> Command -> IO (ExitCode, String)
runIO :: a -> Command -> IO ExitCode

class CommandExecutor a => Machine a where
executeCmdIO :: a -> Command -> IO (ExitCode, String)
runCmdIO :: a -> Command -> IO ExitCode
getHostname :: a -> String
getUsername :: a -> String
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
