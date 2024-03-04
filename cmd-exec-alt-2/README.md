# cmd-exec-alt-2

Alternative 2:

Use typeclass for `CommandExecutor` and `Machine`.

```haskell
class Show a => CommandExecutor a where
  executeIO :: a -> Command -> IO (ExitCode, String)
  runIO :: a -> Command -> IO ExitCode

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
  runIO (SomeMachine m) c = runIO m c
  executeIO (SomeMachine m) c = executeIO m c

instance Machine SomeMachine where
  getSshCredentials (SomeMachine m) = getSshCredentials m
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
