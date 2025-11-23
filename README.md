# Haskell Command Execution Framework

A learning project demonstrating advanced Haskell concepts through a type-safe command execution abstraction layer for local and remote machines.

## Project Overview

This repository contains four alternative implementations (`cmd-exec-alt-1` through `cmd-exec-alt-4`) of a command execution framework that provides a unified interface for running shell commands on both local and remote machines via SSH. Each alternative explores different architectural patterns and design approaches in Haskell.

## Core Features

### Command Execution Abstraction
- **Unified Interface**: Execute commands on local or remote machines through a single `CommandExecutor` typeclass
- **Type Safety**: Strong typing with custom `ExitCode`, `Command`, `ExecuteCmd`, and `RunCmd` types
- **Pluggable Executors**: Support for custom command executors via `CustomExecutor` with fallback to default implementations
- **Machine Context**: Encapsulates execution environment with `MachineContext` data type

### Remote Operations
- **SSH Integration**: Full SSH support with configurable credentials (username, hostname, port, private key)
- **SCP File Transfer**: Secure copy protocol implementation for file transfers between local and remote machines
- **Rsync Support**: Advanced synchronization with options for excludes, archive mode, and automatic port/key configuration
- **Command Wrapping**: Automatic SSH command wrapping with proper quote escaping

### Architecture Patterns
- **Typeclass-based Design**: `CommandExecutor` typeclass provides polymorphic command execution
- **Smart Constructors**: Type-safe construction of SSH credentials, SCP file paths, and Rsync operations
- **Error Handling**: Result types and error handling with `Common.Err` and `Common.Result` modules
- **Local/Remote Abstraction**: Transparent handling of local vs remote execution contexts

## Project Structure

Each alternative implementation (`cmd-exec-alt-1` through `cmd-exec-alt-4`) contains:

```
cmd-exec-alt-X/
├── app/
│   ├── Main.hs              # CLI application entry point
│   ├── Machines.hs          # Machine configuration
│   └── Common/              # Common utilities (Err, Result)
├── src/
│   ├── Executor.hs          # Core command executor abstraction
│   ├── Machine.hs           # Machine and context definitions
│   ├── Ssh.hs               # SSH credential and command handling
│   ├── Scp.hs               # Secure copy functionality
│   ├── Rsync.hs             # Rsync synchronization
│   ├── Local/Executor.hs   # Local command execution
│   ├── Remote/Executor.hs  # Remote SSH command execution
│   └── Core/                # Core utilities and error types
└── test/unit/               # Unit tests (QuickCheck, Tasty)
```

## Key Modules

### Executor
Defines the core command execution types and typeclass:
- `CommandExecutor` typeclass with `executeCmdIO` and `runCmdIO` methods
- `ExecuteCmd`: Command -> IO (ExitCode, String) - runs and captures output
- `RunCmd`: Command -> IO ExitCode - runs and returns exit code only
- `CustomExecutor` data type for dependency injection

### Machine
Machine abstraction with local and remote variants:
- `Machine`: `LocalMachine | RemoteMachine SshCredentials`
- `MachineContext`: Wraps machine with optional custom executor
- Instance of `CommandExecutor` for transparent execution routing

### SSH/SCP/Rsync
Remote operations with full configuration support:
- SSH: Credentials with username, hostname, port, and private key file
- SCP: URI-based file path representation for local/remote files
- Rsync: Source/destination with options, automatic SSH parameter passing

## Technical Highlights

### Haskell Language Features
- **Record Syntax**: Named fields with `NamedFieldPuns`, `DisambiguateRecordFields`, `DuplicateRecordFields`
- **Type Classes**: Polymorphic interfaces for command execution
- **Phantom Types**: Type-level safety for SSH credentials and file paths
- **Pattern Matching**: Exhaustive case analysis for machine types
- **Show Instances**: Custom string representations for commands and credentials

### Dependencies
- `base`: Core Haskell library
- `process`: System process execution
- `directory`, `filepath`: File system operations
- `split`: String splitting utilities
- `mtl`: Monad transformer library
- **Testing**: `tasty`, `tasty-hunit`, `tasty-quickcheck`, QuickCheck

### GHC Extensions
- `DisambiguateRecordFields`, `DuplicateRecordFields`
- `RecordWildCards`
- `GeneralizedNewtypeDeriving`
- `NoFieldSelectors`

## Learning Objectives

This project demonstrates:
1. **Type-driven Design**: Using Haskell's type system to enforce correctness
2. **Abstraction Patterns**: Typeclasses, data types, and smart constructors
3. **Error Handling**: Result types and IO error management
4. **Module Organization**: Clean separation of concerns across modules
5. **Testing**: Property-based testing with QuickCheck and unit tests with Tasty
6. **Real-world Integration**: SSH, SCP, and Rsync command-line tool wrappers

## Alternative Implementations

The four alternatives (`cmd-exec-alt-1` through `cmd-exec-alt-4`) explore different:
- Error handling strategies
- Type abstraction levels
- Module organization patterns
- Executor customization approaches
- Testing methodologies

Each can be built and run independently to compare approaches.

## Use Cases

- **Configuration Management**: Deploy and execute commands across multiple machines
- **Build Automation**: Run builds on remote servers
- **File Synchronization**: Sync files between local and remote environments
- **Testing Infrastructure**: Execute tests on different machine configurations
- **DevOps Scripting**: Type-safe deployment and management scripts

## Author

Tibor Fasanga (tibor@fasanga.com)

## License

BSD-3-Clause (2024)
