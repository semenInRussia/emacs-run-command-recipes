#  `run-command` recipe for RUST

## Commands
This recipes has 9 commands:
  * Install Dependecies from Cargo Project
  * Run Cargo Project
  * Build Cargo Project
  * Build Documentation for Cargo Project
  * Run Tests for Cargo Project
  * Update Dependecies for Cargo Project
  * Publish Cargo Project to https://crates.io
  * Compile Rust File with `rustc`
  * Run Rust File with `rustc`

## Customization
### Detect Rust File

For detect rust file, `run-command-recipes` has variable `run-command-recipes-rust-modes`, which contains modes on which this recipes will work, defaults to:
  * `rust-mode`
  * `rustic-mode`

Also has variable `run-command-recipes-rust-mode-p-funtion` which contains function, which return t, when current file is rust file (and this without arguments!), or nil, when use default implementation.

### Detect Cargo Project

Variable `run-command-recipes-rust-cargo-filename` is file's name of file, which if put in root directory, then say that current directional is cargo directory

Variable `run-command-recipes-rust-cargo-project-p-function` as `run-command-recipes-rust-mode-p-funtion`, but for project.

### Compile Command

`rust-command-recipes-rust-rustc-compile-command` is string, which is shell command compiling current rust file via `rustc`. Defaults to:

```shell
  rustc "%s"
```
Here %s is filename of rust file.

`rust-command-recipes-rust-rustc-run-command` is string, which is shell command run current rust file via `rustc`.  Defaults to:


```shell
  rustc "%s" && "%s"
```

Here first %s is filename of rust file, second is filename of binary file.
