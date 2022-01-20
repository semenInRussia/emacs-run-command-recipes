# Elisp recipe

## Commands
This repo support 4 commands:
* Complie/ReCompile to Bytes This File

If file was compiled then only compile, otherwise only recompile

* ReComplie to Bytes Current Directory
* Install Cask's Dependicies

Only when root directory has one of `run-command-recipes-elisp-cask-filename` (by default Cask)
*  Run One ERT test
* Run All ERT Tests


## Customization
### Detect Elisp
`run-command-recipes-elisp-modes`. If you very need to support `special mode`, which must-have `run-command-recipes` commands, then use this:
```elisp
(add-to-list 'run-command-recipes-elisp-modes 'special-mode)
```

Or set `run-command-recipes-mode-p-function` to special function, this function must not take arguments and return non-nil value, when current mode needs to have `run-command-recipes-elisp` commands


### Cask
`run-command-recipes-elisp-cask-filename`. If you want to set to "Cask.el" of Main file of Cask, just use this:
```elisp
(setq run-command-recipes-elisp-cask-filename "Cask.el")
```

Or set `run-command-recipes-cask-project-p-function` to special function, this function must not take arguments and return non-nil value, when current mode is have Cask
### ERT
`run-command-recipes-elisp-ert-tests-directory` is name of directory is  which place of ERT tests. Defaults to `test`.

`run-command-recipes-elisp-has-ert-tests-p-function` is function which return t, when current project has ERT tests

