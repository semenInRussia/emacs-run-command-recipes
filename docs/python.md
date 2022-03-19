# `run-command`'s Recipe for Python


## Commands
Have 4 commands:

   * Just Run Current Python File
   * Run Current Python File Interactively (with flag `-i`) (you can find its [here](https://docs.python.org/3/using/cmdline.html "Link to Offical Python Doc"))
   * Run `pytest` in Current Pytest File
   * Run All Tests in Current Project via Pytest

## Customization

### Detect Python File

Variable `run-command-recipes-python-modes` is list of major modes, in which this amazing recipe of `run-command` will work.

Variable `run-command-recipes-python-mode-p-function` Function (predicate) non-taking arguments, returning t if in current file can work `run-command-recipes-python`.
When set to nil, use default implementation

### Detect Python's Tests
`run-command-recipes-python-test-filename` is regexp which match on Python  test files.

`run-command-recipes-python-test-buffer-p-function` is predicate which get t, when current file is test's file of Python, nil when no. If set to nil, then use default implementation.

`run-command-recipes-python-tests-dirs` list of dirictories names on which will work `pytest`. Defautls to:
  * tests

### Shell Commands

`run-command-recipes-python-run-command` is common shell command, which just run current Python file.

`run-command-recipes-python-pytest-file-command` is shell command which run `pytest` for current single test file.

`run-command-recipes-python-interactively-run-command` is shell command which run `python` on this file INTERACTIVELY.

