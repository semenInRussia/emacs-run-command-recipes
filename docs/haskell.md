# `run-command`'s Recipe for Pandoc
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [`run-command`'s Recipe for Pandoc](#run-commands-recipe-for-pandoc)
    - [Commands](#commands)
    - [Requirements](#requirements)
    - [Customization](#customization)

<!-- markdown-toc end -->


## Commands
Have 2 commands:

* Run Haskell file by Context
* Run Haskell Project with Stack


## Requirements
This is cool recipe needs to requirements in [haskell-mode](https://github.com/haskell/haskell-mode "Haskell Mode"), without `haskell-mode` this recipe drop command "Run Haskell file by context"

## Customization

This recipe has variable `run-command-recipes-haskell-modes`. This is list of all `haskell-modes`. Defautls to 1 mode `haskell-mode`. You can add own mode to this variable

Also this recipe has `run-command-recipes-haskell-complie-function`. This is function which not take arguments and comlie and run Haskell file of current buffer. Defautls to `haskell-complie`.

