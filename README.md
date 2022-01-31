# Package emacs-run-command-recipes
[![MELPA](https://melpa.org/packages/run-command-recipes-badge.svg)](https://melpa.org/#/run-command-recipes)
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Package emacs-run-command-recipes](#package-emacs-run-command-recipes)
    - [Usage](#usage)
    - [Contribute](#contribute)

<!-- markdown-toc end -->

This is collection of recipes to [run-command](https://github.com/bard/emacs-run-command "cool package!").

## Install
Melpa has `emacs-run-command-recipes` called as `run-command-recipes`, so if you use `use-package`, then just paste followed code to you config:

```elisp
(use-package run-command-recipes
    :ensure t
    :after (run-command)
    :init
    (run-command-recipes-use-all))
```
Or manually:
```elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-run-command-recipes")
                                        ; here load path to source
(require 'run-command-recipes)
(run-command-recipes-use-all)
```
## Usage
If you need to use all supported language, just put this to your code:
```emacs-lisp
(run-command-recipes-use-all)
```

If you need to use only special languages, just put this:
```emacs-lisp
(run-command-recipes-use latex
                         pandoc)
```
Also Instead of `LaTeX` and `pandoc` you can use something from this:

* `latex` ([link on support](docs/latex.md "hi"))
* `pandoc`([link on support](docs/pandoc.md "hi"))
* `haskell`([link on support](docs/haskell.md "Haskell Rocks!"))
* `elisp`([link on support](docs/elisp.md "Elisp is LISP"))

## Contribute
All contribution is good, for add recipe to this repository...:

1. [fork](https://docs.github.com/en/get-started/quickstart/fork-a-repo "Documentation from github-docs") this repository
2. Create file with name `run-command-recipes-<recipe-name>.el` instead of `recipe-name` put name of your new recipe
3. Create in its function recipe for `run-command` with name `run-command-recipes-<recipe-name>`
4. Open `run-command-recipes.el` and add `recipe-name` to `run-command-recipes-supported-recipes` variable (this is requres for `run-command-recipes-use-all`)
5. Suggest use `run-command-recipes-project`, this package has function `run-command-recipes-project-root` which return root root of current project
6. Commit, Commit, Push all to your fork
7. [Pull Request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests "Documentation about Pull Requests from GitHub Docs") to this repository
