# Package emacs-run-command-recipes

This is collection of recipes to [run-command](https://github.com/bard/emacs-run-command "cool package!").

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

## Contribute
All contribution is good, for add recipe to this repository...:

1. [fork](https://docs.github.com/en/get-started/quickstart/fork-a-repo "Documentation from github-docs") this repository
2. Create file with name `run-command-recipes-<recpe-name>.el` instead of `recipe-name` put name of your new recipe
3. Open `run-command-recipes.el` and add `recipe-name` to `rcr/supported-recipes` variable
4. Commit, Commit, Push all to your fork
5. [Pull Request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests "Documentation about Pull Requests from GitHub Docs") to this repository
