# Pandoc - `run-command-recipes-pandoc-major-modes-input-formats`

This is list of Emacs' major modes which `run-command-recipe-pandoc` support

This is list:
* `latex-mode`
* `LaTeX-mode`
* `tex-mode`
* `TeX-mode`
* `web-mode`
* `html-mode`
* `markdown-mode`
* `gfm-mode`
* `haskell-mode`
* `rtf-mode`
* `rst-mode`
* `txt2tags-mode`
* `textile-mode`
* `org-mode`
* `csv-mode`
* `json-mode`

For new mode use this code:

```emacs-lisp
(puthash "markdown"
         'markdown-mode
         run-command-recipes-pandoc-major-modes-input-formats)
```
