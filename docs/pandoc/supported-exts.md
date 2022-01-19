# Panoc Variable `run-command-recipes-pandoc-formats-and-extensions`

This is list of extensions of files for Pandoc's input formats, which have no equal format name and extension (for example `html` input Pandoc's input format have extension equal to format name `.html`).

Defaults have this formats (this is only formats which has different with format's name extension):
* `asciidoc`
* `context`
* `docbook`
* `markdown`
* `ms`
* `latex`
* `texinfo`
* `mediawiki`
* `biblatex`

For add new use this code:

```emacs-lisp
(puthash "markdown"
         "md"
         run-command-recipes-pandoc-formats-and-extensions)
```
