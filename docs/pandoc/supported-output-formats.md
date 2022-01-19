# Cool Variable for Pandoc Recipe `run-command-recipes-pandoc-output-formats`

This is list of strings - Pandoc's output formats which `run-command-recipe-pandoc`support.

Default value:

* `asciidoc`
* `beamer`
* `bibtex`
* `biblatex`
* `commonmark`
* `context`
* `csljson`
* `docbook`
* `docbook5`
* `docx`
* `dokuwiki`
* `epub`
* `epub2`
* `fb2`
* `gfm`
* `haddock`
* `html`
* `html4`
* `icml`
* `ipynb`
* `jats`
* `jira`
* `json`
* `latex`
* `man`
* `markdown`
* `mediawiki`
* `ms`
* `muse`
* `native`
* `odt`
* `opml`
* `opendocument`
* `org`
* `pdf`
* `plain`
* `pptx`
* `rst`
* `rtf`
* `texinfo`
* `textile`
* `slideous`
* `slidy`
* `dzslides`
* `revealjs`
* `s5`
* `tei`
* `xwiki`
* `zimwiki`

For new use this code
```emacs-lisp
(add-to-list 'run-command-recipes-pandoc-output-formats "docx")
```
