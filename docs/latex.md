# `run-command` Recipe for LaTeX
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [`run-command` Recipe for LaTeX](#run-command-recipe-for-latex)
    - [Commands](#commands)
    - [Customization](#customization)

<!-- markdown-toc end -->

## Commands
This cool recipe can only one thing:
* Convert to PDF with pdflatex

## Customization

I am use only `pdflatex`, so you have little customization, you can customize `run-command-recipes-latex-command` var, by default `pdflatex` `run-command-recipes` ignore able handle errors to user, because default compiltaion mode of Emacs dont't able to handle stdin input of user, but you can customize this when set var `run-command-recipes-latex-command`.

`run-command-recipes-latex-command` is string which take 2 format (`%s`), first is (`--output-dir`, flag of `pdflatex`), second filename, please wrap this quotes, this is important.

`run-command-recipes-latex-command` defaults to:

```shell
pdflatex -interaction nonstopmode -file-line-error --output-directory "%s" "%s"
```
