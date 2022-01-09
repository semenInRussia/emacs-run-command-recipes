# `run-command` Recipe for LaTeX

I am use only `pdflatex`, so you have little customization, you can customize `rcr/pdflatex-command` var, by default `pdflatex` `run-command-recipes` ignore able handle errors to user, because default compiltaion mode of Emacs dont't able to handle stdin input of user, but you can customize this when set var `rcr/pdflatex-command`.

`rcr/pdflatax-command` is string which take 2 format (`%s`), first is (`--output-dir`, flag of `pdflatex`), second filename, please wrap this quotes, this is important.

`rcr/pdflatax-command` defaults to:

```shell
pdflatex -interaction nonstopmode -file-line-error --output-directory "%s" "%s"
```
