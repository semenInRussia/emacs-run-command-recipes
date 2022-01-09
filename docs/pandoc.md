# `run-command`'s Recipe for Pandoc

This cool package can only one thing:
* convert `SOMETHING` to `ANYTHING`, but customization has more.

Who is Work, and how extend its?

1. When call `run-command-recipe-pandoc`, if this one of [this](docs/pandoc/supported-modes.md "List of Pandoc's Input Major Modes") major modes, find pandoc format of current major mode

2. Iterate on formats of `rcr/pandoc-output-formats` (see [list](docs/pandoc/supported-output-formats.md "List of Supported Pandoc's Output Formats")), for each create `run-command` recipe with command:
```shell
pandoc -o foo.md -f org -t markdown foo.org
# here foo.org is buffer in which run run-command, and current mode is org-mode
# and markdown is output format
```
Extension for output file take from [here](docs/pandoc/supported-exts.md "Supported Extensions of Pandoc's Formats")

So if you need to special pandoc's output format see [this](docs/pandoc/supported-output-formats.md "List of Supported Pandoc's Output Formats"), if you need to support of any major mode, then see [this](docs/pandoc/supported-modes.md "List of Pandoc's Input Major Modes"), if you need to special extension of file for pandoc's format visit [this](docs/pandoc/supported-exts.md "Supported Extensions of Pandoc's Formats")
