# `run-command`'s Recipe for Pandoc
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [`run-command`'s Recipe for Pandoc](#run-commands-recipe-for-pandoc)
    - [Commands](#commands)
    - [Who is Work? (require if you need to customize)](#who-is-work-require-if-you-need-to-customize)
    - [Customization](#customization)

<!-- markdown-toc end -->

## Commands
This cool package can only one thing:
* convert `SOMETHING` to `ANYTHING`, but customization has more.

## Who is Work? (require if you need to customize)
Who is Work, and how extend its?

1. When call `run-command-recipe-pandoc`, if this one of [this](pandoc/supported-modes.md "List of Pandoc's Input Major Modes") major modes, find pandoc format of current major mode

2. Iterate on formats of `run-command-recipes-pandoc-output-formats` (see [list](pandoc/supported-output-formats.md "List of Supported Pandoc's Output Formats")), for each create `run-command` recipe with command:
```shell
pandoc -o foo.md -f org -t markdown foo.org
# here foo.org is buffer in which run run-command, and current mode is org-mode
# and markdown is output format
```

## Customization

Extension for output file take from [here](pandoc/supported-exts.md "Supported Extensions of Pandoc's Formats")

So if you need to special pandoc's output format see [this](pandoc/supported-output-formats.md "List of Supported Pandoc's Output Formats"), if you need to support of any major mode, then see [this](pandoc/supported-modes.md "List of Pandoc's Input Major Modes"), if you need to special extension of file for pandoc's format visit [this](pandoc/supported-exts.md "Supported Extensions of Pandoc's Formats")
