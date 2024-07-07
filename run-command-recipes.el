;;; run-command-recipes.el --- Start pack of recipes to `run-command' inside Emacs using only one command. -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (dash "2.18.0") (f "0.20.0") (run-command "1.0.0"))
;; Keywords: extensions, run-command
;; Homepage: https://github.com/semenInRussia/emacs-run-command-recipes

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is collection of recipes to `run-command'.

;; I found `run-command' package of Bard very useful, the great idea
;; that you have one command to run ALL compile-like commands which
;; have a relation to your file is very awesome.  Also it uses main
;; power of Emacs: extensibility.  `run-command' not only let you
;; ability to customization, even better you can choose the commands
;; that will be visible on your own and even control WHEN, HOW, WHERE.
;; WOW!  But without the initial start kit is useless, you can call
;; command but it do nothing.  I am trying to provide for you an OK
;; starting pack of these recipes, for all languages in which I
;; sometimes found that need in some help to run it.

;; If you're going to use all supported languages, just put the
;; following code into your Emacs configuration:
;;
;;   (run-command-recipes-use-all)
;;
;; If you're going to use only some special languages, just put the following one

;;
;;   (run-command-recipes-use 'latex
;;                            'pandoc)
;;

;; Also, Instead of LaTeX and pandoc, you can be interested in other
;; "build systems" (or how to call it?)

;; - c
;; - cpp
;; - csharp
;; - elisp
;; - elixir
;; - go
;; - haskell
;; - java
;; - latex
;; - make
;; - pandoc
;; - python
;; - racket
;; - rust

;;; Code:

(require 'run-command)

(require 'dash)
(require 's)

(defgroup run-command-recipes nil
  "Start pack of recipes to `run-command' inside Emacs using only one command."
  :group 'tools
  :link '(url-link
          :tag "GitHub"
          "https://github.com/semenInRussia/emacs-run-command-recipes"))

(defcustom run-command-recipes-supported-recipes
  '(latex pandoc haskell elisp rust python c cpp csharp java racket make go elixir)
  "List of recipes names that are supported by `run-command-recipes'."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defun run-command-recipes-use-all ()
  "Use all recipes for `run-command' supported by `run-command-recipes'.

Note that the list of supported recipes you can see either in variable
`run-command-recipes-supported-recipes' of at the top of this file or
in README.md at the following URL:

https://github.com/semenInRussia/emacs-run-command-recipes"
  (apply 'run-command-recipes-use
         run-command-recipes-supported-recipes))

(defun run-command-recipes-use (&rest recipes)
  "Use RECIPES for `run-command' that are supported by `run-command-recipes'.

Each of RECIPES is identifier to recipe without `run-command-recipes-'
prefix.

For example if you need in c and cpp recipes, use the following snippet

(run-command-recipes-use \\='c \\='cpp)"
  (--each recipes (run-command-recipes-use-one it)))

;;; Internal:

(defun run-command-recipes--use-one (recipe)
  "Use the RECIPE for `run-command' supported by `run-command-recipes'."
  (let ((recipe-symbol
         (intern (concat "run-command-recipes-" (symbol-name recipe)))))
    (require recipe-symbol)
    (add-to-list 'run-command-recipes recipe-symbol)))

(provide 'run-command-recipes)
;;; run-command-recipes.el ends here
