;;; run-command-recipes.el --- This is collection of recipes to `run-command' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.3
;; Package-Requires: ((emacs "25.1") (dash "2.18.0") (f "0.20.0") (run-command "0.1.0"))
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

;; If you're going to use all supported language, just put the following
;; code into your Emacs configuration:
;;
;;   (run-command-recipes-use-all)
;;
;; If you're going to use only some special languages, just put the following
;; code to your Emacs configuration:

;;
;;   (run-command-recipes-use latex
;;                            pandoc)
;;

;; Also, Instead of LaTeX and pandoc, you can put anything from the
;; following list:

;; - latex
;; - pandoc
;; - haskell
;; - elisp
;; - rust
;; - python
;; - c
;; - cpp
;; - csharp
;; - java
;; - racket
;; - make
;; - go
;; - elixir

;;; Code:

(require 'run-command)

(require 'dash)
(require 's)

(defgroup run-command-recipes nil
  "Run recipes for the package `run-command'."
  :group 'tools
  :link '(url-link
          :tag "GitHub"
          "https://github.com/semenInRussia/emacs-run-command-recipes"))

(defcustom run-command-recipes-supported-recipes
  '(latex pandoc haskell elisp rust python c cpp csharp java racket make go elixir)
  "List of recipes names supported by `run-command-recipes'."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defun run-command-recipes-use-all ()
  "Use all recipes for `run-command' supported by `run-command-recipes'."
  (-each run-command-recipes-supported-recipes 'run-command-recipes-use-one))

(defmacro run-command-recipes-use (&rest recipes)
  "Use RECIPES for `run-command' supported by `run-command-recipes'.

Each of RECIPES is indentifier to recipe without `run-command-recipes-'
prefix."
  `(--each ',recipes (run-command-recipes-use-one it)))

(defun run-command-recipes-use-one (recipe)
  "Use RECIPE for `run-command' supported by `run-command-recipes'.

RECIPE is symbol references to the one of `run-command-recipes'
recipes without `run-command-recipes-' prefix.  See to the comments at
the start of this file for list of supported recipes."
  (let ((recipe-symbol
         (intern (concat "run-command-recipes-" (symbol-name recipe)))))
    (require recipe-symbol)
    (add-to-list 'run-command-recipes recipe-symbol)))

(provide 'run-command-recipes)
;;; run-command-recipes.el ends here
