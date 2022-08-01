;;; run-command-recipes-lib.el --- Standard library for `run-command-recipes' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>

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
;; Standard library for `run-command-recipes'

;;; Code:

(require 'dash)
(require 'f)
(require 's)

(defun run-command-recipes-lib-compose-recipes (&rest recipes)
  "Return composition of all RECIPES."
  (->> recipes (-map #'funcall) (apply #'-concat)))

(defcustom run-command-recipes-lib-bind-variables
  '(("file-name" . (buffer-file-name))
    ("current-dir" . default-directory)
    ("file-name-no-ext" . (--when-let (buffer-file-name) (f-no-ext it))))
  "Variables' usages for :command-line in recipes of `run-command'.
Alist in which keys are fragments of strings, values are Lisp sexps which will
replace source string.  In string at :command-line {file-name} will be replaced
to value of this alist at key \"file-name\""
  :type '(alist :key-type string :value-type list)
  :group 'run-command-recipes)

(defun run-command-recipes-lib-bind-in-recipe (plists)
  "In each plist of PLISTS replace \"some things\" in :command-line.

Each of PLISTS is element of `run-command' recipe result (see variable
`run-command-recipes'), so each of plists can has value at key :command-line

\"Some things\" must have the following syntax:

{something}

But insetad of something you may use one of following list:

- file-name
Absolute path to current file with quotes \"\".

- file-name-no-ext
Like on the file-name, but without extension

- current-dir
Full path to the directory which has the file at current buffer"
  (->>
   plists
   (--map
    (run-command-recipes-lib-plist-map
     it
     :command-line #'run-command-recipes-lib--change-command-line))))

(defun run-command-recipes-lib--change-command-line (command-line)
  "Replace some fragments of COMMAND-LINE to respective things."
  (let ((replacements
         (--map
          (cons
           (s-concat "{" (car it) "}")
           (eval (cdr it)))
          run-command-recipes-lib-bind-variables)))
    (s-replace-all replacements command-line)))

(defun run-command-recipes-lib-plist-map (plist prop transformer)
  "Transform the value of PROP in PLIST with TRANSFORMER.
This function modifies plist with `plist-put'.  So it does the same
side-effects."
  (--when-let
      (plist-get plist prop)
    (->> it (funcall transformer) (plist-put plist prop))))

(provide 'run-command-recipes-lib)
;;; run-command-recipes-lib.el ends here
