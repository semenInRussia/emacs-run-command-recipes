;;; run-command-recipes-lib.el --- Standard library for `run-command-recipes' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.3
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
;; Standard library of `run-command-recipes'

;;; Code:

(require 'dash)
(require 'f)
(require 's)

(defun run-command-recipes-lib-compose-recipes (&rest recipes)
  "Return the composition of all RECIPES."
  (->> recipes (-map #'funcall) (apply #'-concat)))

(defcustom run-command-recipes-lib-bind-variables
  '(("file-name" . (buffer-file-name))
    ("current-dir" . default-directory)
    ("file-name-no-ext" . (--when-let (buffer-file-name) (f-no-ext it))))
  "Variables for :command-line in recipes of `run-command'.

This is `alist' in which keys are fragments of strings, values are
Lisp sexps which should replace fragements to result of its
evaulating.

In :command-line of a `run-command' recipe string {file-name} will be replaced
with the evaulating of key, in the current case, evaluating
of (buffer-file-name)

List of variables bindings:

- file-name
Absolute path to current file with quotes \"\".

- file-name-no-ext
Like on the file-name, but without extension

- current-dir
Full path to the directory which has the file at current buffer"
  :type '(alist :key-type string :value-type list)
  :group 'run-command-recipes)

(defun run-command-recipes-lib-bind-in-recipe (plists)
  "For each plist of PLISTS replace the some things in the :command-line string.

Each of PLISTS is recipe for `run-command', result of this function
should be list of plists, but each plist will be with replaced
:command-line string, for example in :command-line string {file-name}
will be replaced with evaulating of (buffer-file-name), because
'file-name is the key of `run-command-recipes-lib-bind-variables'
and (buffer-file-name) is value.

So, see `run-command-recipes-lib-bind-variables' for the list of changes"
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
           (concat "{" (car it) "}")
           (eval (cdr it)))
          run-command-recipes-lib-bind-variables)))
    (run-command-recipes-lib-replace-all replacements command-line)))

(defun run-command-recipes-lib-replace-all (replacements s)
  "Replace the `car's of REPLACEMENTS to the `cdr's REPLACEMENTS in S."
  (replace-regexp-in-string
   (regexp-opt (-map 'car replacements))
   (lambda (from)
     (alist-get from replacements "" nil #'string-equal))
   s))

(defun run-command-recipes-lib-plist-map (plist prop transformer)
  "Transform the value of PROP in PLIST with TRANSFORMER.

This function modifies plist with `plist-put'.  So it does the same
side-effects."
  (--when-let
      (plist-get plist prop)
    (plist-put plist prop (funcall transformer it))))

(provide 'run-command-recipes-lib)
;;; run-command-recipes-lib.el ends here
