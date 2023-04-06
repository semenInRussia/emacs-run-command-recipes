;;; run-command-recipes-lib.el --- Standard library for `run-command-recipes' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.2
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

(require 'run-command-recipes-project)

(require 'dash)
(require 'f)
(require 's)

(defun run-command-recipes-lib-compose-recipes (&rest recipes)
  "Return the composition of all RECIPES.

Each of given RECIPES can be either function that returns a `plist' (recipe)
or just a `plist'"
  (->>
   recipes
   (-map #'run-command-recipes-lib--to-recipe)
   (apply #'-concat)))

(defun run-command-recipes-lib--to-recipe (recipe)
  "Expand a `run-command' RECIPE.

It can be either function that returns a `plist' (recipe) or just a `plist'"
  (cond
   ((functionp recipe)
    (funcall recipe))
   (t recipe)))

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

Also, change the :working-dir of each plists to the value of
the `run-command-recipes-project-root'

And also, if a recipe has the unsupported field :lisp-function, then replace it
with the supporting things (:hook + :command-line \"true\").

So, see `run-command-recipes-lib-bind-variables' for the list of changes"
  (let ((root (run-command-recipes-project-root)))
    (->>
     plists
     (--map (run-command-recipes-lib--working-dir it root))
     (-map 'run-command-recipes-lib--command-line)
     (-map 'run-command-recipes-lib--lisp-function))))

(defun run-command-recipes-lib--command-line (recipe)
  "Do some change in property of RECIPE :command-line.

See `run-command-recipes-lib-bind-variables'"
  (run-command-recipes-lib-plist-map
   recipe
   :command-line #'run-command-recipes-lib--change-command-line))

(defun run-command-recipes-lib--change-command-line (command-line)
  "Replace some fragments of COMMAND-LINE to respective things.

See `run-command-recipes-lib-bind-variables'"
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
side-effects.

If a PROP isn't exists in the PLIST, then return the same PLIST without
modifications"
  (--if-let
      (plist-get plist prop)
      (plist-put plist prop (funcall transformer it))
    plist))

(defun run-command-recipes-lib--working-dir (recipe &optional root)
  "If working-dir of plist RECIPE is nil change it to project root path.

Return modified RECIPE.  If ROOT is non-nil, then change working-dir to it,
otherwise change to value of a `run-command-recipes-project' call"
  (if (or (plist-get recipe :working-dir) (not recipe))
      recipe
    (append recipe
            (list :working-dir
                  (or root (run-command-recipes-project-root))))))

(defun run-command-recipes-lib--nothing-runner (_line _buf-base output-buf)
  "Runner for `run-command' which do nothing.

OUTPUT-BUF is a buffer in which should be saved the ouput, this runner do
nothing, so save nothing."
  ;; this should go to the file from which was a `run-command' call
  (switch-to-buffer (car (buffer-list))))

(defun run-command-recipes-lib--lisp-function (recipe)
  "Get lisp-function prop from the plist RECIPE and get `run-command' recipe.

This function is support of the `:lisp-function' from the past `run-command'
versions (at the last version this isn't supported)"
  (if (or
       (memq 'run-command-experiment-lisp-commands
             run-command-experiments)
       (not (map-elt recipe :lisp-function)))
      recipe
    (->
     recipe
     (map-insert :command-line "true")
     (map-insert :runner #'run-command-recipes-lib--nothing-runner)
     (map-insert :hook (map-elt recipe :lisp-function)))))

(provide 'run-command-recipes-lib)
;;; run-command-recipes-lib.el ends here
