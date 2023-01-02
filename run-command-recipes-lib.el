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

(declare-function run-command--run-compile "run-command")
(declare-function run-command--run-term "run-command")
(declare-function run-command--run-vterm "run-command")

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
  (let ((root (run-command-recipes-project-root)))
    (->>
     plists
     (--map
      (run-command-recipes-lib-plist-map
       it
       :command-line #'run-command-recipes-lib--change-command-line))
     (-map 'run-command-recipes-lib--change-method-of-run)
     (--map (run-command-recipes-lib--change-working-dir it root)))))

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

(defun run-command-recipes-lib--change-method-of-run (recipe)
  "Change the run method of RECIPE depends on its attributes.

RECIPE will be runned with `compilation-mode' if :way attribute of
RECIPE alist setted to 'compile, or if `run-command-run-method' setted to
'compile and :run-method attribute isn't provided.  Also RECIPE can be runned
with `term-mode', for enabling either set :run-method attribute of RECIPE to
'term or set `run-command-run-method' to 'term and don't provide that attribute.
Also you can run command asynchronously in background with set :run-method
attribute of RECIPE to 'async."
  (let* ((run-method (plist-get recipe :run-method))
         (command-line
          (if (functionp (plist-get recipe :command-line))
              (funcall (plist-get recipe :command-line))
            (plist-get recipe :command-line)))
         (command-name
          (or
           (plist-get recipe :command-name)
           (plist-get recipe :command-line)))
         (scope-name
          (or
           (plist-get recipe :scope-name)
           (plist-get recipe :working-dir)
           default-directory))
         (buffer-base-name (format "%s[%s]" command-name scope-name)))
    (when (and command-line run-method)
      (plist-put recipe
                 :lisp-function         ;nofmt
                 (cond
                  ((eq run-method 'compile)
                   (message "COMPILE")
                   (lambda ()
                     (run-command--run-compile command-line buffer-base-name)))
                  ((eq run-method 'term)
                   (lambda ()
                     (run-command--run-term command-line buffer-base-name)))
                  ((eq run-method 'vterm)
                   (lambda ()
                     (run-command--run-vterm command-line buffer-base-name)))
                  ((eq run-method 'async)
                   (lambda ()
                     (run-command-recipes-lib--run-async
                      command-line buffer-base-name))))))
    recipe))

(defun run-command-recipes-lib--run-async (command-line buffer-base-name)
  "Run COMMAND-LINE asynchronously in background BUFFER-BASE-NAME."
  (async-shell-command command-line buffer-base-name))

(defun run-command-recipes-lib--change-working-dir (recipe &optional root)
  "If working-dir of plist RECIPE is nil change it to project root path.

Return modified RECIPE.  If ROOT is non-nil, then change working-dir to it,
otherwise change to value of a `run-command-recipes-project' call"
  (if (plist-get recipe :working-dir)
      recipe
    (append recipe
            (list :working-dir
                  (or root (run-command-recipes-project-root))))))

(provide 'run-command-recipes-lib)
;;; run-command-recipes-lib.el ends here
