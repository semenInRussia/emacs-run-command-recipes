;;; run-command-recipes-elisp.el --- Recipe of `run-command' for `elisp` -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Keywords: extensions run-command
;; Homepage: https://github.com/semenInRussia/emacs-run-command-recipes
;; URL: https://github.com/semenInRussia/emacs-run-command-recipes/blob/main/docs/elisp.md

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
;; For use this code put this to config:
;; ```
;; (run-command-recipes-use-one 'elisp)
;; ```
;;; Code:

(require 'dash)
(require 'f)
(require 'run-command-recipes-project)
(require 'run-command-recipes-lib)

(defun run-command-recipes-elisp ()
  "Recipe of `run-command' for emacs-lisp."
  (run-command-recipes-lib-compose-recipes
   #'run-command-recipes-elisp-cask
   #'run-command-recipes-elisp-ert
   #'run-command-recipes-elisp-virgin))

(defun run-command-recipes-elisp-cask ()
  "Recipe of `run-command' for cask, subrecipe of emacs-lisp recipe."
  (when (run-command-recipes-elisp-cask-project-p)
    (list
     (list
      :command-name "cask-install"
      :display "Install Dependecies from Cask"
      :command-line "cask"))))

(defcustom run-command-recipes-elisp-cask-project-p-function nil
  "Get non-nil when current opened project have Cask."
  :type 'predicate
  :group 'run-command-recipes)

(defcustom run-command-recipes-elisp-cask-filename "Cask"
  "Name of cask file."
  :type 'string
  :group 'run-command-recipes)

(defun run-command-recipes-elisp-cask-project-p ()
  "Get non-nil when current opened project have Cask."
  (if run-command-recipes-elisp-cask-project-p-function
      (funcall run-command-recipes-elisp-cask-project-p-function)
    (run-command-recipes-project-root-has
     run-command-recipes-elisp-cask-filename)))

(defcustom run-command-recipes-elisp-has-ert-tests-p-function nil
  "Funtion not take arguments, get non-nil, if current project has `ert' tests.
By virgin this is function which return t, when root of project has directory
`run-command-recipes-elisp-ert-tests-directory'"
  :type 'predicate
  :group 'run-command-recipes)

(defcustom run-command-recipes-elisp-ert-tests-directory "test"
  "This is name of directory in which put ert tests."
  :type 'string
  :group 'run-command-recipes)

(defun run-command-recipes-elisp-ert ()
  "Recipe of `run-command' for `ert', subrecipe of emacs-lisp recipe."
  (when (run-command-recipes-elisp-has-ert-tests-p)
    (list
     (list
      :command-name "run-all-ert-tests"
      :display "Run All ERT Tests"
      :lisp-function #'run-command-recipes-elisp-run-ert-all-tests)
     (list
      :command-name "run-ert-tests"
      :display "Run One ERT Test"
      :lisp-function #'run-command-recipes-elisp-run-ert))))

(defun run-command-recipes-elisp-has-ert-tests-p ()
  "Get non-nil, if current project has `ert'."
  (if run-command-recipes-elisp-has-ert-tests-p-function
      (funcall run-command-recipes-elisp-has-ert-tests-p-function)
    (run-command-recipes-project-root-has
     run-command-recipes-elisp-ert-tests-directory)))

(defun run-command-recipes-elisp-run-ert-all-tests ()
  "Run all `ert' in directory."
  (ert t))

(defun run-command-recipes-elisp-run-ert ()
  "Function which run `ert'."
  (call-interactively #'ert))

(defcustom run-command-recipes-elisp-modes
  '(emacs-lisp-mode lisp-interaction-mode)
  "List of modes in which can work recipe `elisp' for `run-command'."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defun run-command-recipes-elisp-virgin ()
  "Recipe of `run-command' for virgin version of emacs-lisp."
  (when (run-command-recipes-elisp-mode-p)
    (list
     (if (run-command-recipes-elisp-file-was-compiled-p)
         (list
          :command-name "byte-recompile-file"
          :lisp-function 'run-command-recipes-elisp-recompile-current-file
          :display "ReCompile to Bytes This File")
       (list
        :command-name "byte-compile-file"
        :lisp-function 'run-command-recipes-elisp-compile-current-file
        :display "Compile to Bytes This File"))
     (list
      :command-name "byte-recompile-directory"
      :display "ReCompile to bytest Current Directory"
      :lisp-function 'run-command-recipes-elisp-recompile-current-directory))))

(defun run-command-recipes-elisp-mode-p ()
  "Predicate, return non-nil value, if current `major-mode' is Emacs Lisp mode."
  (-contains-p run-command-recipes-elisp-modes major-mode))

(defun run-command-recipes-elisp-file-was-compiled-p ()
  "Return t when current file was run in `byte-compile'."
  (-when-let
      (filename (buffer-file-name))
    (f-file-p (f-swap-ext filename "elc"))))

(defun run-command-recipes-elisp-compile-current-file ()
  "Compile to bytes current Elisp file."
  (byte-compile-file (buffer-file-name)))

(defun run-command-recipes-elisp-recompile-current-file ()
  "Recompile to bytes current Elisp file."
  (byte-recompile-file (buffer-file-name)))

(defun run-command-recipes-elisp-recompile-current-directory ()
  "Recompile to bytes current directory."
  (byte-recompile-directory (run-command-recipes-project-root)))

(provide 'run-command-recipes-elisp)
;;; run-command-recipes-elisp.el ends here
