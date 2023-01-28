;;; run-command-recipes-elisp.el --- Recipe of `run-command' for `elisp` -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.3
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
;; For use this code put the following to your Emacs configuration:
;;
;; (run-command-recipes-use-one 'elisp)
;;
;;; Code:

(require 'dash)
(require 'f)
(require 'run-command-recipes-project)
(require 'run-command-recipes-lib)

(defcustom run-command-recipes-elisp-subrecipes
  (list #'run-command-recipes-elisp-cask
        #'run-command-recipes-elisp-ert
        #'run-command-recipes-elisp-virgin
        #'run-command-recipes-elisp-eldev)
  "List of subrecipes for recipe of run-command for elisp."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defun run-command-recipes-elisp ()
  "Recipe of `run-command' for the emacs-lisp."
  (apply #'run-command-recipes-lib-compose-recipes
         run-command-recipes-elisp-subrecipes))

(defcustom run-command-recipes-elisp-cask-filename "Cask"
  "Name of cask file."
  :type 'string
  :group 'run-command-recipes)

(defun run-command-recipes-elisp-cask ()
  "Recipe of `run-command' for Cask, subrecipe of the emacs-lisp recipe."
  (when (run-command-recipes-elisp-cask-project-p)
    (list
     (list
      :command-name "cask-install"
      :display "Install Dependecies from Cask"
      :command-line "cask"))))

(defun run-command-recipes-elisp-cask-project-p ()
  "Get non-nil when current opened project have Cask."
  (run-command-recipes-project-root-has
   run-command-recipes-elisp-cask-filename))

(defcustom run-command-recipes-elisp-ert-tests-directory "test"
  "This is name of directory in which put ert tests."
  :type 'string
  :group 'run-command-recipes)

(defun run-command-recipes-elisp-ert ()
  "Recipe of `run-command' for `ert', subrecipe of the emacs-lisp recipe."
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
  "Get non-nil, if current project use `ert'."
  (run-command-recipes-project-root-has
   run-command-recipes-elisp-ert-tests-directory))

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
  (when (and (run-command-recipes-elisp-mode-p) (buffer-file-name))
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
  "Return non-nil value, if current `major-mode' is one Emacs Lisp modes."
  (-contains-p run-command-recipes-elisp-modes major-mode))

(defun run-command-recipes-elisp-file-was-compiled-p ()
  "Return t, when current file already was compiled via `byte-compile'."
  (--when-let (buffer-file-name) (f-file-p (f-swap-ext it "elc"))))

(defun run-command-recipes-elisp-compile-current-file ()
  "Compile to bytes the current Elisp file."
  (byte-compile-file (buffer-file-name)))

(defun run-command-recipes-elisp-recompile-current-file ()
  "Recompile to bytes the current Elisp file."
  (byte-recompile-file (buffer-file-name)))

(defun run-command-recipes-elisp-recompile-current-directory ()
  "Recompile to bytes the current directory."
  (byte-recompile-directory (run-command-recipes-project-root)))

(defun run-command-recipes-elisp-eldev-project-p ()
  "Return non-nil when the project root is Eldev project."
  (run-command-recipes-project-root-has-one-of
   '("Eldev" "Eldev-local")))

(defun run-command-recipes-elisp-eldev ()
  "Recipe of `run-command' for `eldev', subrecipe of the emacs-lisp recipe."
  (when (and
         (run-command-recipes-elisp-eldev-project-p)
         (executable-find "eldev"))
    (run-command-recipes-lib-bind-in-recipe
     (list
      (list
       :command-name "eldev-archives"
       :command-line "eldev archives"
       :working-dir (run-command-recipes-project-root)
       :display "View `Eldev' archives")
      (list
       :command-name "eldev-upgrade"
       :command-line "eldev upgrade"
       :working-dir (run-command-recipes-project-root)
       :display "Upgrade `Eldev' Project")
      (list
       :command-name "eldev-targets"
       :command-line "eldev targets"
       :working-dir (run-command-recipes-project-root)
       :display "View `Eldev' targets")
      (list
       :command-name "eldev-build-main"
       :command-line "eldev build"
       :working-dir (run-command-recipes-project-root)
       :display "Build Main `Eldev' targets")
      (list
       :command-name "eldev-build-all"
       :command-line "eldev build all"
       :working-dir (run-command-recipes-project-root)
       :display "Build `Eldev' Targets of Each Set")
      (list
       :command-name "eldev-package-build"
       :command-line "eldev package"
       :working-dir (run-command-recipes-project-root)
       :display "Build the `Eldev' Package")
      (list
       :command-name "eldev-compile"
       :command-line "eldev compile"
       :working-dir (run-command-recipes-project-root)
       :display "Byte-compile `Eldev' Project Files")
      (and
       (buffer-file-name)
       (list
        :command-name "eldev-compile-current-file"
        :command-line "eldev compile {file-name}"
        :working-dir (run-command-recipes-project-root)
        :display "Byte-compile Current `Eldev' File of Project")
       (list
        :command-name "eldev-compile-current-file-warnings-as-errors"
        :command-line "eldev compile {file-name} --warnings-as-errors"
        :working-dir (run-command-recipes-project-root)
        :display "Compile Current `Eldev' File of Project, Warnings as Errors"))
      (list
       :command-name "eldev-clean"
       :command-line "eldev clean"
       :working-dir (run-command-recipes-project-root)
       :display "Clean Current `Eldev' Project")
      (list
       :command-name "eldev-test"
       :command-line "eldev test"
       :working-dir (run-command-recipes-project-root)
       :display "Run Tests of the `Eldev' Project")
      (and
       (buffer-file-name)
       (list
        :command-name "eldev-test-current-file"
        :command-line "eldev test {file-name}"
        :working-dir (run-command-recipes-project-root)
        :display "Run Tests from Current Test File with `Eldev'"))
      (and
       (buffer-file-name)
       (list
        :command-name "eldev-test-until-unexpected"
        :command-line "eldev test {file-name}"
        :working-dir (run-command-recipes-project-root)
        :display "Run Tests of Current `Eldev' Project, until Some Fails"))
      (and
       (buffer-file-name)
       (list
        :command-name "eldev-test-failed"
        :command-line "eldev test :fail"
        :working-dir (run-command-recipes-project-root)
        :display "Run Failed Tests of `Current' Eldev Project"))
      (and
       (buffer-file-name)
       (list
        :command-name "eldev-test-failed"
        :command-line "eldev test :new"
        :working-dir (run-command-recipes-project-root)
        :display "Run New Tests of `Current' Eldev Project"))
      (and
       (buffer-file-name)
       (list
        :command-name "eldev-lint"
        :command-line "eldev lint"
        :working-dir (run-command-recipes-project-root)
        :display "Lint Current `Eldev' Project"))))))

(provide 'run-command-recipes-elisp)
;;; run-command-recipes-elisp.el ends here
