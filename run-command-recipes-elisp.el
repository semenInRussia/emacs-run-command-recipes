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
        #'run-command-recipes-elisp-eldev
        #'run-command-recipes-elisp-eask)
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
    (run-command-recipes-lib-bind-in-recipe
     (list
      (list
       :command-name "run-all-ert-tests"
       :display "Run All ERT Tests"
       :lisp-function #'run-command-recipes-elisp-run-ert-all-tests)
      (list
       :command-name "run-ert-tests"
       :display "Run One ERT Test"
       :lisp-function #'run-command-recipes-elisp-run-ert)))))

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
    (run-command-recipes-lib-bind-in-recipe
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
       :lisp-function
       'run-command-recipes-elisp-recompile-current-directory)))))

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
       :display "View `Eldev' archives")
      (list
       :command-name "eldev-upgrade"
       :command-line "eldev upgrade"
       :display "Upgrade `Eldev' Project")
      (list
       :command-name "eldev-targets"
       :command-line "eldev targets"
       :display "View `Eldev' targets")
      (list
       :command-name "eldev-build-main"
       :command-line "eldev build"
       :display "Build Main `Eldev' targets")
      (list
       :command-name "eldev-build-all"
       :command-line "eldev build all"
       :display "Build `Eldev' Targets of Each Set")
      (list
       :command-name "eldev-package-build"
       :command-line "eldev package"
       :display "Build the `Eldev' Package")
      (list
       :command-name "eldev-compile"
       :command-line "eldev compile"
       :display "Byte-compile `Eldev' Project Files")
      (list
       :command-name "eldev-clean"
       :command-line "eldev clean"
       :display "Clean Current `Eldev' Project")
      (list
       :command-name "eldev-test"
       :command-line "eldev test"
       :display "Run Tests of the `Eldev' Project")
      (list
       :command-name "eldev-test-until-unexpected"
       :command-line "eldev test {file-name}"
       :display "Run Tests of Current `Eldev' Project, until Some Fails")
      (list
       :command-name "eldev-test-failed"
       :command-line "eldev test :fail"
       :display "Run Failed Tests of `Current' Eldev Project")
      (list
       :command-name "eldev-test-failed"
       :command-line "eldev test :new"
       :display "Run New Tests of `Current' Eldev Project")
      (list
       :command-name "eldev-lint"
       :command-line "eldev lint"
       :display "Lint Current `Eldev' Project")
      ;; commands that works for current file
      (and
       (buffer-file-name)
       (run-command-recipes-elisp-mode-p)
       (list
        (list
         :command-name "eldev-compile-current-file"
         :command-line "eldev compile {file-name}"
         :display "Byte-compile Current `Eldev' File of Project")
        (list
         :command-name "eldev-compile-current-file-warnings-as-errors"
         :command-line "eldev compile {file-name} --warnings-as-errors"
         :display
         "Compile Current `Eldev' File of Project, Warnings as Errors")
        (list
         :command-name "eldev-test-current-file"
         :command-line "eldev test {file-name}"
         :display "Run Tests from Current Test File with `Eldev'")))))))

(defcustom run-command-recipes-elisp-eask-linters
  '(elsa
    checkdoc
    elint
    elisp-lint
    elsa
    indent
    keywords
    package
    regexp)
  "List of the linters which can be runned with `Eask'.

Each element of the list, should be symbol."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defun run-command-recipes-elisp-eask-project-p ()
  "Return non-nil when the active project is Eask project."
  (run-command-recipes-project-root-has-one-of '("Eask" "eask")))

(defun run-command-recipes-elisp-eask-run-scripts (&optional project-root)
  "Find in the Eask file of the PROJECT-ROOT all scripts to run using eask run.

PROJECT-ROOT defaults to value of the `run-command-recipes-project-root'"
  (setq project-root
        (or project-root (run-command-recipes-project-root)))
  (->>
   (f-join project-root "Eask")
   (f-read)
   (s-prepend "(")
   (s-append ")")
   (read)
   (--filter (eq (car it) 'script))
   (-map '-second-item)))

(defun run-command-recipes-elisp-eask ()
  "Recipe of `run-command' for `eask', subrecipe of the emacs-lisp recipe."
  (when (and
         (run-command-recipes-elisp-eask-project-p)
         (executable-find "eask"))
    (run-command-recipes-lib-bind-in-recipe
     (run-command-recipes-lib-compose-recipes
      'run-command-recipes-elisp-eask-run-scripts-recipe
      (list
       (list
        :command-name "eask-archivies"
        :command-line "eask archivies"
        :display "List out all `Eask' package archivies")
       (list
        :command-name "eask-autoloads"
        :command-line "eask autoloads"
        :display "Generate autoloads file using `Eask'")
       (list
        :command-name "eask-concat"
        :command-line "eask concat"
        :display "Concatenate elisp files using `Eask'")
       (list
        :command-name "eask-info"
        :command-line "eask info"
        :display "Print info about the `Eask' project")
       (list
        :command-name "eask-keywords"
        :command-line "eask keywords"
        :display "List available keywords that can be used in the header")
       (list
        :command-name "eask-install"
        :command-line "eask install"
        :display "Install the `Eask' project dependecies")
       (list
        :command-name "eask-reinstall"
        :command-line "eask reinstall"
        :display "ReInstall the `Eask' project dependecies")
       (list
        :command-name "eask-upgrade"
        :command-line "eask upgrade"
        :display "Upgrade the `Eask' project dependecies")
       (list
        :command-name "eask-run"
        :command-line "eask run"
        :display "Run the `Eask' project")
       (list
        :command-name "eask-compile"
        :command-line "eask compile"
        :display "Compile the `Eask' project")
       (list
        :command-name "eask-compile"
        :command-line "eask compile {file-name}"
        :display "Compile the current file of `Eask' project"))
      (--map
       (list
        :command-name (format "eask-lint-%s" it)
        :command-line (format "eask lint %s" it)
        :display (format "Lint the current `Eask' project with `%s'" it))
       run-command-recipes-elisp-eask-linters)
      (--map
       (list
        :command-name (format "eask-lint-%s-current-file" it)
        :command-line (format "eask lint %s {file-name}" it)
        :display (format "Lint the current opened file with Eask + `%s'" it))
       run-command-recipes-elisp-eask-linters)))))

(defun run-command-recipes-elisp-eask-run-scripts-recipe ()
  "Function returning a `run-command' recipe for `Eask' run scripts.

\"Eask run scripts\" mean scripts, what can be runned using \"eask run\""
  (--map
   (list
    :command-name (format "eask-run-%s" it)
    :command-line (format "eask run %s" it)
    :display (format "Eask-run %s" it))
   (run-command-recipes-elisp-eask-run-scripts)))

(provide 'run-command-recipes-elisp)
;;; run-command-recipes-elisp.el ends here
