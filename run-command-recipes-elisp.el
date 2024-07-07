;;; run-command-recipes-elisp.el --- Recipe of `run-command' for `elisp` -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1.0
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
      :display "Cask: install the project dependencies"
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
    (run-command-recipes-lib-build
     (list
      (list
       :command-name "run-all-ert-tests"
       :display "ERT: run all tests"
       :lisp-function #'run-command-recipes-elisp-run-ert-all-tests)
      (list
       :command-name "run-ert-tests"
       :display "ERT: run a test"
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
    (run-command-recipes-lib-build
     (list
      (if (run-command-recipes-elisp-file-was-compiled-p)
          (list
           :command-name "byte-recompile-file"
           :lisp-function 'run-command-recipes-elisp-recompile-current-file
           :display "ReCompile to Bytes This File")
        (list
         :command-name "byte-compile-file"
         :lisp-function 'run-command-recipes-elisp-compile-current-file
         :display "Compile to Bytes File"))
      (list
       :command-name "byte-recompile-directory"
       :display "ReCompile to bytes the Directory"
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
    (run-command-recipes-lib-build
     (list
      (list
       :command-name "eldev-archives"
       :command-line "eldev archives"
       :display "Eldev: view archives")
      (list
       :command-name "eldev-upgrade"
       :command-line "eldev upgrade"
       :display "Eldev: upgrade")
      (list
       :command-name "eldev-targets"
       :command-line "eldev targets"
       :display "Eldev: view targets")
      (list
       :command-name "eldev-tree"
       :command-line "eldev dependency-tree"
       :display "Eldev: display the dependency tree")
      (list
       :command-name "eldev-build-main"
       :command-line "eldev build"
       :display "Eldev: build (main set)")
      (list
       :command-name "eldev-build-all"
       :command-line "eldev build all"
       :display "Eldev: build the project (all sets)")
      (list
       :command-name "eldev-package-build"
       :command-line "eldev package"
       :display "Eldev: build as package")
      (list
       :command-name "eldev-compile"
       :command-line "eldev compile"
       :display "Eldev: byte-compile every project file")
      (list
       :command-name "eldev-clean"
       :command-line "eldev clean"
       :display "Eldev: clean artifacts")
      (list
       :command-name "eldev-test"
       :command-line "eldev test"
       :display "Eldev: run all tests")
      (list
       :command-name "eldev-test-failed"
       :command-line "eldev test :fail"
       :display "Eldev: run failed tests")
      (list
       :command-name "eldev-test-new"
       :command-line "eldev test :new"
       :display "Eldev: run new tests")
      (list
       :command-name "eldev-lint"
       :command-line "eldev lint"
       :display "Eldev: lint the code")
      ;; commands that works for current file
      (and
       (buffer-file-name)
       (run-command-recipes-elisp-mode-p)
       (list
        (list
         :command-name "eldev-compile-current-file"
         :command-line "eldev compile {file-name}"
         :display "Eldev: byte-compile file")
        (list
         :command-name "eldev-compile-current-file-warnings-as-errors"
         :command-line "eldev compile {file-name} --warnings-as-errors"
         :display "Eldev: compile file (warnings as errors)")
        (list
         :command-name "eldev-test-current-file"
         :command-line "eldev test {file-name}"
         :display "Eldev: run file tests")
        (list
         :command-name "eldev-test-until-unexpected"
         :command-line "eldev test {file-name}"
         :display
         "Eldev: run file tests until one fails")))))))

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
    (run-command-recipes-lib-build
     (run-command-recipes-lib-compose-recipes
      'run-command-recipes-elisp-eask-run-scripts-recipe
      (list
       (list
        :command-name "eask-archivies"
        :command-line "eask archivies"
        :display "Eask: view archivies")
       (list
        :command-name "eask-autoloads"
        :command-line "eask autoloads"
        :display "Eask: Generate autoloads file'")
       (list
        :command-name "eask-concat"
        :command-line "eask concat"
        :display "Eask: concatenate elisp files'")
       (list
        :command-name "eask-info"
        :command-line "eask info"
        :display "Eask: print project info")
       (list
        :command-name "eask-keywords"
        :command-line "eask keywords"
        :display "Eask: possible keywords")
       (list
        :command-name "eask-install"
        :command-line "eask install"
        :display "Eask: install the dependencies")
       (list
        :command-name "eask-reinstall"
        :command-line "eask reinstall"
        :display "Eask: REinstall the dependencies")
       (list
        :command-name "eask-upgrade"
        :command-line "eask upgrade"
        :display "Eask: upgrade")
       (list
        :command-name "eask-run"
        :command-line "eask run"
        :display "Eask: run, execute the project")
       (list
        :command-name "eask-compile"
        :command-line "eask compile"
        :display "Eask: compile project")
       (list
        :command-name "eask-compile"
        :command-line "eask compile {file-name}"
        :display "Eask: compile file"))
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
    :display (format "Eask: run `%s' script" it))
   (run-command-recipes-elisp-eask-run-scripts)))

(provide 'run-command-recipes-elisp)
;;; run-command-recipes-elisp.el ends here
