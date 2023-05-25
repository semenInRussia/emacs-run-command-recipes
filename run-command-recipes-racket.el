;;; run-command-recipes-racket.el --- Recipe of `run-command' for `racket` -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.3
;; Keywords: extensions run-command
;; Homepage: https://github.com/semenInRussia/emacs-run-command-recipes
;; URL: https://github.com/semenInRussia/emacs-run-command-recipes/blob/main/docs/racket.md

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
;; (run-command-recipes-use-one 'racket)
;;
;;; Code:

(require 'dash)
(require 'f)
(require 'run-command-recipes-lib)

(defcustom run-command-recipes-racket-modes
  '(racket-mode)
  "List of major modes in which recipe of `run-command' for racket should work."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defcustom run-command-recipes-racket-raco-to-bytecode-command
  "raco make {file-name}"
  "Command of raco, which translate racket source file to racket bytecode.
See `run-command-recipes-lib-build' for understand what's {file-name}."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-racket-compile-and-run-command
  "raco exe {file-name} && {file-name-no-ext}"
  "Command of raco, which compile and execute racket source file.
See `run-command-recipes-lib-build' for understand what's {file-name}."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-racket-compile-and-run-command
  "raco exe {file-name} && {file-name-no-ext}"
  "Command of raco, which compile and execute racket source file.
See `run-command-recipes-lib-build' for understand what's {file-name}."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-racket-run-command
  "racket {file-name}"
  "Command of racket, which juts run a racket source file.
See `run-command-recipes-lib-build' for understand what's {file-name}."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-racket-compile-command
  "raco exe {file-name}"
  "Command of raco, which compile racket source file.
See `run-command-recipes-lib-build' for understand what's {file-name}."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-racket-run-test-file-command
  "raco test {file-name}"
  "Command of raco, which run tests of racket source file.
See `run-command-recipes-lib-build' for understand what's {file-name}."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-racket-run-directory-tests-command
  "raco test ."
  "Command of raco, which run all tests of current racket project."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-racket-pretty-read-command
  "raco read {file-name}"
  "Command of raco, which pretty print content of current racket source file.
See `run-command-recipes-lib-build' for understand what's {file-name}."
  :type 'string
  :group 'run-command-recipes)

(defun run-command-recipes-racket ()
  "Recipe of `run-command' for racket."
  (when (run-command-recipes-racket-p)
    (run-command-recipes-lib-compose-recipes
     #'run-command-recipes-racket-virgin
     #'run-command-recipes-racket-raco)))

(defun run-command-recipes-racket-p ()
  "Return t, when recipe of `run-command' for racket should work."
  (-contains-p run-command-recipes-racket-modes major-mode))

(defun run-command-recipes-racket-virgin ()
  "Recipe of `run-command' for virgin version of racket, subrecipe of racket."
  (run-command-recipes-lib-build
   (when (and (buffer-file-name) (executable-find "racket"))
     (list
      (list
       :command-name "racket-run-file"
       :display "Racket: run, execute file (without compiling!)"
       :command-line run-command-recipes-racket-run-command)))))

(defun run-command-recipes-racket-raco ()
  "Recipe of `run-command' for raco, subrecipe of racket."
  (run-command-recipes-lib-build
   (when (and (buffer-file-name) (executable-find "raco"))
     (list
      (list
       :command-name "raco-check-file"
       :display "Raco: byte-compile file"
       :command-line run-command-recipes-racket-raco-to-bytecode-command)
      (list
       :command-name "raco-compile-file"
       :display "Raco: compile file"
       :command-line run-command-recipes-racket-compile-command)
      (list
       :command-name "raco-run-file"
       :display "Raco: run, execute file"
       :command-line run-command-recipes-racket-compile-and-run-command)
      (list
       :command-name "raco-run-tests-in-file"
       :display "Raco: run file tests"
       :command-line run-command-recipes-racket-run-test-file-command)
      (list
       :command-name "raco-run-directory-tests"
       :display "Raco: run directory, project tests"
       :command-line run-command-recipes-racket-run-directory-tests-command)
      (list
       :command-name "raco-pretty-read-file"
       :display "Raco: pretty print file"
       :command-line run-command-recipes-racket-pretty-read-command)))))

(provide 'run-command-recipes-racket)
;;; run-command-recipes-racket.el ends here
