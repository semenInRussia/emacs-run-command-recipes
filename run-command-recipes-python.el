;;; run-command-recipes-python.el --- Recipe of `run-command' for `python` -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.2
;; Keywords: extensions run-command
;; Homepage: https://github.com/semenInRussia/emacs-run-command-recipes
;; URL: https://github.com/semenInRussia/emacs-run-command-recipes/blob/main/docs/python.md

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
;; (run-command-recipes-use-one 'python)
;;
;;; Code:

(require 'dash)
(require 'f)
(require 'run-command-recipes-project)
(require 'run-command-recipes-lib)

(defcustom run-command-recipes-python-modes
  '(python-mode)
  "List of modes in which will work recipe of `run-command' `python'."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defcustom run-command-recipes-python-run-command "python {file-name}"
  "Shell command, which just run current python file, and nothing else.
See `run-command-recipes-lib-bind-in-recipe' for understand what's {file-name}."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-python-test-filename "test_.*\.py"
  "Regexp which math with test's file of Python."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-python-pytest-file-command "pytest {file-name}"
  "Command which run `pytest' on current file.
See `run-command-recipes-lib-bind-in-recipe' for understand what's {file-name}."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-python-interactively-run-command
  "python -i {file-name}"
  "Command which run python file interactively.
See `run-command-recipes-lib-bind-in-recipe' for understand what's {file-name}."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-python-tests-dirs
  '("tests")
  "List of directories for pytest's tests."
  :type '(repeat string)
  :group 'run-command-recipes)

(defcustom run-command-recipes-python-pytest-project-command "pytest"
  "Command which run all tests of python project via pytest.
Command will runned on root of project."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-python-subrecipes
  '(run-command-recipes-python-pytest run-command-recipes-python-virgin)
  "List of subrecipes for recipe of `run-command' for python."
  :group 'run-command-recipes-python
  :type '(repeat function))

(defun run-command-recipes-python ()
  "Recipe of `run-command' for `python'."
  (apply #'run-command-recipes-lib-compose-recipes
         run-command-recipes-python-subrecipes))

(defun run-command-recipes-python-mode-p ()
  "Return t if in current buffer will should work `run-command-recipes-python'."
  (-contains-p run-command-recipes-python-modes major-mode))

(defun run-command-recipes-python-pytest ()
  "Recipe of `run-command' for `pytest', subrecipe of recipe for `python'."
  (run-command-recipes-lib-bind-in-recipe
   (when (executable-find "pytest")
     (list
      (when (run-command-recipes-python-test-buffer-p)
        (list
         :command-name "run-pytests-in-current-file"
         :display "Run `pytest' for Current Pytest File"
         :command-line run-command-recipes-python-pytest-file-command))
      (when (run-command-recipes-python-pytest-project-p)
        (list
         :command-name "run-all-pytests-in-project"
         :display "Run All Tests in Current Project via Pytest"
         :working-dir (run-command-recipes-project-root)
         :command-line run-command-recipes-python-pytest-project-command))))))

(defun run-command-recipes-python-pytest-project-p ()
  "Return t if current project has pytest test."
  (run-command-recipes-project-root-has-one-of
   run-command-recipes-python-tests-dirs))

(defun run-command-recipes-python-test-buffer-p ()
  "Return t, when current file is test's file of python.
Implementation depends on `run-command-recipes-python-test-buffer-p-function'"
  (--when-let
      (buffer-file-name)
    (string-match-p run-command-recipes-python-test-filename it)))

(defun run-command-recipes-python-virgin ()
  "Recipe of `run-command' for virgin version of python, subrecipe of python."
  (run-command-recipes-lib-bind-in-recipe
   (when (and
          (run-command-recipes-python-mode-p)
          (executable-find "pytest"))
     (list
      (list
       :command-name "run-python-file"
       :display "Just Run Current Python File"
       :command-line run-command-recipes-python-run-command)
      (list
       :command-name "interactively-run-python-file"
       :display "Run Current Python Interactively File"
       :command-line run-command-recipes-python-interactively-run-command)))))

(provide 'run-command-recipes-python)
;;; run-command-recipes-python.el ends here
