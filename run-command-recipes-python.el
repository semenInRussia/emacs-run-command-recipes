;;; run-command-recipes-python.el --- Recipe of `run-command' for `python` -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
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
;; For use this code put this to config:
;;
;; (run-command-recipes-use-one 'python)
;;
;;; Code:

(require 'dash)
(require 'f)
(require 'run-command-recipes-project)

(defcustom run-command-recipes-python-modes '(python-mode)
  "List of modes in which will work recipe of `run-command' `python'."
  :type '(repeat symbol)
  :group 'run-command-recipes)


(defcustom run-command-recipes-python-mode-p-function nil
  "Func returning t if in current buffer can work `run-command-recipes-python'.
If nil, then use default implementation"
  :type 'predicate
  :group 'run-command-recipes)


(defun run-command-recipes-python-mode-p ()
  "Return t if in current buffer will can work `run-command-recipes-python'.
Implementation depends on `run-command-recipes-python-mode-predicate'"
  (if run-command-recipes-python-mode-p-function
      (funcall run-command-recipes-python-mode-p-function)
      (-contains-p run-command-recipes-python-modes major-mode)))


(defcustom run-command-recipes-python-run-command "python \"%s\""
  "Shell command, which just run current python file, and nothing else.
Here %s is path of file."
  :type 'string
  :group 'run-command-recipes)


(defcustom run-command-recipes-python-test-filename "test_.*\.py"
  "Regexp which math with test's file of Python."
  :type 'string
  :group 'run-command-recipes)


(defcustom run-command-recipes-python-test-buffer-p-function nil
  "Funtion which get t, when current file is test's file of Python.
If nil, then use default implementation."
  :type 'predicate
  :group 'run-command-recipes)


(defcustom run-command-recipes-python-pytest-file-command "pytest \"%s\""
  "Command which run `pytest' on current file."
  :type 'string
  :group 'run-command-recipes)


(defcustom run-command-recipes-python-interactively-run-command
  "python -i \"%s\""
  "Command which run python file interactively."
  :type 'string
  :group 'run-command-recipes)


(defun run-command-recipes-python-test-buffer-p ()
    "Return t, when current file is test's file of python.
Implementation depends on `run-command-recipes-python-test-buffer-p-function'"
    (-when-let (file-name (buffer-file-name))
        (string-match-p run-command-recipes-python-test-filename
                        (buffer-file-name))))


(defcustom run-command-recipes-python-tests-dirs '("tests")
  "List of directories for pytest's tests."
  :type '(repeat string)
  :group 'run-command-recipes)


(defcustom run-command-recipes-python-pytest-project-p-function nil
  "Func returning t if current project has pytest's tests.
If nil, then use default implementation"
  :type 'predicate
  :group 'run-command-recipes)


(defun run-command-recipes-python-pytest-project-p ()
    "Return t if current project has pytest's test.
If nil, then use default implementation."
    (if run-command-recipes-python-pytest-project-p-function
        (funcall run-command-recipes-python-pytest-project-p-function)
        (run-command-recipes-project-root-has-one-of
         run-command-recipes-python-tests-dirs)))


(defcustom run-command-recipes-python-pytest-project-command "pytest"
  "Command which run all tests of python project via Pytest.
Command will runned on root of project."
  :type 'string
  :group 'run-command-recipes)

(defun run-command-recipes-python ()
    "This is recipe for `run-command' from `run-command-recipes'."
    (-concat
     (when (run-command-recipes-python-mode-p)
         (list
          (list
           :command-name "run-python-file"
           :display "Just Run Current Python File"
           :command-line (format run-command-recipes-python-run-command
                                 (buffer-file-name)))
          (list
           :command-name "interactively-run-python-file"
           :display "Run Current Python Interactively File"
           :command-line (format
                          run-command-recipes-python-interactively-run-command
                          (buffer-file-name)))))
     (list
      (when (run-command-recipes-python-test-buffer-p)
          (list
           :command-name "run-pytests-in-current-file"
           :display "Run `pytest' in Current Pytest File"
           :command-line (format run-command-recipes-python-pytest-file-command
                                 (buffer-file-name))))
      (when (run-command-recipes-python-pytest-project-p)
          (list
           :command-name "run-all-pytests-in-project"
           :display "Run All Tests in Current Project via Pytest"
           :working-dir (run-command-recipes-project-root)
           :command-line run-command-recipes-python-pytest-project-command)))))


(provide 'run-command-recipes-python)
;;; run-command-recipes-python.el ends here
