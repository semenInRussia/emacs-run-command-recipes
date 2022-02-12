;;; run-command-recipes-command.el --- Simplify work with options of shell command -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: semenInRussia <hrams205@gmail.com>
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

;; Simplify work with options of shell command.

;;; Code:
(require 'dash)
(require 's)

(define-error 'run-command-recipes-command-non-existent-option
    "Option of command not-existent")

(defclass run-command-recipes-command ()
  ((base :initarg :base)
   (options :initarg :options
            :accessor run-command-recipes-command-options))
  "Object helping with work with options of shell command.
BASE is base part of shell command.  For example, in command \"pandoc ...\"
base is \"pandoc\".  OPTIONS is list of options with type string for shell
command.  For example, \"--toc\", \"-disable-installer\" for pandoc.")

(defun run-command-recipes-command-options (command)
    "Get options of COMMAND.
This is alist in which keys is names of options, values is options.
For example:
'((toc . \"--toc\") (disable-installer \"-disable-installer\"))"
    (-map 'run-command-recipes-command--parse-option
          (oref command :options)))

(defun run-command-recipes-command--parse-option (from)
    "Parse FROM to normal `run-command-recipes-command' option.
FROM may be one of: string, cons from string and string.  Normal option is
cons from option name as `car', and option as `cdr'"
    (cl-typecase from
      (string `(,from . ,from))
      (cons from)))

(defun run-command-recipes-command-get-option-with-name (name command)
    "Get option with name NAME from options of COMMAND."
    (cdr (assoc name (run-command-recipes-command-options command))))

(defun run-command-recipes-command-get-some-options-with-names (names command)
    "Get some options with names NAMES from options of COMMAND."
    (--map (run-command-recipes-command-get-option-with-name it command) names))

(defun run-command-recipes-command-get-option-names (command)
    "Get some options with names NAMES from options of COMMAND."
    (-map 'car (run-command-recipes-command-options command)))

(defun run-command-recipes-command-select-options (command opts-names)
    "Select in object COMMAND some options with names OPTS-NAMES.
COMMAND created with `run-command-recipes-command'.  OPTIONS is list of
options, with type string, available only options of COMMAND."
    (run-command-recipes-command-ensure-existent-options command opts-names)
    (let ((command-base (run-command-recipes-command-base command))
          (options
           (run-command-recipes-command-get-some-options-with-names opts-names
                                                                    command)))
        (s-concat command-base " " (s-join " " options))))

(defun run-command-recipes-command-ensure-existent-options (command
                                                            options-names)
    "Ensure that all names of options OPTIONS-NAMES existent for COMMAND."
    (-when-let (non-existent-options
                (-difference
                 options-names
                 (run-command-recipes-command-get-option-names command)))
        (signal 'run-command-recipes-command-non-existent-option
                non-existent-options)))

(provide 'run-command-recipes-command)
;;; run-command-recipes-command.el ends here
