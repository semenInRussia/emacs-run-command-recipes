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

(require 'cl-lib)
(require 'dash)
(require 'eieio)                        ; Builtin lib for objects
(require 's)

(define-error 'run-command-recipes-command-non-existent-option
    "Option of command not-existent")

(defclass run-command-recipes-command ()
  ((base :initarg :base :accessor run-command-recipes-command-get-base)
   (options :initarg :options :accessor run-command-recipes-command-get-options)
   (selected-options :accessor run-command-recipes-command-selected-options
                     :initform nil))
  "Object helping with work with options of shell command.
BASE is base part of shell command.  For example, in command \"pandoc ...\"
base is \"pandoc\".  OPTIONS is list of options with type string for shell
command.  For example, \"--toc\", \"-disable-installer\" for pandoc.")

(defmethod initialize-instance :after
    ((command run-command-recipes-command)
     &key)
    (let ((options (oref command :options)))
        (oset command :options
              (run-command-recipes-command--parse-some-options options))))

(defun run-command-recipes-command--parse-some-options (from)
    "Parse FROM to normal options of `run-command-recipes-command'.
This is alist in which keys is names of options, values is options.
For example:
\((\"toc\" . \"--toc\") (\"disable-installer\" . \"-disable-installer\"))"
    (-map 'run-command-recipes-command--parse-option from))

(defun run-command-recipes-command--parse-option (from)
    "Parse FROM to normal `run-command-recipes-command' option.
FROM may be one of: string, cons from string and string.  Normal option is
cons from option name as `car', and option as `cdr'"
    (cl-typecase from (string `(,from . ,from)) (cons from)))

(defun run-command-recipes-command-get-option-with-name (name command)
    "Get option with name NAME from options of COMMAND."
    (cdr
     (assoc name (run-command-recipes-command-get-options command))))

(defun run-command-recipes-command-get-some-options-with-names (names command)
    "Get some options with names NAMES from options of COMMAND."
    (--map
     (run-command-recipes-command-get-option-with-name it command)
     names))

(defun run-command-recipes-command-get-option-names (command)
    "Get some options with names NAMES from options of COMMAND."
    (-map 'car (run-command-recipes-command-get-options command)))

(defun run-command-recipes-command-select-options (command options-names)
    "Select in object COMMAND some options with names OPTIONS-NAMES.
COMMAND created with `run-command-recipes-command'."
    (--reduce-from
     (run-command-recipes-command-select-one-option acc it)
     command
     options-names))

(defun run-command-recipes-command-select-one-option (command option-name)
    "Select in object COMMAND option with name OPTION-NAME.
COMMAND created with `run-command-recipes-command'."
    (run-command-recipes-command--ensure-existent-option command option-name)
    (setf
     (run-command-recipes-command-selected-options command)
     (cons option-name
           (run-command-recipes-command-selected-options command)))
    command)

(defun run-command-recipes-command--ensure-existent-option (command option-name)
    "Ensure that option with name OPTION-NAME existent for COMMAND."
    (unless (-contains-p
             (run-command-recipes-command-get-option-names command)
             option-name)
        (signal 'run-command-recipes-command-non-existent-option
                option-name)))

(defun run-command-recipes-command-collect (command)
    "Collect object COMMAND to shell command with type string."
    (let* ((base (run-command-recipes-command-get-base command))
           (selected-options-names
            (run-command-recipes-command-selected-options command))
           (selected-options
            (run-command-recipes-command-get-some-options-with-names
             selected-options-names command)))
        (s-concat base " " (s-join " " selected-options))))

(provide 'run-command-recipes-command)
;;; run-command-recipes-command.el ends here
