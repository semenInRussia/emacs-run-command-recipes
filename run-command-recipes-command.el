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
(require 'run-command-recipes-project)


(defcustom run-command-recipes-command-variables-in-shell-code-alist
  '(("current-directory" . default-directory)
    ("project-root"      . (run-command-recipes-project-root))
    ("file-name"         . (buffer-file-name))
    ("file-extension"    . (-some-> (buffer-file-name) (f-ext)))
    ("buffer-name"       . (buffer-name)))
  "All normal for option of command variables alist, keys is quoted code."
  :type '(alist :key-type string :value-type list))

(define-error 'run-command-recipes-command-non-existent-option
    "Option of command not-existent")

(define-error 'run-command-recipes-command-non-existent-var-name-in-shell-code
    "In shell code with special syntax non-existent variable")

(defclass run-command-recipes-command ()
  ((base :initarg :base :accessor run-command-recipes-command-base)
   (options :initarg :options
            :accessor run-command-recipes-command-options
            :initform nil)
   (suffix :initarg :suffix
           :accessor run-command-recipes-command-suffix
           :initform nil)
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
    (cdr (assoc name (run-command-recipes-command-options command))))

(defun run-command-recipes-command-get-some-options-with-names (names command)
    "Get some options with names NAMES from options of COMMAND."
    (--map
     (run-command-recipes-command-get-option-with-name it command)
     names))

(defun run-command-recipes-command-get-options-names (command)
    "Get some options with names NAMES from options of COMMAND."
    (-map 'car (run-command-recipes-command-options command)))

(defun run-command-recipes-command-select-options (command options-names)
    "Select in object COMMAND some options with names OPTIONS-NAMES.
COMMAND created with `run-command-recipes-command'."
    (--reduce-from
     (run-command-recipes-command-select-one-option acc it)
     command
     options-names))

(defun run-command-recipes-command-selected-option-p (command option-name)
    "Return t, when option of COMMAND with name OPTION-NAME was selected."
    (-contains-p
     (run-command-recipes-command-selected-options command)
     option-name))

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
    (unless (run-command-recipes-command-existent-option-p command option-name)
        (signal 'run-command-recipes-command-non-existent-option
                option-name)))

(defun run-command-recipes-command-existent-option-p (command option-name)
    "Return t, when OPTION-NAME is name of existent option of COMMAND."
    (-contains-p
     (run-command-recipes-command-get-options-names command)
     option-name))

(defun run-command-recipes-command-collect (command)
    "Collect object COMMAND to shell command with type string."
    (let* ((base (run-command-recipes-command-base command))
           (selected-options
            (-> command
                (run-command-recipes-command-selected-options)
                (run-command-recipes-command-get-some-options-with-names
                 command)
                (run-command-recipes-command-expand-list-of-shell-code)))
           (suffix (run-command-recipes-command-suffix command))
           (words
            (->>
             (list base selected-options suffix)
             (-keep 'identity)
             (-flatten)
             (run-command-recipes-command-expand-list-of-shell-code))))
        (s-join " " words)))

(defun run-command-recipes-command-expand-list-of-shell-code (shell-codes)
    "Expand each of SHELL-CODES, with special syntax.
Example of special syntax:

[current-directory] => value of `default-directory'"
    (-map 'run-command-recipes-command-expand-shell-code shell-codes))

(defun run-command-recipes-command-expand-shell-code (shell-code)
    "Expand SHELL-CODE with special syntax, [current-directory] is example."
    (let* ((used-vars
            (run-command-recipes-command--find-variables-in-shell-code
             shell-code)))
        (run-command-recipes-command--replace-vars-in-shell-code-by-alist
         shell-code used-vars
         run-command-recipes-command-variables-in-shell-code-alist)))

(defun run-command-recipes-command--find-variables-in-shell-code (shell-code)
    "Find all variables like on [current-directory] in SHELL-CODE.
Return list of lists from variable name and part of source in SHELL-CODE"
    (s-match-strings-all "\\[\\W*\\([^\] ]*\\)\\W*\\]" shell-code))

(defun run-command-recipes-command--replace-vars-in-shell-code-by-alist ;nofmt
    (shell-code ;nofmt
     used-vars
     vars-alist)
    "Replace all USED-VARS in SHELL-CODE find var content in VARS-ALIST.
USED-VARS is list of lists from part of OPTION and used variable name.
VARS-ALIST is alist where keys is variables' names, values is variables'
values"
    (--reduce-from
     (run-command-recipes-command--use-var-in-shell-code acc
                                                         (-second-item
                                                          it)
                                                         (-first-item it)
                                                         vars-alist)
     shell-code used-vars))

(defun run-command-recipes-command--use-var-in-shell-code (shell-code ;nofmt
                                                           var-name
                                                           var-usage
                                                           vars-alist)
    "Replace VAR-USAGE with value of VAR-NAME in VARS-LIST in SHELL-CODE.
Example of VAR-USAGE is [ current-directory   ]"
    (let* ((var-code
            (or
             (alist-get var-name vars-alist nil nil 'equal)
             (signal
              'run-command-recipes-command-non-existent-var-name-in-shell-code
              (list var-name shell-code))))
           (var-content (eval var-code)))
        (s-replace var-usage var-content shell-code)))

(defun run-command-recipes-command-interactively-collect (command)
    "Select options of COMMAND, collect its to shell command by user."
    (->> command
         (run-command-recipes-command-get-options-names)
         (--map
          (run-command-recipes-command--append-option-suffix command it))
         (cons "*Already Ready*")
         (completing-read "Please Select Option of Shell Command:")
         (run-command-recipes-command--chop-option-suffix)
         (run-command-recipes-command--interactively-select-or-collect
          command)))

(defun run-command-recipes-command--interactively-select-or-collect (command
                                                                     ;;nofmt
                                                                     option)
    "If OPTION is existent for COMMAND, then select, otherwise collect COMMAND."
    (if (run-command-recipes-command-existent-option-p command option)
        (run-command-recipes-command-select-one-option command option)
        (run-command-recipes-command-collect command)))

(defun run-command-recipes-command--append-option-suffix (command option-name)
    "Add right suffix to OPTION-NAME of COMMAND, depends on selection state."
    (s-append
     (if (run-command-recipes-command-selected-option-p command option-name)
         " (selected)"
         " (non selected)")
     option-name))

(defun run-command-recipes-command--chop-option-suffix (option-name)
    "Chop suffix of OPTION-NAME of COMMAND, depends on selection state."
    (s-chop-suffixes '(" (selected)" " (non selected)") option-name))

(provide 'run-command-recipes-command)
;;; run-command-recipes-command.el ends here
