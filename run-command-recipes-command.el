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
    ("buffer-name"       . (buffer-name))
    ("any-string"        .
     (read-string (s-append ": " (or argument "Any string, please")))))
  "All normal for option of command variables alist, keys is quoted code."
  :type '(alist :key-type string :value-type list))

(define-error 'run-command-recipes-command-non-existent-option
    "Option of command not-existent")

(define-error 'run-command-recipes-command-non-existent-var-name-in-shell-code
    "In shell code with special syntax non-existent variable")

(defclass run-command-recipes-command ()
  ((name :initarg :name :accessor run-command-recipes-command--name)
   (base :initarg :base :accessor run-command-recipes-command-base)
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
command.  For example, \"--toc\", \"-disable-installer\" for pandoc.  SUFFIX
is string which will expand and append to final shell command.")

(defmethod initialize-instance :after
    ((command run-command-recipes-command)
     &key)
    (let ((options (oref command :options)))
        (oset command :options
              (run-command-recipes-command--parse-some-options options))))

(defun run-command-recipes-command-name (command)
    "Get name of COMMAND."
    (or
     (run-command-recipes-command--name command)
     (run-command-recipes-command-base command)))

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

(defun run-command-recipes-command-selected-options-shell-codes (command)
    "Return some shell codes of COMMAND's selected options."
    (-> command
        (run-command-recipes-command-selected-options)
        (run-command-recipes-command-get-some-options-with-names command)))

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
If option non-existent, then signal
`run-command-recipes-command-non-existent-option'."
    (run-command-recipes-command--ensure-existent-option command option-name)
    (setf
     (run-command-recipes-command-selected-options command)
     (cons option-name
           (run-command-recipes-command-selected-options command)))
    command)

(defun run-command-recipes-command-unselect-one-option (command option-name)
    "UnSelect in object COMMAND option with name OPTION-NAME.
If option non-existent, then signal
 `run-command-recipes-command-non-existent-option'"
    (run-command-recipes-command--ensure-existent-option command option-name)
    (setf
     (run-command-recipes-command-selected-options command)
     (remove option-name
             (run-command-recipes-command-selected-options command)))
    command)

(defun run-command-recipes-command-toggle-option (command option-name)
    "If option of COMMAND OPTION-NAME is selected, then unselect, else select.
If option non-existent, then signal
`run-command-recipes-command-non-existent-option'"
    (if (run-command-recipes-command-selected-option-p command option-name)
        (run-command-recipes-command-unselect-one-option command option-name)
        (run-command-recipes-command-select-one-option command option-name)))

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
                (run-command-recipes-command-selected-options-shell-codes)
                (run-command-recipes-command-expand-list-of-shell-code)))
           (suffix (run-command-recipes-command-suffix command))
           (words
            (->>
             (list base selected-options suffix)
             (-keep 'identity)
             (-flatten)
             (run-command-recipes-command-expand-list-of-shell-code))))
        (run-command-recipes-command-save-command-in-buffer command)
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
         shell-code used-vars)))

(defun run-command-recipes-command--find-variables-in-shell-code (shell-code)
    "Find all variables like on [current-directory] in SHELL-CODE.
Return list of lists from variable name and part of source in SHELL-CODE"
    (s-match-strings-all
     "\\[\\W*\\([^] ]*\\)\\W*:?W*\\([^]]*\\)?\\W*\\]" shell-code))

(defun run-command-recipes-command--replace-vars-in-shell-code-by-alist ;nofmt
    (shell-code ;nofmt
     used-vars)
    "Replace all USED-VARS in SHELL-CODE find var content in VARS-ALIST.
USED-VARS is list of lists from part of OPTION and used variable name."
    (--reduce-from
     (run-command-recipes-command--use-var-in-shell-code acc
                                                         (-second-item
                                                          it)
                                                         (-first-item it)
                                                         (-third-item it))
     shell-code used-vars))

(defun run-command-recipes-command--use-var-in-shell-code (shell-code ;nofmt
                                                           var-name
                                                           var-usage
                                                           argument)
    "Replace VAR-USAGE with value of VAR-NAME in VARS-LIST in SHELL-CODE.
Example of VAR-USAGE is [ current-directory   ]"
    (let* ((var-content
            (run-command-recipes-lookup-value-of-shell-code-var
             var-name var-usage argument)))
        (s-replace var-usage var-content shell-code)))

(defun run-command-recipes-lookup-value-of-shell-code-var (var-name ;nofmt
                                                           var-usage
                                                           argument)
    "Get value of VAR-NAME, which use in shell code in part of string VAR-USAGE.
Using for var ARGUMENT (any stuff, no required)

Example of shell code is:
\"--output-dir=[any-string : Type output directory]\"
Here VAR-USAGE is [current-directory : Type output directory].
And VAR-NAME is \"current-directory\".
ARGUMENT - \"Type output directory\""
    (-if-let
        (var-sexp
         (alist-get
          var-name
          run-command-recipes-command-variables-in-shell-code-alist
          nil nil 'equal))
        (eval var-sexp
              `((usage . ,var-usage)
                (argument . ,argument)))
        (signal
         'run-command-recipes-command-non-existent-var-name-in-shell-code
         (list var-name var-usage))))

(defun run-command-recipes-command-interactively-collect (command)
    "Select options of COMMAND via user, stop when user need."
    (while (run-command-recipes-command-p command)
        (setq command
              (run-command-recipes-command-interactively-toggle-one-option
               command)))
    command)

(defun run-command-recipes-command-interactively-toggle-one-option (command)
    "Select options of COMMAND, if user select, that collect to shell command."
    (->> command
         (run-command-recipes-command-get-options-names)
         (--map
          (run-command-recipes-command--append-option-suffix
           command it))
         (cons "*Already Ready*")
         (completing-read "Please Select Option of Shell Command:")
         (run-command-recipes-command--chop-option-suffix)
         (run-command-recipes-command--select-or-collect command)))

(defun run-command-recipes-command--select-or-collect (command
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
