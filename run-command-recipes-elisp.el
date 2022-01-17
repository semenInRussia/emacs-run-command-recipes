;;; run-command-recipes-elisp.el --- Recipe of `run-command' for `elisp` -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (dash "2.18.0") (s "1.12.0") (f "0.20.0") (run-command "0.1.0"))
;; Keywords: extensions, run-command
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
;; For use this code put this to config:
;; ```
;; (run-command-recipes-use-one 'elisp)
;; ```
;;; Code:

(require 'dash)
(require 'f)
(require 'run-command-recipes-files)


(defcustom rcr/elisp-modes '(emacs-lisp-mode lisp-interaction-mode)
  "List of modes in which can work recipe `elisp' for `run-command'."
  :type '(repeat symbol)
  :group 'rcr)


(defcustom rcr/elisp-cask-project-p-function nil
  "Get non-nil when current opened project have Cask."
  :type 'predicate
  :group 'rcr)


(defcustom rcr/elisp-cask-filename "Cask"
  "Name of cask file."
  :type 'string
  :group 'rcr)


(defun rcr/elisp-cask-project-p ()
    "Get non-nil when current opened project have Cask."
    (if rcr/elisp-cask-project-p-function
        (funcall rcr/elisp-cask-project-p-function)
        (let ((project-root-files (->> (rcr/project-root)
                                       (f-files)
                                       (--map (f-filename it)))))
            (-contains-p project-root-files rcr/elisp-cask-filename))))


(defun rcr/elisp-file-was-compiled-p ()
    "Return t when current file was run in `byte-compile'."
    (-when-let (filename (buffer-file-name))
        (f-file-p (f-swap-ext filename "elc"))))


(defun rcr/elisp-compile-current-file ()
    "Compile to bytes current Elisp file."
    (byte-compile-file (buffer-file-name)))


(defun rcr/elisp-recompile-current-file ()
    "Recompile to bytes current Elisp file."
    (byte-recompile-file (buffer-file-name)))


(defun rcr/elisp-recompile-current-directory ()
    "Recompile to bytes current directory."
    (byte-recompile-directory (rcr/project-root)))


(defun run-command-recipe-elisp ()
    "This is recipe for `run-command' from `run-command-recipes'."
    (list
     (when (rcr/elisp-cask-project-p)
         (list
          :command-name "cask-install"
          :display "Install Dependecies from Cask"
          :command-line "cask"))
     (if (rcr/elisp-file-was-compiled-p)
         (list
          :command-name "byte-recompile-file"
          :lisp-function 'rcr/elisp-recompile-current-file
          :display "ReCompile to Bytes This File")
         (list
          :command-name "byte-compile-file"
          :lisp-function 'rcr/elisp-compile-current-file
          :display "Compile to Bytes This File"))
     (list
      :command-name "byte-recompile-directory"
      :display "ReCompile to bytest Current Directory"
      :lisp-function 'rcr/elisp-recompile-current-directory)))


(provide 'run-command-recipes-elisp)
;;; run-command-recipes-elisp.el ends here
