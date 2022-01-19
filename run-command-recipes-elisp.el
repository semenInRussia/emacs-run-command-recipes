;;; run-command-recipes-elisp.el --- Recipe of `run-command' for `elisp` -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
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
;; For use this code put this to config:
;; ```
;; (run-command-recipes-use-one 'elisp)
;; ```
;;; Code:

(require 'dash)
(require 'f)
(require 'run-command-recipes-files)


(defcustom run-command-recipes-elisp-modes
  '(emacs-lisp-mode lisp-interaction-mode)
  "List of modes in which can work recipe `elisp' for `run-command'."
  :type '(repeat symbol)
  :group 'run-command-recipes)


(defcustom run-command-recipes-elisp-mode-p-funtion
  (lambda () (-contains-p run-command-recipes-elisp-modes major-mode))
  "Predicate, return non-nil value, if current `major-mode' is Emacs Lisp mode."
  :type 'predicate
  :group 'run-command-recipes)


(defun run-command-recipes-elisp-mode-p ()
    "Get t, when current `major-mode' is Emacs Lisp mode."
    (funcall run-command-recipes-elisp-mode-p-funtion))


(defcustom run-command-recipes-elisp-cask-project-p-function nil
  "Get non-nil when current opened project have Cask."
  :type 'predicate
  :group 'run-command-recipes)


(defcustom run-command-recipes-elisp-cask-filename "Cask"
  "Name of cask file."
  :type 'string
  :group 'run-command-recipes)


(defun run-command-recipes-elisp-cask-project-p ()
    "Get non-nil when current opened project have Cask."
    (if run-command-recipes-elisp-cask-project-p-function
        (funcall run-command-recipes-elisp-cask-project-p-function)
        (let ((project-root-files (->> (run-command-recipes-project-root)
                                       (f-files)
                                       (--map (f-filename it)))))
            (-contains-p project-root-files
                         run-command-recipes-elisp-cask-filename))))


(defun run-command-recipes-elisp-file-was-compiled-p ()
    "Return t when current file was run in `byte-compile'."
    (-when-let (filename (buffer-file-name))
        (f-file-p (f-swap-ext filename "elc"))))


(defun run-command-recipes-elisp-compile-current-file ()
    "Compile to bytes current Elisp file."
    (byte-compile-file (buffer-file-name)))


(defun run-command-recipes-elisp-recompile-current-file ()
    "Recompile to bytes current Elisp file."
    (byte-recompile-file (buffer-file-name)))


(defun run-command-recipes-elisp-recompile-current-directory ()
    "Recompile to bytes current directory."
    (byte-recompile-directory (run-command-recipes-project-root)))


(defun run-command-recipe-elisp ()
    "This is recipe for `run-command' from `run-command-recipes'."
    (list
     (when (run-command-recipes-elisp-cask-project-p)
         (list
          :command-name "cask-install"
          :display "Install Dependecies from Cask"
          :command-line "cask"))
     (when (run-command-recipes-elisp-mode-p)
         (if (run-command-recipes-elisp-file-was-compiled-p)
             (list
              :command-name "byte-recompile-file"
              :lisp-function 'run-command-recipes-elisp-recompile-current-file
              :display "ReCompile to Bytes This File")
             (list
              :command-name "byte-compile-file"
              :lisp-function 'run-command-recipes-elisp-compile-current-file
              :display "Compile to Bytes This File")))
     (when (run-command-recipes-elisp-mode-p)
         (list
          :command-name "byte-recompile-directory"
          :display "ReCompile to bytest Current Directory"
          :lisp-function
          'run-command-recipes-elisp-recompile-current-directory))))


(provide 'run-command-recipes-elisp)
;;; run-command-recipes-elisp.el ends here
