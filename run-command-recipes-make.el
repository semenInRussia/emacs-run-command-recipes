;;; run-command-recipes-make.el --- Recipe of `run-command' for GNU Make -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1.0
;; Keywords: extensions run-command
;; Homepage: https://github.com/semenInRussia/emacs-run-command-recipes
;; URL: https://github.com/semenInRussia/emacs-run-command-recipes/blob/main/docs/make.md

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
;; To use this recipe put the following to your Emacs configuration:
;;
;; (run-command-recipes-use-one 'make)
;;
;;; Code:

(require 'dash)
(require 'f)
(require 'run-command-recipes-project)


(defcustom run-command-recipes-make-executable "make"
  "Path to the GNU Make executable."
  :group 'run-command-recipes
  :type 'string)

(defcustom run-command-recipes-makefile-names '("Makefile" "Make")
  "The list of names in which can be defined commands for GNU Make."
  :group 'run-command-recipes
  :type '(repeat string))

(defun run-command-recipes-make ()
  "Recipe of `run-command' for GNU Make."
  (and
   (run-command-recipes-make-project-p)
   (executable-find run-command-recipes-make-executable)
   (->>
    (run-command-recipes-make--project-makefile)
    (run-command-recipes-make--commands)
    (--map
     (list
      :command-name (concat "make-" it)
      :command-line (concat "make " it)
      :display (concat "Make: `" it "'"))))))

(defun run-command-recipes-make--commands (makefile)
  "Return the list of commands names that was defined in MAKEFILE."
  (let ((s (f-read makefile))
        (commands nil)
        (pos 0))
    (while (string-match "^\\([^ \n]+\\):" s pos)
      (push (match-string 1 s) commands)
      (setq pos (match-end 0)))
    commands))

(defun run-command-recipes-make--project-makefile (&optional root)
  "Return path to the Makefile of the project at ROOT.

ROOT defaults to value of `run-command-recipes-project-root'"
  (or root (setq root (run-command-recipes-project-root)))
  (f-join root "Makefile"))

(defun run-command-recipes-make-project-p (&optional root)
  "Return non-nil if the project at ROOT has Makefile.

ROOT defaults to value of `run-command-recipes-project-root'"
  (or root (setq root (run-command-recipes-project-root)))
  (run-command-recipes-project-root-has-one-of run-command-recipes-makefile-names))

(provide 'run-command-recipes-make)
;;; run-command-recipes-make.el ends here
