;;; run-command-recipes-latex.el --- Recipe of `run-command' for `latex` -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.3
;; Keywords: extensions, run-command
;; Homepage: https://github.com/semenInRussia/emacs-run-command-recipes
;; URL: https://github.com/semenInRussia/emacs-run-command-recipes/blob/main/docs/latex.md

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
;; (run-command-recipes-use-one 'latex)
;;
;;; Code:

(require 'run-command-recipes-lib)

(defcustom run-command-recipes-latex-command
  (concat "pdflatex"
          " -interaction nonstopmode"
          " -file-line-error"
          " --output-directory \"{current-dir}\""
          " \"{file-name}\"")
  "Command running pdflatex, ignoring errors."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-latex-modes
  '(LaTeX-mode latex-mode tex-mode TeX-mode)
  "List of tex/latex major modes in the Emacs."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defun run-command-recipes-latex ()
  "Recipe of `run-command' for LaTeX."
  (run-command-recipes-lib-bind-in-recipe
   (when (-contains-p run-command-recipes-latex-modes major-mode)
     (list
      (list
       :display "Convert to PDF with `pdflatex`, ignoring errors"
       :command-name "pdflatex"
       :command-line run-command-recipes-latex-command)))))

(provide 'run-command-recipes-latex)
;;; run-command-recipes-latex.el ends here
