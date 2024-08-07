;;; run-command-recipes-latex.el --- Recipe of `run-command' for `latex` -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1.0
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
(require 'run-command-recipes-project)

(declare-function 'latex/compile-commands-until-done "latex-extra")

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

(defcustom run-command-recipes-latexmk-config-filenames
  '(".latexmkrc" "latexmkrc")
  "Names of the `latexmk' config files indicating a LaTeX directory list."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defun run-command-recipes-latex ()
  "Recipe of `run-command' for LaTeX."
  (when (-contains-p run-command-recipes-latex-modes major-mode)
    (run-command-recipes-lib-compose-recipes
     'run-command-recipes-latex-pdflatex
     'run-command-recipes-latex-latexmk)))

(defun run-command-recipes-latex-pdflatex ()
  "Subrecipe of `run-command' for command tool pdflatex."
  (run-command-recipes-lib-build
   (list
    (list
     :display "PDFLaTeX: convert to PDF"
     :command-name "pdflatex"
     :command-line run-command-recipes-latex-command)
    (when (featurep 'latex-extra)
      (list
       :display "LaTeX: compile, build everything"
       :command-name "latex-compile-commands-until-done"
       :lisp-function 'latex/compile-commands-until-done)))))

(defun run-command-recipes-latex-latexmk-project-p (&optional root)
  "Return non-nil if the project at ROOT has the latexmk configuration file.

ROOT defaults to `run-command-recipes-project-root'"
  (or root (setq root (run-command-recipes-project-root)))
  (run-command-recipes-project-root-has-one-of
   run-command-recipes-latexmk-config-filenames))

(defun run-command-recipes-latex-latexmk ()
  "Subrecipe of `run-command' for command tool latexmk."
  (let ((working-dir (run-command-recipes-project-root)))
    (when (and
           working-dir
           (run-command-recipes-latex-latexmk-project-p working-dir)))
    (list
     (list
      :display "LaTeX MK: compile"
      :command-name "latexmk"
      :command-line "latexmk"
      :working-dir working-dir)
     (list
      :display "LaTeX MK: compile, preview"
      :command-name "latexmk-with-preview"
      :command-line "latexmk -pv"
      :working-dir working-dir))))

(provide 'run-command-recipes-latex)
;;; run-command-recipes-latex.el ends here
