;;; run-command-recipes-latex.el --- Recipe of `run-command' for `latex` -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash        "2.18.0")
;;                    (s           "1.12.0")
;;                    (f           "0.20.0")
;;                    (run-command "0.1.0")
;;                    (emacs       "27.1"))

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

;;; Code:
(require 'dash)
(require 'f)
(require 's)
(require 'run-command)


(defcustom rcr/pdflatex-command (s-concat "pdflatex"
                                      " -interaction nonstopmode"
                                      " -file-line-error"
                                      " --output-directory \"%s\""
                                      " \"%s\"")
  "Command for run `pdflatex`, ignoring errors."
  :type 'string)


(defcustom rcr/tex-modes '(LaTeX-mode latex-mode tex-mode TeX-mode)
  "List of tex/latex major modes in Emacs."
  :type '(repeat symbol))


(defun run-command-recipe-latex ()
    "Recipe for LaTeX `run-command'.
See https://github.com/bard/emacs-run-command#examples."
    (-when-let (file-path (buffer-file-name))
        (when (-contains-p rcr/tex-modes major-mode)
            (list
             (list
              :display "Convert to PDF with `pdflatex`, ignoring errors"
              :command-name "pdflatex"
              :command-line (format
                             rcr/pdflatex-command (f-dirname file-path)
                             file-path
                             )
              )))
        )
    )


(provide 'run-command-recipes-latex)
;;; run-command-recipes-latex.el ends here
