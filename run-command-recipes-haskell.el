;;; run-command-recipes-haskell.el --- Run Command's recipe for `haskell` -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (dash "2.18.0") (s "1.12.0") (f "0.20.0") (run-command "0.1.0") (haskell-mode "1.4"))
;; Keywords: extensions, run-command
;; Homepage: https://github.com/semenInRussia/emacs-run-command-recipes
;; URL: https://github.com/semenInRussia/emacs-run-command-recipes/blob/main/docs/haskell.md


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
;; (run-command-recipes-use-one 'haskell)
;; ```
;;; Code:

(require 'run-command-recipes-files)

(require 'f)
(require 'dash)
(require 'haskell-mode)


(defcustom rcr/haskell-complie-function 'haskell-compile
  "This function run when run recipe \"Run Haskell File by Context\"."
  :type 'function
  :group 'rcr)


(defcustom rcr/haskell-modes '(haskell-mode)
  "List of `major-modes` for Haskell."
  :type '(repeat function)
  :group 'rcr)


(defcustom rcr/haskell-mode-p
  (lambda () (-contains-p rcr/haskell-modes major-mode))
  "Predicate whic get t, when current `major-mode' is for Haskell."
  :type 'predicate
  :group 'rcr)


(defun run-command-recipe-haskell ()
    "`run-command''s recipe for `haskell`."
    (when (funcall rcr/haskell-mode-p)
        (list
         (list :command-name "stack-run"
               :command-line "stack run"
               :display "Run this Project with Stack"
               :working-dir (rcr/project-root))
         (list :command-name "haskell-run-by-context"
               :lisp-function rcr/haskell-complie-function
               :display "Run Haskell File by Context")))
    )


(provide 'run-command-recipes-haskell)
;;; run-command-recipes-haskell.el ends here
