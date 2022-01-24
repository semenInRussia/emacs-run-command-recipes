;;; run-command-recipes-haskell.el --- Run Command's recipe for `haskell` -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Keywords: extensions run-command
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

(require 'run-command-recipes-project)

(require 'f)
(require 'dash)


(defcustom run-command-recipes-haskell-run-function
  (if (fboundp 'haskell-compile)
      'haskell-compile
      #'(user-error
         "Run-command-recipes-haskell.el: Install `haskell-mode', pls!"))
  "This function run when run recipe \"Run Haskell File by Context\"."
  :type 'function
  :group 'run-command-recipes-haskell)


(defcustom run-command-recipes-haskell-modes '(haskell-mode)
  "List of `major-modes` for Haskell."
  :type '(repeat function)
  :group 'run-command-recipes)


(defcustom run-command-recipes-haskell-mode-p
  (lambda () (-contains-p run-command-recipes-haskell-modes major-mode))
  "Predicate whic get t, when current `major-mode' is for Haskell."
  :type 'predicate
  :group 'run-command-recipes)


(defun run-command-recipes-haskell ()
    "`run-command''s recipe for `haskell`."
    (when (funcall run-command-recipes-haskell-mode-p)
        (list
         (list :command-name "stack-run"
               :command-line "stack run"
               :display "Run this Project with Stack"
               :working-dir (run-command-recipes-project-root))
         (list :command-name "haskell-run-by-context"
               :lisp-function run-command-recipes-haskell-run-function
               :display "Run Haskell File by Context"))))


(provide 'run-command-recipes-haskell)
;;; run-command-recipes-haskell.el ends here
