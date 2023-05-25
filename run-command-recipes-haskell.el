;;; run-command-recipes-haskell.el --- Run Command's recipe for `haskell` -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.3
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

;; For use this code put the following to your Emacs configuration:
;; ```
;; (run-command-recipes-use-one 'haskell)
;; ```
;;; Code:

(require 'run-command-recipes-lib)
(require 'run-command-recipes-project)

(require 'f)
(require 'dash)

(defcustom run-command-recipes-haskell-run-function
  (if (fboundp 'haskell-compile)
      'haskell-compile
    #'(user-error
       "Run-command-recipes-haskell.el: Install `haskell-mode', pls!"))
  "Run when the recipe \"Run Haskell File by Context\" was choosen."
  :type 'function
  :group 'run-command-recipes-haskell)

(defcustom run-command-recipes-haskell-modes
  '(haskell-mode)
  "List of `major-modes' for Haskell."
  :type '(repeat function)
  :group 'run-command-recipes)

(defun run-command-recipes-haskell-mode-p ()
  "Get t, when current `major-mode' is the mode for Haskell."
  (-contains-p run-command-recipes-haskell-modes major-mode))

(defun run-command-recipes-haskell ()
  "`run-command' recipe for Haskell."
  (when (and (buffer-file-name) (run-command-recipes-haskell-mode-p))
    (run-command-recipes-lib-build
     (list
      (list
       :command-name "stack-run"
       :command-line "stack run"
       :display "Stack: run, execute project"
       :working-dir (run-command-recipes-project-root))
      (list
       :command-name "haskell-run-by-context"
       :lisp-function run-command-recipes-haskell-run-function
       :display "Haskell by context")))))

(provide 'run-command-recipes-haskell)
;;; run-command-recipes-haskell.el ends here
