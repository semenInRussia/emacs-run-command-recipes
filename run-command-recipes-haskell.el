;;; run-command-recipes-haskell.el --- Run Command's recipe for `haskell` -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1.0
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
;; (run-command-recipes-use 'haskell)
;; ```
;;; Code:
(require 'run-command-recipes-project)

(defcustom run-command-recipes-haskell-modes
  '(haskell-mode)
  "List of `major-mode' s for Haskell."
  :type '(repeat function)
  :group 'run-command-recipes)

(defun run-command-recipes-haskell-mode-p ()
  "Get t, when current `major-mode' is the mode for Haskell."
  (memq run-command-recipes-haskell-modes major-mode))

(defun run-command-recipes-haskell ()
  "`run-command' recipe for Haskell."
  (when (and (buffer-file-name)
             (executable-find "stack")
             (run-command-recipes-haskell-mode-p))
    (list
     (list
      :command-name "stack-run"
      :command-line "stack run"
      :display "Stack: run, execute project"
      :working-dir (run-command-recipes-project-root)))))

(provide 'run-command-recipes-haskell)
;;; run-command-recipes-haskell.el ends here
