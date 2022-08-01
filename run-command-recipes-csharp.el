;;; run-command-recipes-csharp.el --- Recipe of `run-command' for c# -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Keywords: extensions run-command
;; Homepage: https://github.com/semenInRussia/emacs-run-command-recipes
;; URL: https://github.com/semenInRussia/emacs-run-command-recipes/blob/main/docs/csharp.md

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
;; (run-command-recipes-use-one 'csharp)
;;
;;; Code:

(require 'dash)
(require 'f)
(require 'run-command-recipes-lib)

(defcustom run-command-recipes-csharp-modes
  '(csharp-mode)
  "List of major modes in which the recipe of `run-command' for c# should work."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defun run-command-recipes-csharp-p ()
  "Return t, when `run-command' recipe for c# should work."
  (-contains-p run-command-recipes-csharp-modes major-mode))

(defun run-command-recipes-csharp ()
  "Recipe of `run-command' for c#."
  (when (and (executable-find "dotnet") (run-command-recipes-csharp-p))
    (list
     :command-name "run-dotnet-project"
     :display "Run .NET Project"
     :command-line "dotnet run")
    (list
     :command-name "compile-dotnet-project"
     :display "Only Compile .NET Project"
     :command-line "dotnet build")))

(provide 'run-command-recipes-csharp)
;;; run-command-recipes-csharp.el ends here
