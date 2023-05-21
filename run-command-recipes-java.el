;;; run-command-recipes-java.el --- Recipe of `run-command' for `java` -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.3
;; Keywords: extensions run-command
;; Homepage: https://github.com/semenInRussia/emacs-run-command-recipes
;; URL: https://github.com/semenInRussia/emacs-run-command-recipes/blob/main/docs/java.md

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
;; (run-command-recipes-use-one 'java)
;;
;;; Code:

(require 'run-command-recipes-lib)

(defcustom run-command-recipes-java-run-file-command
  "javac {file-name} && {file-name-no-ext}"
  "Command running current Java source file."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-java-compile-file-command
  "javac -Werror {file-name}"
  "Command only compiling current Java file.

See `run-command-recipes-lib-build' for understand dinamic replaces in
the current shell command"
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-java-modes
  '(java-mode)
  "List of major modes in which the `run-command' recipe for Java should work."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defun run-command-recipes-java ()
  "Recipe of `run-command' for Java."
  (run-command-recipes-lib-build
   (when (and (run-command-recipes-java-p) (executable-find "javac"))
     (list
      (list
       :command-name "run-java-file"
       :display "Run Current Java File"
       :command-line run-command-recipes-java-run-file-command)
      (list
       :command-name "compile-java-file"
       :display "Compile Current Java File"
       :command-line run-command-recipes-java-compile-file-command)))))

(defun run-command-recipes-java-p ()
  "Return t, when the recipe of `run-command' for Java should work."
  (and
   (buffer-file-name)
   (-contains-p run-command-recipes-java-modes major-mode)))

(provide 'run-command-recipes-java)
;;; run-command-recipes-java.el ends here
