;;; run-command-recipes-java.el --- Recipe of `run-command' for `java` -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1.0
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

(require 'run-command-recipes-project)

(defcustom run-command-recipes-java-javac "javac"
  "Path to the executable of javac: compiler for java."
  :group 'run-command-recipes
  :type 'string)

(defcustom run-command-recipes-java-javac-flags "-Werror"
  "A string of arguments which will be passed to Javac when use this recipe."
  :group 'run-command-recipes
  :type 'string)

(defcustom run-command-recipes-java-modes
  '(java-mode)
  "List of major modes in which the `run-command' recipe for Java should work."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defun run-command-recipes-java--exe-name (filename &optional dir)
  "Get the filename for executable to produce when compile FILENAME java file.

Defaults to just chop extension, like main.java => main

Consider that the command will be ran inside DIR"
  (concat (or dir "")
          (file-name-base filename)))

(defun run-command-recipes-java ()
  "Recipe of `run-command' for Java."
  (when (and (buffer-file-name)
             (run-command-recipes-java-p)
             (executable-find run-command-recipes-java-javac))
    (let* ((cmd run-command-recipes-java-javac)
           (dir (run-command-recipes-project-root))
           (exe (run-command-recipes-java--exe-name (buffer-file-name)
                                                    dir)))
      (list
       (list
        :command-name "run-java-file"
        :display "Java: compile, execute file"
        :working-dir dir
        :command-line
        (concat cmd " "
                run-command-recipes-java-javac-flags
                " -o " exe
                " " (buffer-file-name)
                " && " exe))
       (list
        :command-name "Java: compile file"
        :display "Java: compile file"
        :working-dir dir
        :command-line
        (concat cmd " " (buffer-file-name)
                " " run-command-recipes-java-javac-flags
                " -o " exe))))))

(defun run-command-recipes-java-p ()
  "Return t, when the recipe of `run-command' for Java should work."
  (and
   (buffer-file-name)
   (-contains-p run-command-recipes-java-modes major-mode)))

(provide 'run-command-recipes-java)
;;; run-command-recipes-java.el ends here
