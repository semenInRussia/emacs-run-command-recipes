;;; run-command-recipes-c.el --- Recipe of `run-command' for C -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1.0
;; Keywords: extensions run-command
;; Homepage: https://github.com/semenInRussia/emacs-run-command-recipes
;; URL: https://github.com/semenInRussia/emacs-run-command-recipes/blob/main/docs/c.md

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

;; (run-command-recipes-use 'c)

;;; Code:
(require 'run-command-recipes-project)

(defcustom run-command-recipes-c-major-modes
  '(c-mode)
  "List of major modes in which the recipe for C should work."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defcustom run-command-recipes-c-flags "-Wall -Werror"
  "String which specify flags which are passed to a C compiler."
  :type 'string
  :group 'run-command-recipes)

(defun run-command-recipes-c-p ()
  "Return t, when recipe of `run-command' for C should work."
  (and (buffer-file-name)
       (memq major-mode run-command-recipes-c-major-modes)))

(defun run-command-recipes-c--exe-name (filename &optional dir)
  "Return the filename for executable to produce when compile FILENAME C file.

Defaults to just chop extension, like main.c => main

Consider that the command will be ran inside DIR"
  (concat (or dir "")
          (file-name-base filename)))

;; NOTE: that I provide two different recipes functions (for clang and
;; GCC) which are almost the same, but both of them are useful for C.
;;
;; The more naive way is to write one function for clang and GCC, but
;; in this case user can't delete avoid usage one of them.  Now you
;; can do anything like:
;;
;; \\(advice-add 'run-command-recipes-c-gcc :around #'ignore)
;;
;; and disable GCC support

(defun run-command-recipes-c ()
  "Support of C to execute the current file using `run-command'.

See either `run-command-recipes-c-gcc' or
`run-command-recipes-c-clang' for details"
  (append (run-command-recipes-c-gcc)
          (run-command-recipes-c-clang)))

(defun run-command-recipes-c-gcc ()
  "Support of GCC compiler of C to execute the current file using `run-command'.

Recipe of `run-command' for support of GCC compiler of C to execute
the current file with it.  See `run-command-recipes' (variable) if
don't know what recipe means.

NOTE that if you prefer the Clang compiler and don't need to see when
`run-command' inside C buffer, then you should use `fset' or
`advice-add' with `ignore' over `run-command-recipes-c-gcc'.

\\(advice-add \\='run-command-recipes-c-gcc :around #\\='ignore)"
  (when (and (executable-find "gcc")
             (buffer-file-name)
             (run-command-recipes-c-p))
    (let* ((dir (run-command-recipes-project-root))
           (exe (run-command-recipes-c--exe-name (buffer-file-name) dir)))
      (list
       (list
        :command-name "gcc-compile-and-exec"
        :display "GCC: compile, execute file"
        :working-directory dir
        :command-line
        (concat "gcc " (buffer-file-name)
                " " run-command-recipes-c-flags
                " -o " exe
                " && " exe))
       (list
        :command-name "gcc-only-compile"
        :display "GCC: compile file"
        :working-directory dir
        :command-line
        (concat "gcc " (buffer-file-name)
                " " run-command-recipes-c-flags
                " -o " exe))))))

(defun run-command-recipes-c-clang ()
  "Support of Clang compiler to execute the current C file using `run-command'.

Recipe of `run-command' for support of Clang compiler of C to execute
the current file with it.  See `run-command-recipes' (variable) if
don't know what recipe means.

NOTE that if you prefer the GCC compiler and don't need to see when
`run-command' inside C buffer, then you should use `fset' or
`advice-add' with `ignore' over `run-command-recipes-c-clang'.

\\(advice-add \\='run-command-recipes-c-clang :around #\\='ignore)"
  (when (and (executable-find "clang")
             (buffer-file-name)
             (run-command-recipes-c-p))
    (let* ((dir (run-command-recipes-project-root))
           (exe (run-command-recipes-c--exe-name (buffer-file-name) dir)))
      (list
       (list
        :command-name "clang-compile-and-exec"
        :display "Clang: compile, execute file"
        :working-directory dir
        :command-line
        (concat "clang " (buffer-file-name)
                " " run-command-recipes-c-flags
                " -o " exe
                " && " exe))
       (list
        :command-name "clang-only-compile"
        :display "Clang: compile file"
        :working-directory dir
        :command-line
        (concat "clang " (buffer-file-name)
                " " run-command-recipes-c-flags
                " -o " exe))))))

(provide 'run-command-recipes-c)
;;; run-command-recipes-c.el ends here
