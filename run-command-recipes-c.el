;;; run-command-recipes-c.el --- Recipe of `run-command' for c -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.3
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

;; (run-command-recipes-use-one 'c)

;;; Code:

(require 'dash)
(require 'f)
(require 'run-command-recipes-project)
(require 'run-command-recipes-lib)

(defcustom run-command-recipes-c-major-modes
  '(c-mode)
  "List of major modes in which the recipe for C should work."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defcustom run-command-recipes-c-flags "-Wall -Werror"
  "String which specify flags which are passed to a C compiler."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-c-subrecipes
  '(run-command-recipes-c-gcc run-command-recipes-c-clang)
  "List of subrecipes of `run-command' recipe for C."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defun run-command-recipes-c ()
  "Recipe of `run-command' for C."
  (when (run-command-recipes-c-p)
    (apply #'run-command-recipes-lib-compose-recipes
           run-command-recipes-c-subrecipes)))

(defun run-command-recipes-c-p ()
  "Return t, when recipe of `run-command' for C should work."
  (and
   (buffer-file-name)
   (run-command-recipes-c-major-mode-p major-mode)))

(defun run-command-recipes-c-major-mode-p (mode)
  "Return t, when in major mode MODE c-recipe should work."
  (-contains-p run-command-recipes-c-major-modes mode))

(defun run-command-recipes-c-gcc ()
  "Recipe of `run-command' fort the gcc compiler of c.

Subrecipe of recipe for C."
  (when (and (executable-find "gcc") (buffer-file-name))
    (run-command-recipes-lib-build
     (list
      (list
       :display "GCC: compile, execute file"
       :command-line
       (concat "gcc {file-name} "
               run-command-recipes-c-flags
               " && {file-name-no-ext}")
       :command-name "gcc-compile-and-exec")
      (list
       :display "GCC: compile file"
       :command-line
       (concat "gcc {file-name} "
               run-command-recipes-c-flags
               " -o {file-name-no-ext}")
       :command-name "gcc-only-compile")))))

(defun run-command-recipes-c-clang ()
  "Recipe of `run-command' for clang compiler of C.

Subrecipe of recipe for C."
  (when (and (executable-find "clang") (buffer-file-name))
    (run-command-recipes-lib-build
     (list
      (list
       :display "CLang: compile, execute file"
       :command-line
       (concat "clang {file-name} -o {file-name-no-ext} && {file-name-no-ext} "
               run-command-recipes-c-flags)
       :command-name "clang-compile-and-exec")
      (list
       :display "Clang: compile file'"
       :command-line
       (concat  "clang {file-name} -o {file-name-no-ext} "
                run-command-recipes-c-flags)
       :command-name "clang-only-compile")))))

(provide 'run-command-recipes-c)
;;; run-command-recipes-c.el ends here
