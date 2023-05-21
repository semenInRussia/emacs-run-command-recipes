;;; run-command-recipes-cpp.el --- Recipe of `run-command' for c++ -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.3
;; Keywords: extensions run-command
;; Homepage: https://github.com/semenInRussia/emacs-run-command-recipes
;; URL: https://github.com/semenInRussia/emacs-run-command-recipes/blob/main/docs/cpp.md

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
;; (run-command-recipes-use-one 'cpp)
;;
;;; Code:

(require 'dash)
(require 'f)
(require 'run-command-recipes-project)
(require 'run-command-recipes-lib)

(defcustom run-command-recipes-cpp-modes
  '(c++-mode)
  "List of all modes in which the recipe for C++ should work."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defun run-command-recipes-cpp-p ()
  "Return t, when recipe for C++ should work."
  (and
   (buffer-file-name)
   (-contains-p run-command-recipes-cpp-modes major-mode)))

(defun run-command-recipes-cpp ()
  "Recipe of `run-command' for c++."
  (run-command-recipes-lib-build
   (when (and (run-command-recipes-cpp-p) (executable-find "g++"))
     (list
      (list
       :command-name "run-cpp-file"
       :display "Compile and Execute Current C++ File via `g++'"
       :command-line                    ;nofmt
       "g++ {file-name} -o {file-name-no-ext} && {file-name-no-ext}")
      (list
       :command-name "compile-cpp-file"
       :display "Compile Only Current C++ File via `g++'"
       :command-line "g++ -Wall -Werror {file-name} -o {file-name-no-ext}")))))

(provide 'run-command-recipes-cpp)
;;; run-command-recipes-cpp.el ends here
