;;; run-command-recipes-haskell --- `run-command''s recipe for `haskell`. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Packages-Requires: ((dash        "2.18.0")
;;                     (s           "1.12.0")
;;                     (f           "0.20.0")
;;                     (run-command "0.1.0")
;;                     (emacs       "27.1"))


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

;;; Code:

(require 'run-command)
(require 'run-command-recipes-files)
(require 'f)

(defun run-command-recipe-haskell ()
    "`run-command''s recipe for `haskell`."
    (list
     (when (or (eq major-mode 'haskell-mode)
               (equal (f-base (buffer-file-name))
                      "Cabal"))
         (list
          :command-name "stack-run"
          :command-line "stack run"
          :display "Run this Project with Stack"
          :working-dir (rcr/project-root)))
     (when (eq major-mode 'haskell-mode)
         (list
          :command-name "ghci-run"
          :lisp-function 'haskell-compile
          :display "Run this Project with ghci"
          ))
     )
    )



(provide 'run-command-recipes-haskell)
;;; run-command-recipes-haskell.el ends here
