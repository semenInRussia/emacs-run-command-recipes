;;; run-command-recipes.el --- This is collection of recipes to `run-command' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash        "2.18.0")
;;                    (s           "1.12.0")
;;                    (f           "0.20.0")
;;                    (run-command "0.1.0")
;;                    (emacs       "27.1"))

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
;; This is collection of recipes to `run-command'.

;;; Code:
(require 'run-command)

(add-to-list 'run-command-experiments
             'run-command-experiment-lisp-commands)

(defcustom rcr/supported-recipes '(latex
                                   pandoc
                                   haskell)
  "List of recipes' names, which `run-command-recipes' support."
  :type '(repeat symbol))


(defmacro run-command-recipes-use-all ()
    "Use all recipes for `run-command' from this package."
    `(run-command-recipes-use ,@rcr/supported-recipes)
    )


(defmacro run-command-recipes-use (&rest recipes)
    "Use RECIPES for `run-command' from this package."
    (--each recipes
        (run-command-recipes-use-one it))
    )


(defmacro run-command-recipes-useq-one (recipe)
    "Use RECIPE for `run-command' from `run-command-recipes' with quote RECIPE."
    (run-command-recipes-use-one ',recipe))


(defun run-command-recipes-use-one (recipe)
    "Use RECIPE for `run-command' from `run-command-recipes'."
    (progn
        (require (intern
                  (s-concat "run-command-recipes-" (symbol-name recipe))))
        (add-to-list 'run-command-recipes
                     (intern (s-concat "run-command-recipe-"
                                       (symbol-name recipe)))))
    )


(provide 'run-command-recipes)
;;; run-command-recipes.el ends here
