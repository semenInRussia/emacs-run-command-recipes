;;; run-command-recipes.el --- This is collection of recipes to `run-command' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (dash "2.18.0") (f "0.20.0") (run-command "0.1.0") (s "1.12.0") (ht "2.4"))
;; Keywords: extensions, run-command
;; Homepage: https://github.com/semenInRussia/emacs-run-command-recipes

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

;; If you're going to use all supported language, just put the following
;; code to your Emacs configuration:
;;
;;   (run-command-recipes-use-all)
;;
;; If you're going to use only some special languages, just put the following
;; code to your Emacs configuration:

;;
;;   (run-command-recipes-use latex
;;                            pandoc)
;;

;; Also, Instead of LaTeX and pandoc, you can put anything from the
;; following list:

;; - latex
;; - pandoc
;; - haskell
;; - elisp
;; - rust
;; - python
;; - c
;; - cpp
;; - csharp
;; - java
;; - racket

;;; Code:

(require 'run-command)

(require 'dash)
(require 'f)
(require 's)

(defgroup run-command-recipes nil
  "Group for `run-command-recipes'."
  :group 'tools
  :link '(url-link :tag "GitHub"
          "https://github.com/semenInRussia/emacs-run-command-recipes"))

(defcustom run-command-recipes-supported-recipes
  '(latex pandoc haskell elisp rust python c cpp csharp java racket)
  "List of recipes names supported by `run-command-recipes'."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(add-to-list 'run-command-experiments
             'run-command-experiment-lisp-commands)

(defcustom run-command-recipes-source-path
  "~/projects/emacs-run-command-recipes"
  "Path to the root directory of the `run-command-recipes' source code.

Used when creating a new recipe."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-main-file-path
  (f-join run-command-recipes-source-path "run-command-recipes.el")
  "Path to the `run-command-recipes' project main file."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-readme-file-path
  (f-join run-command-recipes-source-path "README.org")
  "Path to the README.me file of the `run-command-recipes' project."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-doc-template-file-path
  (f-join run-command-recipes-source-path "docs" "template.org")
  "Path to the template for documentation of the each recipe."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-template-file-path
  (f-join run-command-recipes-source-path "run-command-recipes-template.el")
  "Path to the template file for a `run-command' recipe source code file."
  :type 'string
  :group 'run-command-recipes)

(defmacro run-command-recipes-use-all ()
  "Use all recipes for `run-command' supported by `run-command-recipes'."
  `(run-command-recipes-use ,@run-command-recipes-supported-recipes))

(defmacro run-command-recipes-use (&rest recipes)
  "Use RECIPES for `run-command' supported by `run-command-recipes'.

Each of RECIPES is indentifier to recipe without `run-command-recipes-'
prefix."
  `(--each ',recipes (run-command-recipes-use-one it)))

(defun run-command-recipes-use-one (recipe)
  "Use RECIPE for `run-command' supported by `run-command-recipes'.

RECIPE is symbol references to the one of `run-command-recipes'
recipes without `run-command-recipes-' prefix.  See to the comments at
the start of this file for list of supported recipes."
  (require
   (intern (concat "run-command-recipes-" (symbol-name recipe))))
  (add-to-list 'run-command-recipes
               (intern
                (concat "run-command-recipes-" (symbol-name recipe)))))

(defun run-command-recipes-create-recipe (recipe-name)
  "Create a `run-command' recipe named RECIPE-NAME."
  (interactive (list (read-string "Enter new recipe's name: ")))
  (let* ((elisp-file
          (f-join
           run-command-recipes-source-path
           (s-concat "run-command-recipes-" recipe-name ".el")))
         (doc-file
          (f-join run-command-recipes-source-path
                  "docs"
                  (s-concat recipe-name ".org"))))
    (run-command-recipes-add-supported-recipe-to-elisp-comment recipe-name)
    (run-command-recipes-add-supported-recipe-to-elisp-variable recipe-name)
    (run-command-recipes-documentate-supported-recipe-in-readme recipe-name)
    (run-command-recipes-create-recipe-documentation-file recipe-name)
    (run-command-recipes-create-recipe-source-code-file recipe-name)))

(defun run-command-recipes-documentate-supported-recipe-in-readme (recipe-name)
  "Add RECIPE-NAME to the list of supported recipe in the README.md file."
  (let ((doc-file
         (f-join run-command-recipes-source-path
                 "docs"
                 (s-concat recipe-name ".org"))))
    (find-file run-command-recipes-readme-file-path)
    (goto-char (point-max))
    (search-backward-regexp "- =[^=]+= (")
    (end-of-line)
    (newline)
    (run-command-recipes-insert-org-link-to-support recipe-name)))

(defun run-command-recipes-insert-org-link-to-support (recipe-name)
  "Insert a link to the recipe named RECIPE-NAME using Org mode syntax."
  (insert
   (format
    "- =%s= ([[file:docs/%s.org][link on support]])"
    recipe-name
    recipe-name)))

(defun run-command-recipes-add-supported-recipe-to-elisp-comment (recipe-name)
  "Add RECIPE-NAME to the list of recipes in the run-command-recipes.el.

Commentary be in the \";;; Commentary:\" section

List of recipes look like to the following:

;; - elisp
;; - rust"
  (run-command-recipes-goto-last-recipe-list-item-in-elisp-comment)
  (forward-line -1)
  (insert (format ";; - %s\n" recipe-name)))

(defun run-command-recipes-goto-last-recipe-list-item-in-elisp-comment ()
  "In current elisp file visit last item of supported recipes list.

Commentary be in the \";;; Commentary:\" section

List of recipes look like to the following:

;; - elisp
;; - rust

Here this function navigate to rust"
  (find-file run-command-recipes-main-file-path)
  (goto-char (point-min))
  (search-forward-regexp ";; - [^\n]*\n\n"))

(defun run-command-recipes-add-supported-recipe-to-elisp-variable (recipe-name)
  "Find elisp variable for supported recipes, add to it RECIPE-NAME."
  (run-command-recipes-goto-end-of-supported-recipes-elisp-variable)
  (forward-char -1)
  (insert " " recipe-name))

(defun run-command-recipes-goto-end-of-supported-recipes-elisp-variable ()
  "Go to the end of the `run-command-recipes-supported-recipes' var content."
  (find-file run-command-recipes-main-file-path)
  (goto-char (point-min))
  (search-forward-regexp
   "(defcustom run-command-recipes-supported-recipes$")
  (search-forward ")"))

(defun run-command-recipes-create-recipe-documentation-file (recipe-name)
  "Create file for the documentaion of the recipe named RECIPE-NAME."
  (find-file doc-file)
  (run-command-recipes-use-template run-command-recipes-doc-template-file-path
                                    recipe-name))

(defun run-command-recipes-create-recipe-source-code-file (recipe-name)
  "Create file for the source code of the recipe named RECIPE-NAME."
  (let ((elisp-file
         (f-join
          run-command-recipes-source-path
          (s-concat "run-command-recipes-" recipe-name ".el"))))
    (find-file elisp-file)
    (run-command-recipes-use-template run-command-recipes-template-file-path
                                      recipe-name)))

(defun run-command-recipes-use-template (template-path recipe-name)
  "Insert the file on TEMPLATE-PATH content as a template for the recipe.

The recipe is recipe named RECIPE-NAME."
  (->>
   (f-read template-path)
   (s-replace "_recipe-name_" recipe-name)
   (insert)))

(provide 'run-command-recipes)
;;; run-command-recipes.el ends here
