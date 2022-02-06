;;; run-command-recipes.el --- This is collection of recipes to `run-command' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (dash "2.18.0") (f "0.20.0") (run-command "0.1.0"))
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

;; If you need to use all supported language, just put this to your code:
;;
;;   (run-command-recipes-use-all)
;;
;; If you need to use only special languages, just put this:

;;
;;   (run-command-recipes-use latex
;;                            pandoc)
;;

;; Also Instead of LaTeX and pandoc you can use something from this:

;; - latex
;; - pandoc
;; - haskell
;; - elisp
;; - rust
;; - python

;;; Code:

(require 'run-command)

(require 'dash)
(require 'f)


(defgroup run-command-recipes nil
    "Group for `run-command-recipes'"
    :group 'tools
    :link '(url-link :tag "GitHub"
            "https://github.com/semenInRussia/emacs-run-command-recipes"))


(add-to-list 'run-command-experiments
             'run-command-experiment-lisp-commands)


(defcustom run-command-recipes-supported-recipes '(latex
                                                   pandoc
                                                   haskell
                                                   elisp
                                                   rust)
  "List of recipes' names, which `run-command-recipes' support."
  :type '(repeat symbol)
  :group 'run-command-recipes)


(defcustom run-command-recipes-source-path
  "~/projects/emacs-run-command-recipes"
  "Path to `run-command-recipes''s source.
Used when create new recipe."
  :type 'string
  :group 'run-command-recipes)


(defmacro run-command-recipes-use-all ()
    "Use all recipes for `run-command' from this package."
    `(run-command-recipes-use ,@run-command-recipes-supported-recipes))


(defmacro run-command-recipes-use (&rest recipes)
    "Use RECIPES for `run-command' from this package."
    `(--each ',recipes
         (run-command-recipes-use-one it)))


(defmacro run-command-recipes-useq-one (recipe)
    "Use RECIPE for `run-command' from `run-command-recipes' with quote RECIPE."
    `(run-command-recipes-use-one ',recipe))


(defun run-command-recipes-use-one (recipe)
    "Use RECIPE for `run-command' from `run-command-recipes' without quote."
    (require (intern
              (concat "run-command-recipes-" (symbol-name recipe))))
    (add-to-list 'run-command-recipes
                 (intern (concat "run-command-recipes-"
                                 (symbol-name recipe)))))

(defun run-command-recipes-replace-in-current-buffer (old new)
    "Replace OLD to NEW in whole current buffer."
    (save-excursion
        (beginning-of-buffer)
        (replace-string old new)))


(defun run-command-recipes-replace-recipe-name-in-buffer (recipe-name)
    "Replace _recipe-name_ to RECIPE-NAME in current buffer."
    (run-command-recipes-replace-in-current-buffer "_recipe-name_"
                                                   recipe-name))


(defun run-command-recipes-use-template (template-path recipe-name)
    "Insert template with TEMPLATE-PATH with changed RECIPE-NAME."
    (let* ((template-string (f-read template-path)))
        (insert template-string)
        (run-command-recipes-replace-recipe-name-in-buffer recipe-name)))


(defun run-command-recipes-insert-gfm-link-on-support (recipe-name)
    "Insert link to support of RECIPE-NAME using Github Markdown Syntax."
    (insert (format
             "* `%s`([link on support](docs/%s.md \"SUPER DOC!\"))"
             recipe-name
             recipe-name)))


(defun run-command-recipes-add-supported-recipe-to-readme (readme-file-path
                                                           recipe-name)
    "Visit README-FILE-PATH and add to list of recipes support of RECIPE-NAME.
List of recipes like to this:
* `python`([link on support](docs/python.md \"SUPER DOC!\"))"
    (find-file readme-file-path)
    (end-of-buffer)
    (search-backward-regexp "\\* `.*`(\\[link on support](docs/.*\.md")
    (end-of-line)
    (newline)
    (run-command-recipes-insert-gfm-link-on-support recipe-name))


(defun run-command-recipes-goto-last-recipe-list-item-in-elisp-comment ()
    "In current elisp file visit last item of supported recipes list.
List put on \";;; Commentary:\" section
List of recipes look like to this:

;; - elisp
;; - rust

Here this function navigate to rust"
    (beginning-of-buffer)
    (search-forward-regexp ";; - [^\n]*\n\n"))


(defun run-command-recipes-add-supported-recipe-to-elisp-comment (elisp-filepath
                                                                  recipe-name)
    "In ELISP-FILEPATH add RECIPE-NAME to list of recipes in commentary.
Commentary put on \";;; Commentary:\" section
List of recipes look like to this:

;; - elisp
;; - rust"
    (find-file elisp-filepath)
    (run-command-recipes-goto-last-recipe-list-item-in-elisp-comment)
    (forward-line -1)
    (insert (format ";; - %s\n" recipe-name)))


(defun run-command-recipes-goto-end-of-supported-recipes-elisp-variable ()
    "Go to end of `run-command-recipes-supported-recipes' variable's content."
    (beginning-of-buffer)
    (search-forward-regexp
     "(defcustom run-command-recipes-supported-recipes '(")
    (search-forward ")"))


(defun run-command-recipes-add-supported-recipe-to-elisp-variable (elisp-file
                                                                   recipe-name)
    "Find elisp variable in ELISP-FILE supported recipes, add RECIPE-NAME."
    (find-file elisp-file)
    (run-command-recipes-goto-end-of-supported-recipes-elisp-variable)
    (forward-line -1)
    (end-of-line)
    (newline-and-indent)
    (insert recipe-name))


(defun run-command-recipes-create-recipe (recipe-name)
    "Create `run-command' recipe with RECIPE-NAME."
    (interactive (list (read-string "Enter new recipe's name:")))
    (let* ((elisp-file (f-join
                        run-command-recipes-source-path
                        (s-concat "run-command-recipes-" recipe-name ".el")))
           (elisp-template-file (f-join run-command-recipes-source-path
                                        "run-command-recipes-template.el"))
           (doc-file (f-join run-command-recipes-source-path
                             "docs"
                             (s-concat recipe-name ".md")))
           (doc-template-file (f-join run-command-recipes-source-path
                                      "docs"
                                      "template.md"))
           (pkg-file (f-join run-command-recipes-source-path
                             "run-command-recipes.el"))
           (readme-file (f-join run-command-recipes-source-path "README.md")))
        (run-command-recipes-add-supported-recipe-to-elisp-comment pkg-file
                                                                   recipe-name)
        (run-command-recipes-add-supported-recipe-to-elisp-variable pkg-file
                                                                    recipe-name)
        (run-command-recipes-add-supported-recipe-to-readme readme-file
                                                            recipe-name)
        (find-file doc-file)
        (run-command-recipes-use-template doc-template-file recipe-name)
        (find-file elisp-file)
        (run-command-recipes-use-template elisp-template-file recipe-name)))

(provide 'run-command-recipes)
;;; run-command-recipes.el ends here
