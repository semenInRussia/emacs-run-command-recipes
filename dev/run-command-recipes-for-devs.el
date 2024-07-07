;;; Only for developers of `run-command-recipes'
;; I wrote some useful functions for me (as I am the single developer
;; of this package).  Idea is to simply add new recipe with only one
;; command
;;
;; to use it use M-x eval-buffer, NOTE that you need in additional
;; dependency: `f'

;;; Code:
(require 'dash)

(defcustom run-command-recipes-source-path
  "~/projects/emacs-run-command-recipes.el"
  "Path to the root directory of the `run-command-recipes' source code.

Used when creating a new recipe."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-main-file-path
  (expand-file-name "run-command-recipes.el" run-command-recipes-source-path)
  "Path to the `run-command-recipes' project main file."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-readme-file-path
  (expand-file-name "README.org" run-command-recipes-source-path)
  "Path to the README.me file of the `run-command-recipes' project."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-doc-template-file-path
  (expand-file-name "docs/template.org" run-command-recipes-source-path)
  "Path to the template for documentation of the each recipe."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-template-file-path
  (expand-file-name "run-command-recipes-template.txt" run-command-recipes-source-path)
  "Path to the template file for a `run-command' recipe source code file."
  :type 'string
  :group 'run-command-recipes)

(defun run-command-recipes-create-recipe (recipe-name)
  "Create a `run-command' recipe named RECIPE-NAME."
  (interactive (list (read-string "Enter new recipe's name: ")))
  (run-command-recipes-add-supported-recipe-to-elisp-comment recipe-name)
  (run-command-recipes-add-supported-recipe-to-elisp-variable recipe-name)
  (run-command-recipes-documentate-supported-recipe-in-readme recipe-name)
  (run-command-recipes-create-recipe-documentation-file recipe-name)
  (run-command-recipes-create-recipe-source-code-file recipe-name))

(defun run-command-recipes-documentate-supported-recipe-in-readme (recipe-name)
  "Add RECIPE-NAME to the list of supported recipe in the README.md file."
  (run-command-recipes-goto-supported-recipes-in-readme)
  (run-command-recipes-insert-org-link-to-support recipe-name))

(defun run-command-recipes-goto-supported-recipes-in-readme ()
  "Go to list of the supported `run-command' recipes in the README.org file.

The README.org file is file of the `run-command-recipes' directory.  See
variable `run-command-recipes-readme-file-path'"
  (find-file run-command-recipes-readme-file-path)
  (goto-char (point-max))
  (search-backward-regexp "- =[^=]+= (")
  (end-of-line)
  (newline))

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
  (find-file (run-command-recipes-recipe-doc-file-path recipe-name))
  (run-command-recipes-use-template run-command-recipes-doc-template-file-path
                                    recipe-name))

(defun run-command-recipes-recipe-doc-file-path (recipe-name)
  "Get path to the documentation file of the `run-command-recipes' RECIPE-NAME."
  (expand-file-name run-command-recipes-source-path
                    (concat "docs/" recipe-name ".org")))

(defun run-command-recipes-create-recipe-source-code-file (recipe-name)
  "Create file for the source code of the recipe named RECIPE-NAME."
  (let ((elisp-file
         (expand-file-name
          (concat "run-command-recipes-" recipe-name ".el")
          run-command-recipes-source-path)))
    (find-file elisp-file)
    (run-command-recipes-use-template run-command-recipes-template-file-path
                                      recipe-name)))

(defun run-command-recipes-use-template (template-path recipe-name)
  "Insert the file on TEMPLATE-PATH content as a template for the recipe.

The recipe is recipe named RECIPE-NAME."
  (->>
   (with-temp-buffer
     (insert-file-contents template-path)
     (buffer-string))
   (replace-regexp-in-string "_recipe-name_" recipe-name)
   (insert)))

(provide 'run-command-recipes-for-devs)
;;; run-command-recipes-for-devs.el ends here
