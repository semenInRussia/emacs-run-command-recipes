;;; run-command-recipes-pandoc.el --- Recipe of `run-command' for `pandoc` -*- lexical-binding: t;

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1.0
;; Keywords: extensions run-command
;; Homepage: https://github.com/semenInRussia/emacs-run-command-recipes
;; URL: https://github.com/semenInRussia/emacs-run-command-recipes/blob/main/docs/pandoc.md

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
;; ```
;; (run-command-recipes-use-one 'pandoc)
;; ```

;;; Code:

(require 'dash)
(require 'f)

(require 'run-command-recipes-latex)

(defcustom run-command-recipes-pandoc-output-formats
  '("asciidoc"
    "beamer"
    "bibtex"
    "biblatex"
    "commonmark"
    "context"
    "csljson"
    "docbook"
    "docbook5"
    "docx"
    "dokuwiki"
    "epub"
    "epub2"
    "fb2"
    "gfm"
    "haddock"
    "html"
    "html4"
    "icml"
    "ipynb"
    "jats"
    "jira"
    "json"
    "latex"
    "man"
    "markdown"
    "mediawiki"
    "ms"
    "muse"
    "native"
    "odt"
    "opml"
    "opendocument"
    "org"
    "pdf"
    "plain"
    "pptx"
    "rst"
    "rtf"
    "texinfo"
    "textile"
    "slideous"
    "slidy"
    "dzslides"
    "revealjs"
    "s5"
    "tei"
    "xwiki"
    "zimwiki")
  "This is list of pandoc's formats valid to pandoc's output.
See https://pandoc.org"
  :type '(repeat string)
  :group 'run-command-recipes)

(defcustom run-command-recipes-pandoc-html-modes
  '(html-mode web-mode)
  "List of modes which created for HTML."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defcustom run-command-recipes-pandoc-formats-and-extensions
  '(("asciidoc"  . "adoc")
    ("context"   . "ctx")
    ("docbook"   . "db")
    ("markdown"  . "md")
    ("ms"        . "roff")
    ("latex"     . "tex")
    ("texinfo"   . "texi")
    ("mediawiki" . "wiki")
    ("biblatex"  . "bib"))
  "This is map of pandoc's format code and extension of file.
If your pandoc's code have extensions, which equal to pandoc's code (for
example: org = .(org)), then just don't put pair to this variable."
  :group 'run-command-recipes
  :type '(alist :key-type string :value-type string))

(defun run-command-recipes-pandoc-add-modes-with-format-to-table ;nofmt
    (modes format table)
  "Add MODES as vals, and one FORMAT as keys to TABLE."
  (--each modes (puthash it format table)))

(defcustom run-command-recipes-pandoc-major-modes-input-formats
  (let ((hash (make-hash-table :test 'eq)))
    (puthash 'markdown-mode "markdown" hash)
    (puthash 'gfm-mode "gfm" hash)
    (puthash 'haskell-mode "native" hash)
    (puthash 'rtf-mode "rtf" hash)
    (puthash 'rst-mode "rst" hash)
    (puthash 'txt2tags-mode "t2t" hash)
    (puthash 'textile-mode "textile" hash)
    (puthash 'json-mode "json" hash)
    (puthash 'csv-mode "csv" hash)
    (puthash 'org-mode "org" hash)
    (run-command-recipes-pandoc-add-modes-with-format-to-table
     run-command-recipes-latex-modes "latex" hash)
    (run-command-recipes-pandoc-add-modes-with-format-to-table
     run-command-recipes-pandoc-html-modes "html" hash)
    hash)
  "Hashtable with keys major modes and values pandoc's input format's codes.
See https://pandoc.org for see pandoc's input formats."
  :type 'hashtable
  :group 'run-command-recipes)

(defun run-command-recipes-pandoc ()
  "Pandoc `run-command' recipe, for transform file to other formats."
  (-when-let*
      ((input-file (buffer-file-name))
       (from-format
        (run-command-recipes-pandoc-format-for-major-mode major-mode)))
    (--map
     (run-command-recipes-pandoc-rule-for-pandoc-format
      from-format it input-file)
     run-command-recipes-pandoc-output-formats)))

(defun run-command-recipes-pandoc--change-format-of-file (filename new-format)
  "Change FILENAME of pandoc's format to filename of pandoc's NEW-FORMAT."
  (let ((new-ext
         (alist-get new-format
                    run-command-recipes-pandoc-formats-and-extensions
                    new-format
                    nil
                    #'string-equal)))
    (with-temp-buffer
      (insert filename)
      (end-of-line)
      (while (and (not (bolp))
                  (eq ?. (char-before)))))))

(defun run-command-recipes-pandoc-format-for-major-mode (mode)
  "Return format name of the MODE, if it is one of Pandoc input formats.

See pandoc input formats: https://pandoc.org"
  (gethash mode run-command-recipes-pandoc-major-modes-input-formats))

(defun run-command-recipes-pandoc-rule-for-pandoc-format (from to input-file)
  "Return recipe rule for transform INPUT-FILE FROM format to TO via `pandoc'."
  (let* ((output-file
          (run-command-recipes-pandoc--change-format-of-file input-file to)))
    (list
     :command-name (format "pandoc-%s-to-%s" from to)
     :display (format "Pandoc: convert from %s into %s"
                      (upcase from) (upcase to))
     :command-line (format "pandoc -t %s -f %s -o \"%s\" \"%s\""
                           to from output-file input-file))))

(provide 'run-command-recipes-pandoc)
;;; run-command-recipes-pandoc.el ends here
