;;; run-command-recipes-pandoc.el --- Recipe of `run-command' for `pandoc` -*- lexical-binding: t; 

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
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

;; For use this code put this to config:
;; ```
;; (run-command-recipes-use-one 'pandoc)
;; ```

;;; Code:

(require 'dash)
(require 'f)

(require 'run-command)
(require 'run-command-recipes-hashtables)
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
  (->>
   (make-hash-table :test 'equal)
   (run-command-recipes-hashtables-put "asciidoc" "adoc")
   (run-command-recipes-hashtables-put "context" "ctx")
   (run-command-recipes-hashtables-put "docbook" "db")
   (run-command-recipes-hashtables-put "markdown" "md")
   (run-command-recipes-hashtables-put "ms" "roff")
   (run-command-recipes-hashtables-put "latex" "tex")
   (run-command-recipes-hashtables-put "texinfo" "texi")
   (run-command-recipes-hashtables-put "mediawiki" "wiki")
   (run-command-recipes-hashtables-put "biblatex" "bib"))
  "This is map of pandoc's format code and extension of file.
If your pandoc's code have extensions, which equal to pandoc's code (for
example: org = .(org)), then just don't put pair to this variable."
  :group 'run-command-recipes
  :type 'hashtable)


(defun run-command-recipes-pandoc-add-modes-with-format-to-table (modes format table)
    "Add MODES as vals, and one FORMAT as keys to TABLE."
    (--reduce-from
     (run-command-recipes-hashtables-put it format acc)
     table
     modes))


(defcustom run-command-recipes-pandoc-major-modes-input-formats
  (->>
   (make-hash-table :test 'equal)
   (run-command-recipes-pandoc-add-modes-with-format-to-table
    run-command-recipes-latex-modes "latex")
   (run-command-recipes-pandoc-add-modes-with-format-to-table
    run-command-recipes-pandoc-html-modes "html")
   (run-command-recipes-hashtables-put 'markdown-mode "markdown")
   (run-command-recipes-hashtables-put 'gfm-mode "gfm")
   (run-command-recipes-hashtables-put 'haskell-mode "native")
   (run-command-recipes-hashtables-put 'rtf-mode "rtf")
   (run-command-recipes-hashtables-put 'rst-mode "rst")
   (run-command-recipes-hashtables-put 'txt2tags-mode "t2t")
   (run-command-recipes-hashtables-put 'textile-mode "textile")
   (run-command-recipes-hashtables-put 'json-mode "json")
   (run-command-recipes-hashtables-put 'csv-mode "csv")
   (run-command-recipes-hashtables-put 'org-mode "org"))
  "Hashtable with keys major modes and values pandoc's input format's codes.
See https://pandoc.org for see pandoc's input formats."
  :type 'hashtable
  :group 'run-command-recipes)


(defun run-command-recipes-pandoc-change-format-of-file (filename new-format)
    "Change FILENAME with pandoc's format to filename with pandoc's NEW-FORMAT."
    (let ((new-ext
           (gethash
            new-format
            run-command-recipes-pandoc-formats-and-extensions
            new-format)))
        (f-swap-ext filename new-ext)))


(defmacro run-command-recipes-pandoc-format-for-major-mode (mode)
    "Return format name when MODE is one of Pandoc input formats.
See pandoc input formats: https://pandoc.org"
    `(gethash ,mode run-command-recipes-pandoc-major-modes-input-formats))


(defun run-command-recipes-pandoc ()
    "Pandoc `run-command` recipe, for transform to other formats.
See `run-command-recipes`:
https://github.com/bard/emacs-run-command#examples"
    (-when-let
        (input-file (buffer-file-name))
        (-when-let
            (input-format
             (run-command-recipes-pandoc-format-for-major-mode
              major-mode))
            (--map
             (let* ((output-format it)
                    (output-file
                     (run-command-recipes-pandoc-change-format-of-file
                      input-file
                      output-format)))
                 (list
                  :command-name (format
                                 "pandoc-%s-to-%s"
                                 input-format output-format)
                  :display (format
                            "Convert %s to %s with Pandoc"
                            (upcase input-format)
                            (upcase output-format))
                  :command-line (format
                                 "pandoc -t %s -f %s -o \"%s\" \"%s\""
                                 output-format input-format
                                 output-file input-file)))
             run-command-recipes-pandoc-output-formats))))


(provide 'run-command-recipes-pandoc)
;;; run-command-recipes-pandoc.el ends here
