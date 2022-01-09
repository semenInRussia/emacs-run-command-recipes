;;; run-command-recipes-pandoc --- Recipe of `run-command' for `pandoc` -*- lexical-binding: t; -*-

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
(require 'dash)
(require 'f)
(require 's)

(require 'run-command)
(require 'run-command-recipes-hashtables)
(require 'run-command-recipes-latex)


(defcustom rcr/pandoc-output-formats '("asciidoc"
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
  :type '(repeat string))


(defcustom rcr/html-modes '(html-mode web-mode)
  "List of modes which created for HTML."
  :type '(repeat symbol))


(defcustom rcr/pandoc-formats-and-extensions
  (->> (make-hash-table :test 'equal)
       (rcr/hashtable-put "asciidoc" "adoc")
       (rcr/hashtable-put "context" "ctx")
       (rcr/hashtable-put "docbook" "db")
       (rcr/hashtable-put "markdown" "md")
       (rcr/hashtable-put "ms" "roff")
       (rcr/hashtable-put "latex" "tex")
       (rcr/hashtable-put "texinfo" "texi")
       (rcr/hashtable-put "mediawiki" "wiki")
       (rcr/hashtable-put "biblatex" "bib")
       )
  "This is map of pandoc's format code and extension of file.
If your pandoc's code have extensions, which equal to pandoc's code (for
example: org = .(org)), then just don't put pair to this variable.")


(defun rcr/add-modes-with-format-to-table (modes
                                           format
                                           table)
    "Add MODES as vals, and one FORMAT as keys to TABLE."
    (--reduce-from (rcr/hashtable-put it format acc)
                   table
                   modes)
    )


(defcustom rcr/pandoc-major-modes-input-formats
  (->> (make-hash-table :test 'equal)
       (rcr/add-modes-with-format-to-table rcr/tex-modes "tex")
       (rcr/add-modes-with-format-to-table rcr/html-modes "html")
       (rcr/hashtable-put 'markdown-mode "md")
       (rcr/hashtable-put 'gfm-mode "gfm")
       (rcr/hashtable-put 'haskell-mode "native")
       (rcr/hashtable-put 'rtf-mode "rtf")
       (rcr/hashtable-put 'rst-mode "rst")
       (rcr/hashtable-put 'txt2tags-mode "t2t")
       (rcr/hashtable-put 'textile-mode "textile")
       (rcr/hashtable-put 'json-mode "json")
       (rcr/hashtable-put 'csv-mode "csv")
       (rcr/hashtable-put 'org-mode "org")
       )
  "Hashtable with keys major modes and values pandoc's input format's codes.
See https://pandoc.org for see pandoc's input formats."
  :type 'hashtable)


(defun rcr/pandoc-change-format-of-file (filename new-format)
    "Change FILENAME with pandoc's format to filename with pandoc's NEW-FORMAT."
    (let ((new-ext (or (gethash new-format rcr/pandoc-formats-and-extensions)
                       new-format)))
        (f-swap-ext filename new-ext))
    )


(defun rcr/pandoc-format-for-major-mode (major-mode)
    "Return format name when MAJOR-MODE is one of Pandoc input formats.
See pandoc input formats: https://pandoc.org"
    (-when-let (major-mode-and-format (--find (eq major-mode (-first-item it))
                                              pandoc-input-format-major-modes))
        (-second-item major-mode-and-format))
    )


(defun run-command-recipe-pandoc ()
    "Pandoc `run-command` recipe, for transform to other formats.
See `run-command-recipes`:
https://github.com/bard/emacs-run-command#examples"
    (-when-let (input-file (buffer-file-name))
        (-when-let (input-format (rcr/pandoc-format-for-major-mode major-mode))
            (--map
             (let* ((output-format it)
                    (output-file
                     (rcr/pandoc-change-format-of-file input-file
                                                       output-format)))
                 (list
                  :command-name (format
                                 "pandoc-%s-to-%s"
                                 input-format output-format)
                  :display (format
                            "Convert %s to %s with Pandoc"
                            (s-upcase input-format)
                            (s-upcase output-format))
                  :command-line
                  (format
                   "pandoc -t %s -f %s -o \"%s\" \"%s\""
                   output-format
                   input-format
                   output-file
                   input-file
                   )
                  ))
             rcr/pandoc-output-formats)))
    )


(provide 'run-command-recipes-pandoc)
;;; run-command-recipes-pandoc.el ends here
