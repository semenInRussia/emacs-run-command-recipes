;;; run-command-recipes-hashtables.el --- Operations on `hashtable` -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; URL: https://github.com/semenInRussia/emacs-run-command-recipes
;; Keywords: extensions run-command

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
;; This is library of `run-command-recipes' for `hashtable'

;;; Code:

(defun run-command-recipes-hashtables-put (key val table)
  "Put KEY with VAL to TABLE, and get new TABLE."
  (puthash key val table)
  table)

(provide 'run-command-recipes-hashtables)
;;; run-command-recipes-hashtables.el ends here
