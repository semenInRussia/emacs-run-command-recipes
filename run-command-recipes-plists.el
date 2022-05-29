;;; run-command-recipes-plists.el --- Operations on `plist` -*- lexical-binding: t; -*-

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

;; This is library of `run-command-recipes' for `plist'

;;; Code:

(require 'dash)

(defun run-command-recipes-plists-plist-map (plist prop transformer)
  "Transform the value of PROP in PLIST with TRANSFORMER.
This function modifies plist with `plist-put'.  So it does the same
side-effects."
  (->>
   (plist-get plist prop)
   (funcall transformer)
   (plist-put plist prop)))

(provide 'run-command-recipes-plists)
;;; run-command-recipes-plists.el ends here
