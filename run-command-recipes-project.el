;;; run-command-recipes-project.el --- Operations on project -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Keywords: extensions run-command
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
;; This very small wrapper on `f' and `projectile'

;;; Code:
(require 'dash)
(require 'f)

(declare-function projectile-acquire-root "projectile")
(declare-function project-roots "project")
(declare-function project-current "project")

(defvar projectile-mode)

(defun run-command-recipes-project-root ()
  "Return path to project root."
  (cond
    ((and (require 'projectile nil t) projectile-mode)
     (projectile-acquire-root))
    ((and (require 'project nil t) (project-current))
     (-first-item (project-roots (project-current))))
    (t default-directory)))

(defun run-command-recipes-project-root-has (entire)
  "Return t, when root of current project has ENTIRE (filename or directory)."
  (f-exists-p (f-join (run-command-recipes-project-root) entire)))

(defun run-command-recipes-project-root-has-one-of (entires)
  "Return t, when root of current project has of ENTIRES.
Entire is filename or directory."
  (-any 'run-command-recipes-project-root-has entires))

(provide 'run-command-recipes-project)
;;; run-command-recipes-project.el ends here
