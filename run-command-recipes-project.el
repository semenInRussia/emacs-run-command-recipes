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

(declare-function projectile-acquire-root "projectile")
(declare-function project-roots "project")
(declare-function project-current "project")

(defun run-command-recipes-project-root ()
    "Return path on project root."
    (--find it
            (list
             (when (require 'projectile nil t) (projectile-acquire-root))
             (when (and (require 'project nil t) (project-current))
                 (-first-item (project-roots (project-current))))
             default-directory)))


(provide 'run-command-recipes-project)
;;; run-command-recipes-project.el ends here
