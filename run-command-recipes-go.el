;;; run-command-recipes-go.el --- Recipe of `run-command' for Golang -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1.0
;; Keywords: extensions run-command
;; Homepage: https://github.com/semenInRussia/emacs-run-command-recipes
;; URL: https://github.com/semenInRussia/emacs-run-command-recipes/blob/main/docs/go.md

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
;; To use this code put the following to your Emacs configuration:
;;
;; (run-command-recipes-use-one 'go)
;;
;;; Code:

(require 'dash)
(require 'f)
(require 'run-command-recipes-lib)

(defcustom run-command-recipes-go-major-modes '(go-mode)
  "List of major modes in which should work `run-command' recipe for Golang."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defun run-command-recipes-go-p ()
  "Return non-nil value when the recipe for Go should work."
  (memq major-mode
        run-command-recipes-go-major-modes))

(defun run-command-recipes-go ()
  "Recipe of `run-command' for go."
  (when (run-command-recipes-go-p)
    (let ((dir (run-command-recipes-project-root)))
      (list
       (list
        :command-name "go-build-project"
        :command-line "go build"
        :working-dir dir
        :display "Go: compile packages and dependencies")
       (and
        (buffer-file-name)
        (list
         :command-name "go-build"
         :display "Go: build file"
         :working-dir dir
         :command-line (concat "go build " (buffer-file-name))))
       (and
        (buffer-file-name)
        (list
         :command-name "go-run"
         :display "Go: compile, execute file"
         :working-dir dir
         :command-line (compile "go run " (buffer-file-name))))
       (list
        :command-name "go-run-project"
        :command-line "go run"
        :working-dir dir
        :display "Go: compile, execute program")
       (list
        :command-name "go-bug"
        :command-line "go bug"
        :working-dir dir
        :display "Go: start a bug report")
       (list
        :command-name "go-clean"
        :command-line "go clean"
        :working-dir dir
        :display "Go: remove object files and cached files")
       (list
        :command-name "go-clean-recursively"
        :command-line "go clean -r"
        :working-dir dir
        :display "Go: remove object files and cached files recursively")
       (list
        :command-name "go-env"
        :command-line "go env"
        :working-dir dir
        :display "Go: print Go environment information")
       (list
        :command-name "go-fix"
        :command-line "go fix"
        :working-dir dir
        :display "Go: update packages to use new APIs")
       (list
        :command-name "go-fmt-project"
        :command-line "go fmt"
        :working-dir dir
        :display "Go: gofmt (reformat) package sources")
       (and
        (buffer-file-name)
        (list
         :command-name "go-fmt"
         :command-line (concat "go fmt " (buffer-file-name))
         :working-dir dir
         :display "Go: gofmt (reformat) file"))
       (list
        :command-name "go-generate"
        :command-line "go generate"
        :working-dir dir
        :display "Go: generate Go files by processing source")
       (list
        :command-name "go-get"
        :command-line "go get"
        :working-dir dir
        :display "Go: add dependencies to current module and install them")
       (list
        :command-name "go-install"
        :command-line "go install"
        :working-dir dir
        :display "Go: compile and install packages and dependencies")
       (list
        :command-name "go-list"
        :command-line "go list"
        :working-dir dir
        :display "Go: list packages or modules")
       (list
        :command-name "go-mod"
        :command-line "go mod"
        :working-dir dir
        :display "Go: module maintenance")
       (list
        :command-name "go-work"
        :command-line "go work"
        :working-dir dir
        :display "Go: workspace maintenance")
       (list
        :command-name "go-test"
        :command-line "go test"
        :working-dir dir
        :display "Go: test package")
       (list
        :command-name "go-tool"
        :command-line "go tool"
        :working-dir dir
        :display "Go: run specified go tool")
       (list
        :command-name "go-version"
        :command-line "go version"
        :working-dir dir
        :display "Go: print Go version")
       (list
        :command-name "go-vet"
        :command-line "go vet"
        :working-dir dir
        :display "Go: report likely mistakes in packages")))))

(provide 'run-command-recipes-go)
;;; run-command-recipes-go.el ends here
