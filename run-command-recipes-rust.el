;;; run-command-recipes-rust.el --- Recipe of `run-command' for `rust` -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Keywords: extensions run-command
;; Homepage: https://github.com/semenInRussia/emacs-run-command-recipes
;; URL: https://github.com/semenInRussia/emacs-run-command-recipes/blob/main/docs/rust.md

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
;;
;; (run-command-recipes-use-one 'rust)
;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'run-command-recipes-project)


(defcustom run-command-recipes-rust-modes
  '(rust-mode rustic-mode)
  "List of modes in which can work recipe `rust' for `run-command'."
  :type '(repeat symbol)
  :group 'run-command-recipes)


(defcustom run-command-recipes-rust-mode-p-funtion
  (lambda () (-contains-p run-command-recipes-rust-modes major-mode))
  "Predicate, return non-nil value, if current `major-mode' is Emacs Lisp mode."
  :type 'predicate
  :group 'run-command-recipes)


(defun run-command-recipes-rust-mode-p ()
    "Get t, when current `major-mode' is Emacs Lisp mode."
    (funcall run-command-recipes-rust-mode-p-funtion))


(defcustom run-command-recipes-rust-cargo-project-p-function nil
  "Get non-nil when current opened project is Cargo project.
If nil then just check current project
has `run-command-recipes-rust-cargo-filename' in root of project."
  :type 'predicate
  :group 'run-command-recipes)


(defcustom run-command-recipes-rust-cargo-filename "Cargo.toml"
  "Name of cargo file."
  :type 'string
  :group 'run-command-recipes)


(defun run-command-recipes-rust-cargo-project-p ()
    "Get non-nil when current opened project is Cargo project."
    (if run-command-recipes-rust-cargo-project-p-function
        (funcall run-command-recipes-rust-cargo-project-p-function)
        (run-command-recipes-project-root-has
         run-command-recipes-rust-cargo-filename)))


(defcustom run-command-recipes-rust-rustc-compile-command
  "rustc \"%s\""
  "Shell command which complie currenr rust file with `rustc'.
Instead of \"%s\" put filename of file."
  :type 'string
  :group'run-command-recipes)


(defcustom run-command-recipes-rust-rustc-run-command
  (s-concat run-command-recipes-rust-rustc-compile-command
            " && "
            "\"%s\"")
  "Shell command which run currenr rust file with `rustc'.
Instead of first \"%s\" put filename of file, instead of second binary file."
  :type 'function
  :group 'run-command-recipes)


(defun run-command-recipes-rust ()
    "This is recipe for `run-command' from `run-command-recipes'."
    (list
     (when (run-command-recipes-rust-cargo-project-p)
         (list
          :command-name "cargo-install"
          :display "Install Dependecies from Cargo Project"
          :command-line "cargo install"))
     (when (run-command-recipes-rust-cargo-project-p)
         (list
          :command-name "cargo-run"
          :display "Run Cargo Project"
          :command-line "cargo run"))
     (when (run-command-recipes-rust-cargo-project-p)
         (list
          :command-name "cargo-build"
          :display "Build Cargo Project"
          :command-line "cargo build"))
     (when (run-command-recipes-rust-cargo-project-p)
         (list
          :command-name "cargo-doc"
          :display "Build Documentation for Cargo Project"
          :command-line "cargo doc"))
     (when (run-command-recipes-rust-cargo-project-p)
         (list
          :command-name "cargo-test"
          :display "Run Tests for Cargo Project"
          :command-line "cargo test"))
     (when (run-command-recipes-rust-cargo-project-p)
         (list
          :command-name "cargo-update"
          :display "Update Dependecies for Cargo Project"
          :command-line "cargo update"))
     (when (run-command-recipes-rust-cargo-project-p)
         (list
          :command-name "cargo-publish"
          :display "Publish Cargo Project to https://crates.io"
          :command-line "cargo publish"))
     (when (run-command-recipes-rust-mode-p)
         (list
          :command-name "rustc-compile"
          :display "Compile Rust File with `rustc'"
          :command-line (format run-command-recipes-rust-rustc-compile-command
                                (buffer-file-name))))
     (when (run-command-recipes-rust-mode-p)
         (list
          :command-name "rustc-run"
          :display "Run Rust File with `rustc'"
          :command-line (format run-command-recipes-rust-rustc-run-command
                                (buffer-file-name)
                                (f-no-ext (buffer-file-name)))))))


(provide 'run-command-recipes-rust)
;;; run-command-recipes-rust.el ends here
