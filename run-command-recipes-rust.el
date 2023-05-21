;;; run-command-recipes-rust.el --- Recipe of `run-command' for `rust` -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.3
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
;; For use this code put the following to your Emacs configuration:
;;
;; (run-command-recipes-use-one 'rust)
;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'run-command-recipes-lib)
(require 'run-command-recipes-project)

(defcustom run-command-recipes-rust-modes
  '(rust-mode rustic-mode)
  "List of modes in which can work recipe `rust' for `run-command'."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defun run-command-recipes-rust-mode-p ()
  "Get t, when in current `major-mode' recipe for `rust' should work."
  (-contains-p run-command-recipes-rust-modes major-mode))

(defcustom run-command-recipes-rust-cargo-filename "Cargo.toml"
  "Name of cargo file."
  :type 'string
  :group 'run-command-recipes)

(defun run-command-recipes-rust-cargo-project-p ()
  "Get non-nil when current opened project is Cargo project."
  (run-command-recipes-project-root-has
   run-command-recipes-rust-cargo-filename))

(defcustom run-command-recipes-rust-rustc-compile-command
  "rustc {file-name}"
  "Shell command which compile current rust file with `rustc'.
Instead of \"%s\" put filename of file."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-rust-rustc-run-command
  (concat
   run-command-recipes-rust-rustc-compile-command
   " && {file-name-no-ext}")
  "Shell command which run currenr rust file with `rustc'.
Instead of first \"%s\" put filename of file, instead of second binary file."
  :type 'function
  :group 'run-command-recipes)

(defun run-command-recipes-rust ()
  "Recipe of `run-command' for `rust'."
  (run-command-recipes-lib-compose-recipes
   #'run-command-recipes-rust-cargo
   #'run-command-recipes-rust-rustc))

(defun run-command-recipes-rust-cargo ()
  "Recipe of `run-command' for `cargo', subrecipe for `rust'."
  (run-command-recipes-lib-build
   (when (and
          (run-command-recipes-rust-cargo-project-p)
          (executable-find "cargo"))
     (list
      (list
       :command-name "cargo-install"
       :display "Install Dependecies from Cargo Project"
       :command-line "cargo install --path .")
      (list
       :command-name "cargo-run"
       :display "Run Cargo Project"
       :command-line "cargo run")
      (list
       :command-name "cargo-build"
       :display "Build Cargo Project"
       :command-line "cargo build")
      (list
       :command-name "cargo-doc"
       :display "Build Documentation for Cargo Project"
       :command-line "cargo doc")
      (list
       :command-name "cargo-test"
       :display "Run Tests for Cargo Project"
       :command-line "cargo test")
      (list
       :command-name "cargo-update"
       :display "Update Dependecies for Cargo Project"
       :command-line "cargo update")
      (list
       :command-name "cargo-publish"
       :display "Publish Cargo Project to https://crates.io"
       :command-line "cargo publish")))))

(defun run-command-recipes-rust-rustc ()
  "Recipe of `run-command' for `rustc' compiler, subrecipe for `rust'."
  (run-command-recipes-lib-build
   (when (and
          (buffer-file-name)
          (run-command-recipes-rust-mode-p)
          (executable-find "rustc"))
     (list
      (list
       :command-name "rustc-compile"
       :display "Compile Only Rust File via `rustc'"
       :command-line run-command-recipes-rust-rustc-compile-command)
      (list
       :command-name "rustc-run"
       :display "Run Rust File via `rustc'"
       :command-line run-command-recipes-rust-rustc-run-command)))))

(provide 'run-command-recipes-rust)
;;; run-command-recipes-rust.el ends here
