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
  "Get non-nil, if in the current `major-mode' recipe for rust should work."
  (-contains-p run-command-recipes-rust-modes major-mode))

(defcustom run-command-recipes-rust-cargo-executable
  "cargo"
  "The path to the cargo executable."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-rust-rustc-executable
  "rustc"
  "The path to the rustc executable."
  :type 'string
  :group 'run-command-recipes)

(defcustom run-command-recipes-rust-cargo-filename "Cargo.toml"
  "Name of cargo file."
  :type 'string
  :group 'run-command-recipes)

(defun run-command-recipes-rust-cargo-project-p ()
  "Get non-nil when current opened project is Cargo project."
  (run-command-recipes-project-root-has
   run-command-recipes-rust-cargo-filename))

(defcustom run-command-recipes-rust-rustc-compile-command
  (concat
   run-command-recipes-rust-rustc-executable
   " {file-name}")
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
   (and
    (run-command-recipes-rust-cargo-project-p)
    (executable-find run-command-recipes-rust-cargo-executable)
    (list
     (list
      :command-name "cargo-install"
      :display "Cargo: install dependecies"
      :command-line "cargo install --path .")
     (list
      :command-name "cargo-run"
      :display "Cargo: run (execute)"
      :command-line "cargo run")
     (list
      :command-name "cargo-run"
      :display "Cargo: run (execute) with nightly rust"
      :command-line "cargo +nightly run")
     (list
      :command-name "cargo-run-release"
      :display "Cargo: run (execute) with release flag"
      :command-line "cargo run --release")
     (list
      :command-name "cargo-build"
      :display "Cargo: build"
      :command-line "cargo build")
     (list
      :command-name "cargo-build-nightly"
      :display "Cargo: build (with nightly rust)"
      :command-line "cargo +nightly build")
     (list
      :command-name "cargo-build-release"
      :display "Cargo: build with release flag"
      :command-line "cargo build --release")
     (list
      :command-name "cargo-doc"
      :display "Cargo: build documentation"
      :command-line "cargo doc")
     (list
      :command-name "cargo-test"
      :display "Cargo: run all tests"
      :command-line "cargo test")
     (list
      :command-name "cargo-update"
      :display "Cargo: update dependencies"
      :command-line "cargo update")
     (list
      :command-name "cargo-fix"
      :display "Cargo: fix fixable warnings"
      :command-line "cargo fix")
     (list
      :command-name "cargo-fix-allow-dirty"
      :display "Cargo: fix fixable warnings (allow all, even dirty)"
      :command-line "cargo fix --allow-dirty")
     (list
      :command-name "cargo-fix-allow-staged"
      :display "Cargo: fix fixable warnings (allow only staged)"
      :command-line "cargo fix --allow-staged")
     (list
      :command-name "cargo-clippy"
      :display "Cargo: lint code with `clippy'"
      :command-line "cargo clippy")
     (list
      :command-name "cargo-publish"
      :display "Cargo: publish the project"
      :command-line "cargo publish")
     (list
      :command-name "cargo-tree"
      :display "Cargo: display dependencies tree"
      :command-line "cargo tree")))))

(defun run-command-recipes-rust-rustc ()
  "Recipe of `run-command' for `rustc' compiler, subrecipe for `rust'."
  (run-command-recipes-lib-build
   (and
    (buffer-file-name)
    (run-command-recipes-rust-mode-p)
    (executable-find run-command-recipes-rust-rustc-executable)
    (list
     (list
      :command-name "rustc-compile"
      :display "rustc: compile file"
      :command-line run-command-recipes-rust-rustc-compile-command)
     (list
      :command-name "rustc-run"
      :display "rustc: compile, execute file"
      :command-line run-command-recipes-rust-rustc-run-command)))))

(provide 'run-command-recipes-rust)
;;; run-command-recipes-rust.el ends here
