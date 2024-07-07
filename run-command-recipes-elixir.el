;;; run-command-recipes-elixir.el --- Recipe of `run-command' for `elixir` -*- lexical-binding: t; -*-

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1.0
;; Keywords: extensions run-command
;; Homepage: https://github.com/semenInRussia/emacs-run-command-recipes
;; URL: https://github.com/semenInRussia/emacs-run-command-recipes/blob/main/docs/elixir.md

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; For use this code put the following to your Emacs configuration:
;;
;; (run-command-recipes-use 'elixir)
;;
;;; Code:
(require 'run-command-recipes-project)


(defcustom run-command-recipes-elixir-major-modes '(elixir-mode)
  "List of major modes in which should work `run-command' recipe for Elixir."
  :type '(repeat symbol)
  :group 'run-command-recipes)

(defun run-command-recipes-elixir-p ()
  "Return non-nil, when in the current buffer recipe for Elixir should work."
  (memq major-mode
        run-command-recipes-elixir-major-modes))

(defun run-command-recipes-elixir ()
  "Recipe of `run-command' for elixir."
  (let ((dir (run-command-recipes-project-root)))
    (when (and
           (executable-find "mix")
           (or (run-command-recipes-elixir-p)
               (file-exists-p (expand-file-name "mix.exs" dir))))
      (list
       (list
        :display "Mix: run tests of project"
        :command-line "mix test"
        :working-dir dir
        :command-name "mix-test")
       (list
        :display "Mix: run, execute project --no-halt"
        :command-line "mix run --no-halt"
        :working-dir dir
        :command-name "mix-run")
       (list
        :display "Mix: run, start registered apps"
        :command-line "mix app.start"
        :working-dir dir
        :command-name "mix-app-start")
       (list
        :display "Mix: clean, delete generated application files"
        :command-line "mix clean"
        :working-dir dir
        :command-name "mix-clean")
       (list
        :display "Mix: compile only"
        :command-line "mix compile"
        :working-dir dir
        :command-name "mix-compile")
       (list
        :display "Mix: install, get all out of date dependencies"
        :command-line "mix deps.get"
        :working-dir dir
        :command-name "mix-deps-get")
       (list
        :display "Mix: update all dependencies"
        :command-line "mix deps.update --all"
        :working-dir dir
        :command-name "mix-deps-update")))))

(provide 'run-command-recipes-elixir)
;;; run-command-recipes-elixir.el ends here
