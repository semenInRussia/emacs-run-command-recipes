;;; run-command-recipes-command-test.el --- Tests for run-command-recipes.el

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for run-command-recipes-command.el

;;; Code:

(require 'ert)
(require 'run-command-recipes-command)
(require 'run-command-recipes-project)

(ert-deftest run-command-recipes-command-test-collect-most-simple-command
    ()
    (should
     (equal
      (run-command-recipes-command-collect
       (run-command-recipes-command :base "pandoc"))
      "pandoc")))

(ert-deftest run-command-recipes-command-test-existent-option-p
    ()
    (let ((command
           (run-command-recipes-command
            :base "pandoc"
            :options '("--toc" "-disable-installer"))))
        (should
         (run-command-recipes-command-existent-option-p command "--toc"))
        (should-not
         (run-command-recipes-command-existent-option-p command "lldl"))))

(ert-deftest run-command-recipes-command-test-collect-with-special-base
    ()
    (with-temp-buffer
        (rename-buffer "temp")
        (should
         (equal
          (run-command-recipes-command-collect
           (run-command-recipes-command :base "pandoc [buffer-name]"))
          "pandoc temp"))))

(ert-deftest run-command-recipes-command-test-collect-with-suffix
    ()
    (should
     (equal
      (run-command-recipes-command-collect
       (run-command-recipes-command :base "pandoc" :suffix "~"))
      "pandoc ~")))

(ert-deftest run-command-recipes-command-test-collect-with-special-suffix
    ()
    (with-temp-buffer
        (rename-buffer "temp")
        (should
         (equal
          (run-command-recipes-command-collect
           (run-command-recipes-command :base "pandoc"
                                        :suffix "[buffer-name]"))
          "pandoc temp"))))

(ert-deftest run-command-recipes-command-test-select-options
    ()
    (let* ((options '("--toc" "-disable-installer"))
           (command
            (run-command-recipes-command :base "pandoc"
                                         :options options)))
        (should
         (equal
          (run-command-recipes-command-collect
           (run-command-recipes-command-select-options command
                                                       '("--toc")))
          "pandoc --toc"))))

(ert-deftest run-command-recipes-command-test-selected-option-p
    ()
    ()
    (let* ((options '("--toc" "-disable-installer"))
           (command
            (run-command-recipes-command :base "pandoc" :options options)))
        (should-not
         (run-command-recipes-command-selected-option-p command "--toc"))
        (should
         (run-command-recipes-command-selected-option-p
          (run-command-recipes-command-select-one-option command "--toc")
          "--toc"))))

(ert-deftest run-command-recipes-command-test-select-options-with-suffix
    ()
    (let* ((options '("--toc" "-disable-installer"))
           (command
            (run-command-recipes-command :base "pandoc"
                                         :options options
                                         :suffix "~/README.md")))
        (should
         (equal
          (run-command-recipes-command-collect
           (run-command-recipes-command-select-options command
                                                       '("--toc")))
          "pandoc --toc ~/README.md"))))

(ert-deftest run-command-recipes-command-test-select-options-some-times
    ()
    (let* ((options '("--toc" "-disable-installer"))
           (command
            (run-command-recipes-command :base "pandoc"
                                         :options options)))
        (should
         (equal
          (run-command-recipes-command-collect
           (run-command-recipes-command-select-options
            (run-command-recipes-command-select-options command
                                                        '("--toc"))
            '("-disable-installer")))
          "pandoc -disable-installer --toc"))))

(ert-deftest run-command-recipes-command-test-select-non-existent-options
    ()
    (let* ((options '("--toc" "-disable-installer"))
           (command
            (run-command-recipes-command :base "pandoc"
                                         :options options)))
        (should-error
         (run-command-recipes-command-select-options command
                                                     '("--lll"))
         :type 'run-command-recipes-command-non-existent-option)))

(ert-deftest run-command-recipes-command-test-select-options-with-names
    ()
    (let* ((options
            '(("toc"               . "--toc")
              ("disable-installer" . "-disable-installer")))
           (command
            (run-command-recipes-command :base "pandoc"
                                         :options options)))
        (should
         (equal
          (run-command-recipes-command-collect
           (run-command-recipes-command-select-options command
                                                       '("toc")))
          "pandoc --toc"))))

(ert-deftest run-command-recipes-command-test-select-one-option
    ()
    (let* ((options '("--toc" "-disable-installer"))
           (command
            (run-command-recipes-command :base "pandoc"
                                         :options options)))
        (setq command
              (run-command-recipes-command-select-one-option
               command "--toc"))
        (should
         (equal
          (run-command-recipes-command-selected-options command)
          '("--toc")))
        (should
         (equal
          (run-command-recipes-command-collect command)
          "pandoc --toc"))))



(ert-deftest run-command-recipes-command-test-select-one-more-complex-option
    ()
    (let* ((options '(("data-dir" . "--data-dir=[ buffer-name ]")))
           (command
            (run-command-recipes-command :base "pandoc"
                                         :options options)))
        (with-temp-buffer
            (rename-buffer "temp")
            (should
             (equal
              (run-command-recipes-command-collect
               (run-command-recipes-command-select-one-option command
                                                              "data-dir"))
              "pandoc --data-dir=temp")))))

(ert-deftest run-command-recipes-command-test-unselect-one-option
    ()
    (let* ((options
            '(("disable-installer" . "--disable-installer")
              ("toc"               . "--toc")))
           (command
            (run-command-recipes-command-select-one-option
             (run-command-recipes-command :base "pandoc"
                                          :options options)
             "toc")))
        (message "command is %s" command)
        (should
         (run-command-recipes-command-selected-option-p command "toc"))
        (run-command-recipes-command-unselect-one-option command "toc")
        (should-not
         (run-command-recipes-command-selected-option-p command "toc"))))

(ert-deftest run-command-recipes-command-test-toggle-option
    ()
    (let* ((options
            '(("disable-installer" . "--disable-installer")
              ("toc"               . "--toc")))
           (command
            (run-command-recipes-command :base "pandoc"
                                         :options options)))
        (run-command-recipes-command-toggle-option command "toc")
        (should
         (run-command-recipes-command-selected-option-p command
                                                        "toc"))
        (run-command-recipes-command-toggle-option command "toc")
        (should-not
         (run-command-recipes-command-selected-option-p command "toc"))))

(ert-deftest
    run-command-recipes-command-test--expand-shell-code-current-directory
    ()
    (with-temp-buffer
        (rename-buffer "temp")
        (should
         (equal
          (run-command-recipes-command-expand-shell-code
           "--data-dir=[ buffer-name ]")
          "--data-dir=temp"))))

(ert-deftest
    run-command-recipes-command-test--expand-lits-of-shell-codes
    ()
    (with-temp-buffer
        (rename-buffer "temp")
        (should
         (equal
          (run-command-recipes-command-expand-list-of-shell-code
           '("![buffer-name]" "[ buffer-name]"))
          '("!temp" "temp")))))

(ert-deftest
    run-command-recipes-command-test-project-root
    ()
    (should-error
     (run-command-recipes-command-expand-shell-code
      "--data-dir=[dkdkdkkdkdkdkdkdkdd]")
     :type 'run-command-recipes-command-non-existent-var-name-in-shell-code))

(ert-deftest
    run-command-recipes-command-test-expand-shell-code-project-root
    ()
    (should
     (equal
      (run-command-recipes-command-expand-shell-code
       "--data-dir=[project-root]")
      (concat "--data-dir=" (run-command-recipes-project-root)))))

(ert-deftest run-command-recipes-command-test-get-option-with-name
    ()
    (let* ((options '(("toc" . "--toc") "-disable-checker"))
           (command
            (run-command-recipes-command :base "pandoc"
                                         :options options)))
        (should
         (equal
          (run-command-recipes-command-get-option-with-name "toc" command)
          "--toc"))
        (should
         (equal
          (run-command-recipes-command-get-option-with-name "-disable-checker"
                                                            command)
          "-disable-checker"))))

(ert-deftest run-command-recipes-command-test-selected-options-shell-codes
    ()
    (let* ((options '(("toc" . "--toc") "-disable-installer"))
           (command
            (run-command-recipes-command :base "pandoc"
                                         :options options)))
        (run-command-recipes-command-select-one-option command "toc")
        (should
         (equal
          (run-command-recipes-command-selected-options-shell-codes command)
          '("--toc")))))

(ert-deftest run-command-recipes-command-test-get-options-names
    ()
    (let* ((options '(("toc" . "--toc") "-disable-installer"))
           (command
            (run-command-recipes-command :base "pandoc"
                                         :options options)))
        (should
         (equal
          (run-command-recipes-command-get-options-names command)
          '("toc" "-disable-installer")))))


(provide 'run-command-recipes-command-test)

;;; run-command-recipes-command-test.el ends here
