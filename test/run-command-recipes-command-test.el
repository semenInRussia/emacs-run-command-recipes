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

(ert-deftest run-command-recipes-command-test-get-some-options-with-names
    ()
    (let* ((options '(("toc" . "--toc") "-disable-installer"))
           (command
            (run-command-recipes-command :base "pandoc"
                                         :options options)))
        (should
         (equal
          (run-command-recipes-command-get-some-options-with-names
           '("toc" "-disable-installer")
           command)
          '("--toc" "-disable-installer")))))

(ert-deftest run-command-recipes-command-test-get-option-names
    ()
    (let* ((options '(("toc" . "--toc") "-disable-installer"))
           (command
            (run-command-recipes-command :base "pandoc"
                                         :options options)))
        (should
         (equal
          (run-command-recipes-command-get-option-names command)
          '("toc" "-disable-installer")))))

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
            '(("toc" . "--toc")
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
        (should
         (equal
          (run-command-recipes-command-collect
           (run-command-recipes-command-select-one-option command "--toc"))
          "pandoc --toc"))))

(ert-deftest run-command-recipes-command-test-select-one-option
    ()
    (let* ((options
            '(("data-dir" . "--data-dir=[ current-directory ]")))
           (command
            (run-command-recipes-command :base "pandoc"
                                         :options options)))
        (should
         (equal
          (run-command-recipes-command-collect
           (run-command-recipes-command-select-one-option command "data-dir"))
          (concat "pandoc --data-dir=" default-directory)))))

(ert-deftest
    run-command-recipes-command-test--collect-one-option-current-directory
    ()
    (should
     (equal
      (run-command-recipes-command--collect-one-option
       "--data-dir=[ current-directory]")
      (concat "--data-dir=" default-directory))))

(ert-deftest
    run-command-recipes-command-test--collect-one-option-project-root
    ()
    (should
     (equal
      (run-command-recipes-command--collect-one-option
       "--data-dir=[project-root]")
      (concat "--data-dir=" (run-command-recipes-project-root)))))

(provide 'run-command-recipes-command-test)

;;; run-command-recipes-command-test.el ends here
