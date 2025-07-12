;;; run-tests.el --- summary -*- lexical-binding: t -*-

;; Copyright (C) 2025 Sven Johansson

;; This file is not part of GNU Emacs

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

;; Entrypoint for running all 64tass-mode tests from a batch script/Makefile

;;; Code:

(require 'ert)
(require 'package)

;; Make sure test env dir exists
(defvar 64tass--test-env-dir
  (expand-file-name ".emacs.test-env" default-directory))

(unless (file-directory-p 64tass--test-env-dir)
  (make-directory 64tass--test-env-dir t))

;; Override package user dir and custom-file
(setq package-user-dir (expand-file-name "elpa" 64tass--test-env-dir))
(setq custom-file (expand-file-name "custom.el" 64tass--test-env-dir))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(package-install 'dash)

;; Add root and test directores to load-path
(message (expand-file-name "." default-directory))
(message (expand-file-name "./test" default-directory))

(add-to-list 'load-path (expand-file-name "." default-directory))
(add-to-list 'load-path (expand-file-name "./packages/64tass-proc.el" default-directory))
(add-to-list 'load-path (expand-file-name "./packages/flycheck-64tass.el" default-directory))
(add-to-list 'load-path (expand-file-name "./packages/vice-emu-proc.el" default-directory))
(add-to-list 'load-path (expand-file-name "./test" default-directory))

;; Load test files
(load "64tass-common.number-fns.test.el")
(load "64tass-common.plist-fns.test.el")
(load "64tass-common.buffer-query.test.el")
(load "64tass-docblock.content-extraction.test.el")
(load "64tass-docblock.line-identification.test.el")
(load "64tass-mode.align-and-cycle.test.el")
(load "64tass-mode.resolve-point-context.test.el")
(load "64tass-mode.shift-column.test.el")
(load "64tass-mode.line-editing.test.el")
(load "64tass-parse.parse-line.test.el")
(load "64tass-parse.column-bounds.test.el")
(load "64tass-xref.buffer-source.test.el")
(load "64tass-xref.identifier.test.el")

(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here
