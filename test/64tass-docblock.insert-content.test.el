
;;; 64tass-docblock.insert-content.test.el --- summary -*- lexical-binding: t -*-

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

;; Tests for docblock content insertion functions:
;; `64tass-docblock--insert-content'

;;; Code:


(require 'ert)
(require '64tass-test-fixtures)
(require '64tass-docblock)
(require '64tass-mode)

(ert-deftest 64tass-docblock--insert-content--in-empty-description-section--after-line ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0))
   "
; +-----------------+
; | SETUP THINGS    |
; | setup_things    |
; +-----------------+
; |▮                |
; +-----------------+
sta $d020
"
   (64tass-docblock--insert-content (line-number-at-pos) "Setup all the things!!!" :after-line)
   (should (equal (buffer-string)
                     "
; +-------------------------+
; | SETUP THINGS            |
; | setup_things            |
; +-------------------------+
; |                         |
; | Setup all the things!!! |
; +-------------------------+
sta $d020
"))))


;;; 64tass-docblock.insert-content.test.el ends here
