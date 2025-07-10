;;; 64tass-docblock.content-extraction.test.el --- summary -*- lexical-binding: t -*-

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

;; Tests for docblock content extraction functions:
;; `64tass-docblock--line-content'
;; `64tass-docblock--find-block-start'

;;; Code:

(require '64tass-test-fixtures)
(require '64tass-mode)
(require '64tass-docblock)


;; 64tass-docblock--line-content

(ert-deftest 64tass-docblock--line-content--string-arg--regular-padding ()
  (should (equal (64tass-docblock--line-content "; | hello world |")
                 "hello world")))

(ert-deftest 64tass-docblock--line-content--string-arg--no-padding ()
  (should (equal (64tass-docblock--line-content "; |just tight|")
                 "just tight")))

(ert-deftest 64tass-docblock--line-content--string-arg--no-terminating-pipe ()
  (should (equal (64tass-docblock--line-content "; | only one pipe")
                 "only one pipe")))

(ert-deftest 64tass-docblock--line-content--string-arg--pipe-content--no-termination ()
  (should (equal (64tass-docblock--line-content "; | contains | pipe")
                 "contains | pipe")))

(ert-deftest 64tass-docblock--line-content--string-arg--pipe-content--termination ()
  (should (equal (64tass-docblock--line-content "; | contains | pipe |")
                 "contains | pipe")))

(ert-deftest 64tass-docblock--line-content--string-arg--regular-comment ()
  (should (equal (64tass-docblock--line-content "; just a comment")
                 nil)))

(ert-deftest 64tass-docblock--line-content--contextual ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 16)
    (64tass-comment-column-indent . 30))
   "▮
; | hello world |
; |just tight|
; | only one pipe
; | contains | pipe
; | contains | pipe |
; just a comment
"
   ;; Line 1
   (forward-line 1)
   (should (equal (64tass-docblock--line-content) "hello world"))
   ;; Line 2
   (forward-line 1)
   (should (equal (64tass-docblock--line-content) "just tight"))
   ;; Line 3
   (forward-line 1)
   (should (equal (64tass-docblock--line-content) "only one pipe"))
   ;; Line 4
   (forward-line 1)
   (should (equal (64tass-docblock--line-content) "contains | pipe"))
   ;; Line 5
   (forward-line 1)
   (should (equal (64tass-docblock--line-content) "contains | pipe"))
   ;; Line 6
   (forward-line 1)
   (should (equal (64tass-docblock--line-content) nil))))


;; 64tass-docblock--find-block-start

(ert-deftest 64tass-docblock--find-block-start--from-section ()
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
   (should (= (car (64tass-docblock--find-block-start)) 2))))

(ert-deftest 64tass-docblock--find-block-start--from-first-line-bol ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0))
   "
;▮ +-----------------+
; | SETUP THINGS    |
; | setup_things    |
; +-----------------+
; |                 |
; +-----------------+
sta $d020
"
   (should (= (car (64tass-docblock--find-block-start)) 2))))

(ert-deftest 64tass-docblock--find-block-start--from-before-block ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0))
   "▮
; +-----------------+
; | SETUP THINGS    |
; | setup_things    |
; +-----------------+
; |                 |
; +-----------------+
sta $d020
"
   (should (equal (64tass-docblock--find-block-start) nil))))




;;; 64tass-docblock.content-extraction.test.el ends here
