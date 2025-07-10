;;; 64tass-docblock.line-identification.test.el --- summary -*- lexical-binding: t -*-

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

;; Tests for docblock line identification functions:
;; `64tass-docblock--is-content'
;; `64tass-docblock--is-divider'
;; `64tass-docblock--is-docblock-line'

;;; Code:

(require '64tass-mode)
(require '64tass-test-fixtures)
(require '64tass-docblock)


;; 64tass-dockblock--is-content-line

(ert-deftest 64tass-docblock--is-content-line--string-arg ()
  (should (64tass-docblock--is-content-line "; | something |"))
  (should (64tass-docblock--is-content-line ";|justtight|"))
  (should-not (64tass-docblock--is-content-line "; not a pipe"))
  (should-not (64tass-docblock--is-content-line "   ; totally different")))

(ert-deftest 64tass-docblock--is-content-line--contextual ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 16)
    (64tass-comment-column-indent . 30))
   "▮
; | something |
;|justtight|
; not a pipe
   ; totally different
"
   ;; Put cursor on the matching lines and test
   (forward-line 1)
   (should (64tass-docblock--is-content-line))

   (forward-line 1)
   (should (64tass-docblock--is-content-line))

   (forward-line 1)
   (should-not (64tass-docblock--is-content-line))

   (forward-line 1)
   (should-not (64tass-docblock--is-content-line))))



;; 64tass-docblock--is-divider-line

(ert-deftest 64tass-docblock--is-divider-line--string-arg ()
  (should (64tass-docblock--is-divider-line "; +-----------------+"))
  (should (64tass-docblock--is-divider-line "; -----------------"))
  (should (64tass-docblock--is-divider-line ";+-------+"))
  (should-not (64tass-docblock--is-divider-line "; | not a divider |")))

(ert-deftest 64tass-docblock--is-divider-line--contextual ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 16)
    (64tass-comment-column-indent . 30))
   "▮
; +-----------------+
; -----------------
;+-------+
; | not a divider |
"
   ;; Line 1
   (forward-line 1)
   (should (64tass-docblock--is-divider-line))
   ;; Line 2
   (forward-line 1)
   (should (64tass-docblock--is-divider-line))
   ;; Line 3
   (forward-line 1)
   (should (64tass-docblock--is-divider-line))
   ;; Line 4
   (forward-line 1)
   (should-not (64tass-docblock--is-divider-line))))


;; 64tass-docblock--is-docblock-line

(ert-deftest 64tass-docblock--is-docblock-line--string ()
  (should (64tass-docblock--is-docblock-line "; +-------------------+"))
  (should (64tass-docblock--is-docblock-line "; | SETUP THINGS     |"))
  (should-not (64tass-docblock--is-docblock-line "lda $d020"))
  (should-not (64tass-docblock--is-docblock-line "; not a banner line")))

(ert-deftest 64tass-docblock--is-docblock-line--line-number ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 16)
    (64tass-comment-column-indent . 30))
   "▮
; +-------------------+
; | Title             |
; | Label             |
; +-------------------+
lda #$ff
; just a comment
"
   (should (64tass-docblock--is-docblock-line 2))     ;; divider
   (should (64tass-docblock--is-docblock-line 3))     ;; content
   (should (64tass-docblock--is-docblock-line 4))     ;; content
   (should (64tass-docblock--is-docblock-line 5))     ;; divider
   (should-not (64tass-docblock--is-docblock-line 6)) ;; code
   (should-not (64tass-docblock--is-docblock-line 7)))) ;; regular comment


(ert-deftest 64tass-docblock--is-docblock-line--contextual ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 16)
    (64tass-comment-column-indent . 30))
   "▮
; +-------------------+
; | Title             |
; | Label             |
; +-------------------+
lda #$ff
; just a comment
"
   (forward-line 1)
   (should (64tass-docblock--is-docblock-line)) ;; divider

   (forward-line 1)
   (should (64tass-docblock--is-docblock-line)) ;; content

   (forward-line 1)
   (should (64tass-docblock--is-docblock-line)) ;; content

   (forward-line 1)
   (should (64tass-docblock--is-docblock-line)) ;; divider

   (forward-line 1)
   (should-not (64tass-docblock--is-docblock-line)) ;; code

   (forward-line 1)
   (should-not (64tass-docblock--is-docblock-line)))) ;; regular comment


;;; 64tass-docblock.line-identification.test.el ends here
