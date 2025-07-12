;;; 64tass-mode.shift-column.test.el --- summary -*- lexical-binding: t -*-

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

;; Tests for `64tass-shift-column-up', `64tass-shift-column-down' and
;; by extension `64tass--shift-column'

;;; Code:

(require '64tass-mode)
(require '64tass-test-fixtures)


;; Shift instructions

(ert-deftest shift-column-up--instruction-only-lines ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
                    lda $d011
                    and #$7f
                    sta ▮$d011
"
   (64tass-shift-column-up)
   (should (equal (buffer-string) "
                    lda $d011
                    sta $d011
                    and #$7f
"))
   (should (equal (current-column) 24))
   (should (equal (line-number-at-pos) 3))

   (64tass-shift-column-up)
   (should (equal (buffer-string) "
                    sta $d011
                    lda $d011
                    and #$7f
"))
   (should (equal (current-column) 24))
   (should (equal (line-number-at-pos) 2))))


(ert-deftest shift-column-down--instruction-only-lines ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
                    lda $d011▮
                    and #$7f
                    sta $d011
"
   (64tass-shift-column-down)
   (should (equal (buffer-string) "
                    and #$7f
                    lda $d011
                    sta $d011
"))
   (should (equal (current-column) 29))
   (should (equal (line-number-at-pos) 3))

   (64tass-shift-column-down)
   (should (equal (buffer-string) "
                    and #$7f
                    sta $d011
                    lda $d011
"))
   (should (equal (current-column) 29))
   (should (equal (line-number-at-pos) 4))))



;; Shift labels

(ert-deftest shift-column-down--label-with-instructions ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
write_▮irq_rline     lda $d011
                    and #$7f
                    sta $d011
"
   (64tass-shift-column-down)
   (should (equal (buffer-string) "
                    lda $d011
write_irq_rline     and #$7f
                    sta $d011
"))
   (should (equal (current-column) 6))
   (should (equal (line-number-at-pos) 3))

   (64tass-shift-column-down)
   (should (equal (buffer-string) "
                    lda $d011
                    and #$7f
write_irq_rline     sta $d011
"))
   (should (equal (current-column) 6))
   (should (equal (line-number-at-pos) 4))))


(ert-deftest shift-column-down--standalone-label--onto-instruction-line ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
write_▮irq_rline:
                    lda $d011
                    and #$7f
"
   (64tass-shift-column-down)
   (should (equal (buffer-string) "

write_irq_rline     lda $d011
                    and #$7f
"))
   (should (equal (current-column) 6))
   (should (equal (line-number-at-pos) 3))))


(ert-deftest shift-column-up--inline-label--onto-empty-line ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
write_▮irq_rline:
                    lda $d011
                    and #$7f
"
   (64tass-shift-column-up)
   (should (equal (buffer-string) "write_irq_rline:

                    lda $d011
                    and #$7f
"))
   (should (equal (current-column) 6))
   (should (equal (line-number-at-pos) 1))))



;; Shift comments

(ert-deftest shift-column-down--comment--onto-line-without-comments ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
           lda #$20                    ; ▮Blank out the tetromino in its current location
           sta DRAW_TETR__CHAR         ; on the screen
           lda #$00
           sta DRAW_TETR__COLOR
"
   (64tass-shift-column-down)
   (should (equal (buffer-string) "
           lda #$20                    ; on the screen
           sta DRAW_TETR__CHAR         ; Blank out the tetromino in its current location
           lda #$00
           sta DRAW_TETR__COLOR
"))
   (should (equal (current-column) 41))
   (should (equal (line-number-at-pos) 3))

   (64tass-shift-column-down)
   (should (equal (buffer-string) "
           lda #$20                    ; on the screen
           sta DRAW_TETR__CHAR
           lda #$00                    ; Blank out the tetromino in its current location
           sta DRAW_TETR__COLOR
"))
   (should (equal (current-column) 41))
   (should (equal (line-number-at-pos) 4))))


(ert-deftest shift-column-up-down--comment--onto-long-line ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
           lda #$20
           sta DRAW_TETR__CHAR ; Blank out the tetromino in its current location
           lda #$00            ; ▮Set draw color to black
           sta DRAW_TETR__COLOR
"
   (64tass-shift-column-down)
   (should (equal (buffer-string) "
           lda #$20
           sta DRAW_TETR__CHAR ; Blank out the tetromino in its current location
           lda #$00
           sta DRAW_TETR__COLOR ; Set draw color to black
"))
   (should (equal (current-column) 34))
   (should (equal (line-number-at-pos) 5))

   (64tass-shift-column-up)
   (should (equal (buffer-string) "
           lda #$20
           sta DRAW_TETR__CHAR ; Blank out the tetromino in its current location
           lda #$00            ; Set draw color to black
           sta DRAW_TETR__COLOR
"))
   (should (equal (current-column) 33))
   (should (equal (line-number-at-pos) 4))))



;;; 64tass-mode.shift-column.test.el ends here
