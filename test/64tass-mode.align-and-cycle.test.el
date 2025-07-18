;;; 64tass-mode.align-and-cycle.test.el --- summary -*- lexical-binding: t -*-

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

;; Tests for `64tass-align-and-cycle', that formats a line of 64tass code
;; and cycles the cursor position through its columns and content.

;;; Code:

(require '64tass-mode)
(require '64tass-test-fixtures)



;; Line-type - :instruction

(ert-deftest align-and-cycle--instruction-with-comment--no-precedent ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 16)
    (64tass-comment-column-indent . 30))
   "*=$0801
.byte $0c, $08, $0a, $00, $9e, $20
.byte $32, $30, $36, $34, $00, $00, $00

▮       lda #$06         ; Load ALL the things!
"
   (let ((formatted-line
          "                lda #$06      ; Load ALL the things!"))
     (64tass-align-and-cycle)
     (should (equal (64tass--current-line) formatted-line))
     (should (equal (current-column) 16))

     (64tass-align-and-cycle)
     (should (equal (64tass--current-line) formatted-line))
     (should (equal (current-column) 30))

     (64tass-align-and-cycle)
     (should (equal (64tass--current-line) formatted-line))
     (should (equal (current-column) 0)))))


(ert-deftest align-and-cycle--instruction-with-comment--formats-to-precedent ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 16)
    (64tass-comment-column-indent . 30))
   "*=$0801
.byte $0c, $08, $0a, $00, $9e, $20
.byte $32, $30, $36, $34, $00, $00, $00

       lda #$06         ; Load ALL the things!
▮     ldy #$00            ; Moar comments!
"
   (let ((formatted-line
          "       ldy #$00         ; Moar comments!"))
     (64tass-align-and-cycle)
     (should (equal (64tass--current-line) formatted-line))
     (should (equal (current-column) 7))

     (64tass-align-and-cycle)
     (should (equal (64tass--current-line) formatted-line))
     (should (equal (current-column) 24))

     (64tass-align-and-cycle)
     (should (equal (64tass--current-line) formatted-line))
     (should (equal (current-column) 0)))))


(ert-deftest align-and-cycle--instruction--wide-instr-column ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 20)
    (64tass-comment-column-indent . 40))
   "▮                    sta lvl_draw_screen_r+1"
   (64tass-align-and-cycle)
   (should (equal (64tass--current-line) "                    sta lvl_draw_screen_r+1"))
   (should (equal (current-column) 20))

   (64tass-align-and-cycle)
   (should (equal (64tass--current-line) "                    sta lvl_draw_screen_r+1 "))
   (should (equal (current-column) 44))))



;; Line-type - :blank

(ert-deftest align-and-cycle--blank-line--cycles-through-precedent-columns ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 16)
    (64tass-comment-column-indent . 30))
   "*=$0801
.byte $0c, $08, $0a, $00, $9e, $20
.byte $32, $30, $36, $34, $00, $00, $00

       lda #$06         ; Load ALL the things!
▮"
   (64tass-align-and-cycle)
   (should (equal (64tass--current-line) "       "))
   (should (equal (current-column) 7))

   (64tass-align-and-cycle)
   (should (equal (64tass--current-line) "                        "))
   (should (equal (current-column) 24))

   (64tass-align-and-cycle)
   (should (equal (64tass--current-line) ""))
   (should (equal (current-column) 0))))

(ert-deftest align-and-cycle--blank-line--backward-cycles-through-precedent-columns ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 16)
    (64tass-comment-column-indent . 30))
   "*=$0801
.byte $0c, $08, $0a, $00, $9e, $20
.byte $32, $30, $36, $34, $00, $00, $00

       lda #$06         ; Load ALL the things!
▮"
   (64tass-align-and-cycle -1)
   (should (equal (64tass--current-line) "                        "))
   (should (equal (current-column) 24))

   (64tass-align-and-cycle -1)
   (should (equal (64tass--current-line) "       "))
   (should (equal (current-column) 7))

   (64tass-align-and-cycle -1)
   (should (equal (64tass--current-line) ""))
   (should (equal (current-column) 0))))



;; Line-type - :label

(ert-deftest align-and-cycle--inline-label-typed--cycle-to-instruction ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 16)
    (64tass-comment-column-indent . 30))
   "*=$0810

mainloop▮"
   (64tass-align-and-cycle)
   (should (equal (64tass--current-line) "mainloop        "))
   (should (equal (current-column) 16))

   (64tass-align-and-cycle)
   (should (equal (64tass--current-line) "mainloop                      "))
   (should (equal (current-column) 30))

   (64tass-align-and-cycle)
   (should (equal (64tass--current-line) "mainloop"))
   (should (equal (current-column) 0))))


;; Line-type - :directive

(ert-deftest align-and-cycle--multi-byte-directive--cycle-segments ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 16)
    (64tass-comment-column-indent . 30))
   "*=$0810

▮.byte $00, $04, $0f, $03"
   (64tass-align-and-cycle)
   (should (equal (64tass--current-line) ".byte $00, $04, $0f, $03      "))
   (should (equal (current-column) 30))

   (64tass-align-and-cycle)
   (should (equal (64tass--current-line) ".byte $00, $04, $0f, $03"))
   (should (equal (current-column) 0))))


(ert-deftest align-and-cycle--multi-byte-directive-with-label--cycle-segments ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 16)
    (64tass-comment-column-indent . 30))
   "*=$0810

▮chr_props .byte $00, $04, $0f, $03"
   (64tass-align-and-cycle)
   (should (equal (64tass--current-line) "chr_props .byte $00, $04, $0f, $03"))
   (should (equal (current-column) 10))

   (64tass-align-and-cycle)
   (should (equal (64tass--current-line) "chr_props .byte $00, $04, $0f, $03 "))
   (should (equal (current-column) 35))

   (64tass-align-and-cycle)
   (should (equal (64tass--current-line) "chr_props .byte $00, $04, $0f, $03"))
   (should (equal (current-column) 00))))



;; Line-type - :comment

(ert-deftest align-and-cycle--comment-line--toggle-comment-column ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 16)
    (64tass-comment-column-indent . 30))
   "*=$0810

▮; This comment explains ALL!"
   (64tass-align-and-cycle)
   (should (equal (64tass--current-line) "                              ; This comment explains ALL!"))
   (should (equal (current-column) 30))

   (64tass-align-and-cycle)
   (should (equal (64tass--current-line) "; This comment explains ALL!"))
   (should (equal (current-column) 0))

   (64tass-align-and-cycle)
   (should (equal (64tass--current-line) "                              ; This comment explains ALL!"))
   (should (equal (current-column) 30))))



;;; 64tass-mode.align-and-cycle.test.el ends here
