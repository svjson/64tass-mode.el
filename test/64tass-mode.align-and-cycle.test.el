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

(ert-deftest align-and-cycle--instruction-with-comment--no-precedent ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 16)
    (64tass-comment-column-indent . 30))
   "*=$0801
.byte $0c, $08, $0a, $00, $9e, $20
.byte $32, $30, $36, $34, $00, $00, $00

|       lda #$06         ; Load ALL the things!
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
|     ldy #$00            ; Moar comments!
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

(ert-deftest align-and-cycle--blank-line--cycles-through-precedent-columns ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 16)
    (64tass-comment-column-indent . 30))
   "*=$0801
.byte $0c, $08, $0a, $00, $9e, $20
.byte $32, $30, $36, $34, $00, $00, $00

       lda #$06         ; Load ALL the things!
|"
   (64tass-align-and-cycle)
   (should (equal (64tass--current-line) "       "))
   (should (equal (current-column) 7))

   (64tass-align-and-cycle)
   (should (equal (64tass--current-line) "                        "))
   (should (equal (current-column) 24))

   (64tass-align-and-cycle)
   (should (equal (64tass--current-line) ""))
   (should (equal (current-column) 0))))


(alist-get :label+instruction 64tass-line-type-segments)

;;; 64tass-mode.align-and-cycle.test.el ends here
