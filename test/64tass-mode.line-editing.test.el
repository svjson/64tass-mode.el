;;; 64tass-mode.line-editing.test.el --- summary -*- lexical-binding: t -*-

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

;; Tests for before/after change hook behavior during editing of a single
;; line of assembly source.

;;; Code:

(require '64tass-mode)
(require '64tass-test-fixtures)



; type :instruction

(ert-deftest line-edit--instruction--add-and-remove-label ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 16)
    (64tass-comment-column-indent . 30))
   "*=$0810

▮                    lda #$e7
"
   (sim-type-key ?l)
   (should (equal (64tass--current-line) "l                   lda #$e7"))
   (should (equal (64tass--parse-line)
                  (list :type :instruction
                        :label '(:value "l" :begin 0 :end 1)
                        :opcode '(:value "lda" :begin 20 :end 23)
                        :operand '(:value "#$e7" :begin 24 :end 28))))

   (sim-type-key ?o)
   (should (equal (64tass--current-line) "lo                  lda #$e7"))

   (sim-type-key ?o)
   (should (equal (64tass--current-line) "loo                 lda #$e7"))

   (sim-type-key ?p)
   (should (equal (64tass--current-line) "loop                lda #$e7"))
   (should (equal (64tass--parse-line)
                  (list :type :instruction
                        :label '(:value "loop" :begin 0 :end 4)
                        :opcode '(:value "lda" :begin 20 :end 23)
                        :operand '(:value "#$e7" :begin 24 :end 28))))
   (sim-type-backspace)
   (should (equal (64tass--current-line) "loo                 lda #$e7"))

   (sim-type-backspace)
   (should (equal (64tass--current-line) "lo                  lda #$e7"))

   (sim-type-backspace)
   (should (equal (64tass--current-line) "l                   lda #$e7"))

   (sim-type-backspace)
   (should (equal (64tass--current-line) "                    lda #$e7"))
   (should (equal (64tass--parse-line)
                  (list :type :instruction
                        :opcode '(:value "lda" :begin 20 :end 23)
                        :operand '(:value "#$e7" :begin 24 :end 28))))))


(ert-deftest line-edit--instruction-with-comment--add-and-remove-label ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 16)
    (64tass-comment-column-indent . 30))
   "*=$0810

▮          lda #$e7     ; Set sprite #1 Y-position
"
   (should (equal (64tass--current-line) "          lda #$e7     ; Set sprite #1 Y-position"))

   (sim-type-key ?u)
   (should (equal (64tass--current-line) "u         lda #$e7     ; Set sprite #1 Y-position"))
   (should (equal (64tass--parse-line)
                  (list :type :instruction
                        :label '(:value "u" :begin 0 :end 1)
                        :opcode '(:value "lda" :begin 10 :end 13)
                        :operand '(:value "#$e7" :begin 14 :end 18)
                        :comment '(:value "; Set sprite #1 Y-position" :begin 23 :end 49))))

   (sim-type-key ?p)
   (should (equal (64tass--current-line) "up        lda #$e7     ; Set sprite #1 Y-position"))

   (sim-type-key ?d)
   (should (equal (64tass--current-line) "upd       lda #$e7     ; Set sprite #1 Y-position"))
   (should (equal (64tass--parse-line)
                  (list :type :instruction
                        :label '(:value "upd" :begin 0 :end 3)
                        :opcode '(:value "lda" :begin 10 :end 13)
                        :operand '(:value "#$e7" :begin 14 :end 18)
                        :comment '(:value "; Set sprite #1 Y-position" :begin 23 :end 49))))
   (sim-type-backspace)
   (should (equal (64tass--current-line) "up        lda #$e7     ; Set sprite #1 Y-position"))

   (sim-type-backspace)
   (should (equal (64tass--current-line) "u         lda #$e7     ; Set sprite #1 Y-position"))

   (sim-type-backspace)
   (should (equal (64tass--current-line) "          lda #$e7     ; Set sprite #1 Y-position"))
   (should (equal (64tass--parse-line)
                  (list :type :instruction
                        :opcode '(:value "lda" :begin 10 :end 13)
                        :operand '(:value "#$e7" :begin 14 :end 18)
                        :comment '(:value "; Set sprite #1 Y-position" :begin 23 :end 49))))))



;;; 64tass-mode.line-editing.test.el ends here
