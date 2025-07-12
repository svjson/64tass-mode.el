;;; 64tass-mode.column-bounds.test.el --- summary -*- lexical-binding: t -*-

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

;; Tests for `64tass--column-bounds', parsing a single line of 64tass syntax
;; to a model suitable for formatting and navigation.

;;; Code:

(require 'ert)
(require '64tass-test-fixtures)
(require '64tass-mode)
(require '64tass-parse)



;; Comment only

(ert-deftest column-bounds--comment-only--line-start ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
▮;; I ate the last muffin
"
   (should (equal (64tass--column-bounds)
                  (list :line-number 2
                        :line-type :comment
                        :columns (list (list :bounds (cons 0 24)
                                             :type :comment
                                             :content ";; I ate the last muffin")))))))


;; Blank

(ert-deftest column-bounds--comment-only--indented ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
▮     ;; I ate the last muffin
"
   (should (equal (64tass--column-bounds)
                  (list :line-number 2
                        :line-type :blank
                        :columns (list (list :bounds (cons 0 5)
                                             :type :label)
                                       (list :bounds (cons 5 5)
                                             :type :opcode)
                                       (list :bounds (cons 5 29)
                                             :type :comment
                                             :content ";; I ate the last muffin")))))))

(ert-deftest column-bounds--blank--empty-line ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
▮
"
   (should (equal (64tass--column-bounds)
                  (list :line-number 2
                        :line-type :blank
                        :columns (list (list :bounds (cons 0 0)
                                             :type :label)
                                       (list :bounds (cons 0 0)
                                             :type :opcode)
                                       (list :bounds (cons 0 0)
                                             :type :comment)))))))

(ert-deftest column-bounds--blank--line-with-spaces-and-tabs ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
   \t   ▮
"
   (should (equal (64tass--column-bounds)
                  (list :line-number 2
                        :line-type :blank
                        :columns (list (list :bounds (cons 0 7)
                                             :type :label)
                                       (list :bounds (cons 7 7)
                                             :type :opcode)
                                       (list :bounds (cons 7 7)
                                             :type :comment)))))))


;; Label

(ert-deftest column-bounds--label-standalone ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
▮hic_sun_dracones:
"
   (should (equal (64tass--column-bounds)
                  (list :line-number 2
                        :line-type :label
                        :columns (list (list :bounds (cons 0 17)
                                             :type :label-standalone
                                             :content "hic_sun_dracones:")
                                       (list :bounds (cons 17 17)
                                             :type :comment)))))))

(ert-deftest column-bounds--label--with-comment ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
▮hic_sun_dracones:      ;; True story
"
   (should (equal (64tass--column-bounds)
                  (list :line-number 2
                        :line-type :label
                        :columns (list (list :bounds (cons 0 23)
                                             :type :label-standalone
                                             :content "hic_sun_dracones:      ")
                                       (list :bounds (cons 23 36)
                                             :type :comment
                                             :content ";; True story")))))))


;; Assembly address

(ert-deftest column-bounds--assembly-address ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
*=$0801▮
"
   (should (equal (64tass--column-bounds)
                  (list :line-number 2
                        :line-type :assembly-address
                        :columns (list (list :bounds (cons 0 7)
                                             :type :address
                                             :content "*=$0801")
                                       (list :bounds (cons 7 7)
                                             :type :comment)))))))

(ert-deftest column-bounds--assembly-address--with-comment ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
*=$0801▮ ;; Entry point
"
   (should (equal (64tass--column-bounds)
                  (list :line-number 2
                        :line-type :assembly-address
                        :columns (list (list :bounds (cons 0 8)
                                             :type :address
                                             :content "*=$0801 ")
                                       (list :bounds (cons 8 22)
                                             :type :comment
                                             :content ";; Entry point")))))))


;; Constant declaration


(ert-deftest column-bounds--constant--no-spacing ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
myconstant=$04▮
"
   (should (equal (64tass--column-bounds)
                  (list :line-number 2
                        :line-type :constant
                        :columns (list (list :bounds (cons 0 14)
                                             :type :constant
                                             :content "myconstant=$04")
                                       (list :bounds (cons 14 14)
                                             :type :comment)))))))

(ert-deftest column-bounds--constant--spacing ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
myconstant = $04▮
"
   (should (equal (64tass--column-bounds)
                  (list :line-number 2
                        :line-type :constant
                        :columns (list (list :bounds (cons 0 16)
                                             :type :constant
                                             :content "myconstant = $04")
                                       (list :bounds (cons 16 16)
                                             :type :comment)))))))

(ert-deftest column-bounds--constant--no-spacing--with-comment ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
myconstant=$04    ;; Very important▮
"
   (should (equal (64tass--column-bounds)
                  (list :line-number 2
                        :line-type :constant
                        :columns (list (list :bounds (cons 0 18)
                                             :type :constant
                                             :content "myconstant=$04    ")
                                       (list :bounds (cons 18 35)
                                             :type :comment
                                             :content ";; Very important")))))))

(ert-deftest column-bounds--constant--spacing--with-comment ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
myconstant = $04    ;; Very important▮
"
   (should (equal (64tass--column-bounds)
                  (list :line-number 2
                        :line-type :constant
                        :columns (list (list :bounds (cons 0 20)
                                             :type :constant
                                             :content "myconstant = $04    ")
                                       (list :bounds (cons 20 37)
                                             :type :comment
                                             :content ";; Very important")))))))


;; Directive

(ert-deftest column-bounds--directive--single-byte ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
▮.byte $ff
"
   (should (equal (64tass--column-bounds)
                  (list :line-number 2
                        :line-type :directive
                        :columns (list (list :bounds (cons 0 9)
                                             :type :directive
                                             :content ".byte $ff")
                                       (list :bounds (cons 9 9)
                                             :type :comment)))))))


;; Instruction

(ert-deftest column-bounds--label+instruction ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
▮mainloop       lda #$ff
"
   (should (equal (64tass--column-bounds)
                  (list :line-number 2
                        :line-type :instruction
                        :columns (list (list :bounds (cons 0 15)
                                             :type :label
                                             :content "mainloop       ")
                                       (list :bounds (cons 15 23)
                                             :type :opcode
                                             :content "lda #$ff")
                                       (list :bounds (cons 23 23)
                                             :type :comment)))))))

(ert-deftest column-bounds--label+instruction--no-operand ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
▮mainloop       inx
"
   (should (equal (64tass--column-bounds)
                  (list :line-number 2
                        :line-type :instruction
                        :columns (list (list :bounds (cons 0 15)
                                             :type :label
                                             :content "mainloop       ")
                                       (list :bounds (cons 15 18)
                                             :type :opcode
                                             :content "inx")
                                       (list :bounds (cons 18 18)
                                             :type :comment)))))))

(ert-deftest column-bounds--instruction ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
▮               lda #$ff
"
   (should (equal (64tass--column-bounds)
                  (list :line-number 2
                        :line-type :instruction
                        :columns (list (list :bounds (cons 0 15)
                                             :type :label)
                                       (list :bounds (cons 15 23)
                                             :type :opcode
                                             :content "lda #$ff")
                                       (list :bounds (cons 23 23)
                                             :type :comment)))))))

(ert-deftest column-bounds--instruction--identifier-operand ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
▮               bne mainloop
"
   (should (equal (64tass--column-bounds)
                  (list :line-number 2
                        :line-type :instruction
                        :columns (list (list :bounds (cons 0 15)
                                             :type :label)
                                       (list :bounds (cons 15 27)
                                             :type :opcode
                                             :content "bne mainloop")
                                       (list :bounds (cons 27 27)
                                             :type :comment)))))))


(ert-deftest column-bounds--instruction--no-operand ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
▮               inx
"
   (should (equal (64tass--column-bounds)
                  (list :line-number 2
                        :line-type :instruction
                        :columns (list (list :bounds (cons 0 15)
                                             :type :label)
                                       (list :bounds (cons 15 18)
                                             :type :opcode
                                             :content "inx")
                                       (list :bounds (cons 18 18)
                                             :type :comment)))))))

;;; 64tass-parse.column-bounds.test.el ends here

