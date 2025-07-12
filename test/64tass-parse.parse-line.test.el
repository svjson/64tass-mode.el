;;; 64tass-parse.parse-line.test.el --- summary -*- lexical-binding: t -*-

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

;; Tests for `64tass--parse-line', parsing a single line of 64tass syntax
;; to a model suitable for formatting and navigation.

;;; Code:

(require 'ert)
(require '64tass-mode)


;; Comment only

(ert-deftest parse-line--comment-only--line-start ()
  (should (equal (64tass--parse-line ";; I ate the last muffin")
                 (list :type :comment
                       :comment (list :value ";; I ate the last muffin"
                                      :begin 0
                                      :end 24)))))


;; Blank

(ert-deftest parse-line--blank-with-comment--indented-comment ()
  (should (equal (64tass--parse-line "     ;; I ate the last muffin")
                 (list :type :blank
                       :comment (list :value ";; I ate the last muffin"
                                      :begin 5
                                      :end 29)))))


(ert-deftest parse-line--blank--empty-line ()
  (should (equal (64tass--parse-line "")
                 (list :type :blank))))

(ert-deftest parse-line--blank--line-with-spaces-and-tabs ()
  (should (equal (64tass--parse-line "   \t   ")
                 (list :type :blank))))



;; Label

(ert-deftest parse-line--label-standalone ()
  (should (equal (64tass--parse-line "hic_sunt_dracones:")
                 (list :type :label
                       :label-standalone (list :value "hic_sunt_dracones"
                                               :begin 0
                                               :end 17)))))

(ert-deftest parse-line--label--with-comment ()
  (should (equal (64tass--parse-line "hic_sunt_dracones:     ;; True story")
                 (list :type :label
                       :label-standalone (list :value "hic_sunt_dracones"
                                               :begin 0
                                               :end 17)
                       :comment (list :value ";; True story"
                                      :begin 23
                                      :end 36)))))



;; Assembly address

(ert-deftest parse-line--assembly-address ()
  (should (equal (64tass--parse-line "*=$0801")
                 (list :type :assembly-address
                       :address (list :value "$0801"
                                      :begin 2
                                      :end 7)))))

(ert-deftest parse-line--assembly-address--with-comment ()
  (should (equal (64tass--parse-line "*=$0801 ;; Entry point")
                 (list :type :assembly-address
                       :address (list :value "$0801"
                                      :begin 2
                                      :end 7)
                       :comment (list :value ";; Entry point"
                                      :begin 8
                                      :end 22)))))



;; Constant

(ert-deftest parse-line--constant--no-spacing ()
  (should (equal (64tass--parse-line "myconstant=$04")
                 (list :type :constant
                       :constant (list :value "myconstant"
                                       :begin 0
                                       :end 10)
                       :value (list :value "$04"
                                    :begin 11
                                    :end 14)))))

(ert-deftest parse-line--constant--spacing ()
  (should (equal (64tass--parse-line "myconstant = $04")
                 (list :type :constant
                       :constant (list :value "myconstant"
                                       :begin 0
                                       :end 10)
                       :value (list :value "$04"
                                    :begin 13
                                    :end 16)))))

(ert-deftest parse-line--constant--no-spacing--with-comment ()
  (should (equal (64tass--parse-line "myconstant=$04    ;; Very important")
                 (list :type :constant
                       :constant (list :value "myconstant"
                                       :begin 0
                                       :end 10)
                       :value (list :value "$04"
                                    :begin 11
                                    :end 14)
                       :comment (list :value ";; Very important"
                                      :begin 18
                                      :end 35)))))

(ert-deftest parse-line--constant--spacing--with-comment ()
  (should (equal (64tass--parse-line "myconstant = $04    ;; Very important")
                 (list :type :constant
                       :constant (list :value "myconstant"
                                       :begin 0
                                       :end 10)
                       :value (list :value "$04"
                                    :begin 13
                                    :end 16)
                       :comment (list :value ";; Very important"
                                      :begin 20
                                      :end 37)))))



;; Directive

(ert-deftest parse-line--directive--single-byte ()
  (should (equal (64tass--parse-line ".byte $ff")
                 (list :type :directive
                       :directive (list :value ".byte"
                                        :begin 0
                                        :end 5)
                       :args (list :value "$ff"
                                   :begin 6
                                   :end 9)))))


;; Instruction

(ert-deftest parse-line--label+instruction ()
  (should (equal (64tass--parse-line "mainloop       lda #$ff")
                 (list :type :instruction
                       :label (list :value "mainloop"
                                    :begin 0
                                    :end 8)
                       :opcode (list :value "lda"
                                     :begin 15
                                     :end 18)
                       :operand (list :value "#$ff"
                                      :begin 19
                                      :end 23)))))

(ert-deftest parse-line--label+instruction--no-operand ()
  (should (equal (64tass--parse-line "mainloop       inx")
                 (list :type :instruction
                       :label (list :value "mainloop"
                                    :begin 0
                                    :end 8)
                       :opcode (list :value "inx"
                                     :begin 15
                                     :end 18)))))


(ert-deftest parse-line--instruction ()
  (should (equal (64tass--parse-line "               lda #$ff")
                 (list :type :instruction
                       :opcode (list :value "lda"
                                     :begin 15
                                     :end 18)
                       :operand (list :value "#$ff"
                                      :begin 19
                                      :end 23)))))

(ert-deftest parse-line--instruction--identifier-operand ()
  (should (equal (64tass--parse-line "               bne mainloop")
                 (list :type :instruction
                       :opcode (list :value "bne"
                                     :begin 15
                                     :end 18)
                       :operand (list :value "mainloop"
                                      :begin 19
                                      :end 27)))))


(ert-deftest parse-line--instruction--no-operand ()
  (should (equal (64tass--parse-line "               inx")
                 (list :type :instruction
                       :opcode (list :value "inx"
                                     :begin 15
                                     :end 18)))))



;;; 64tass-parse.parse-line.test.el ends here
