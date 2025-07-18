;;; 64tass-mode.resolve-point-context.test.el --- summary -*- lexical-binding: t -*-

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

;; Tests for `64tass-resolve-point-context', that formats a line of 64tass code
;; and cycles the cursor position through its columns and content.

;;; Code:

(require '64tass-mode)
(require '64tass-test-fixtures)


;; Line-type - :instruction

(ert-deftest resolve-point-context--instruction-with-comment--no-precedent ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 16)
    (64tass-comment-column-indent . 30))
   "                lda #$ff
"
   (let ((parsed-line (list :type :instruction
                            :opcode (list :value "lda"
                                          :begin 16
                                          :end 19)
                            :operand (list :value "#$ff"
                                           :begin 20
                                           :end 24))))
     (should (equal (64tass--resolve-point-context parsed-line 0)
                    (list :point 0
                          :previous :comment
                          :current :label
                          :next :opcode)))
     (should (equal (64tass--resolve-point-context parsed-line 4)
                    (list :point 4
                          :previous :comment
                          :current :label
                          :next :opcode)))
     (should (equal (64tass--resolve-point-context parsed-line 16)
                    (list :point 16
                          :previous :label
                          :current :opcode
                          :next :comment)))
     (should (equal (64tass--resolve-point-context parsed-line 18)
                    (list :point 18
                          :previous :label
                          :current :opcode
                          :next :comment)))
     (should (equal (64tass--resolve-point-context parsed-line 30)
                    (list :point 30
                          :previous :opcode
                          :current :comment
                          :next :label)))
     (should (equal (64tass--resolve-point-context parsed-line 35)
                    (list :point 35
                          :previous :opcode
                          :current :comment
                          :next :label))))))


;; Line-type - :directive


(ert-deftest resolve-point-context--standalone-directive--no-precedent ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-left-margin-indent . 0)
    (64tass-instruction-column-indent . 16)
    (64tass-comment-column-indent . 30))
   "
.byte $01, $02, $ff, $10
"
   (let ((parsed-line (list :type :directive
                            :directive (list :value ".byte"
                                             :begin 0
                                             :end 5)
                            :args (list :value "$01, $02, $ff, $10"
                                        :begin 6
                                        :end 24))))
     (should (equal (64tass--resolve-point-context parsed-line 0)
                    (list :point 0
                          :previous :comment
                          :current :directive
                          :next :comment)))
     (should (equal (64tass--resolve-point-context parsed-line 4)
                    (list :point 4
                          :previous :comment
                          :current :directive
                          :next :comment)))
     (should (equal (64tass--resolve-point-context parsed-line 16)
                    (list :point 16
                          :previous :comment
                          :current :directive
                          :next :comment)))
     (should (equal (64tass--resolve-point-context parsed-line 18)
                    (list :point 18
                          :previous :comment
                          :current :directive
                          :next :comment)))
     (should (equal (64tass--resolve-point-context parsed-line 24)
                    (list :point 24
                          :previous :comment
                          :current :directive
                          :next :comment))))))

(ert-deftest resolve-point-context--directive-with-comment--no-precedent ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
.byte $01, $02, $ff, $10               ;; Character properties
"
   (let ((parsed-line (list :type :directive
                            :directive (list :value ".byte"
                                             :begin 0
                                             :end 5)
                            :args (list :value "$01, $02, $ff, $10"
                                        :begin 6
                                        :end 24)
                            :comment (list :value ";; Character properties"
                                           :begin 39
                                           :end 62))))
     (should (equal (64tass--resolve-point-context parsed-line 0)
                    (list :point 0
                          :previous :comment
                          :current :directive
                          :next :comment)))
     (should (equal (64tass--resolve-point-context parsed-line 4)
                    (list :point 4
                          :previous :comment
                          :current :directive
                          :next :comment)))
     (should (equal (64tass--resolve-point-context parsed-line 16)
                    (list :point 16
                          :previous :comment
                          :current :directive
                          :next :comment)))
     (should (equal (64tass--resolve-point-context parsed-line 30)
                    (list :point 30
                          :previous :comment
                          :current :directive
                          :next :comment)))
     (should (equal (64tass--resolve-point-context parsed-line 39)
                    (list :point 39
                          :previous :directive
                          :current :comment
                          :next :directive)))
     (should (equal (64tass--resolve-point-context parsed-line 50)
                    (list :point 50
                          :previous :directive
                          :current :comment
                          :next :directive)))
     (should (equal (64tass--resolve-point-context parsed-line 62)
                    (list :point 62
                          :previous :directive
                          :current :comment
                          :next :directive))))))

(ert-deftest resolve-point-context--directive-with-label-and-comment--no-precedent ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
chr_props .byte $01, $02, $ff, $10     ;; Character properties
"
   (let ((parsed-line (list :type :directive
                            :label (list :value "chr_props"
                                         :begin 0
                                         :end 9)
                            :directive (list :value ".byte"
                                             :begin 10
                                             :end 15)
                            :args (list :value "$01, $02, $ff, $10"
                                        :begin 16
                                        :end 34)
                            :comment (list :value ";; Character properties"
                                           :begin 39
                                           :end 62))))
     (should (equal (64tass--resolve-point-context parsed-line 0)
                    (list :point 0
                          :previous :comment
                          :current :label
                          :next :directive)))
     (should (equal (64tass--resolve-point-context parsed-line 4)
                    (list :point 4
                          :previous :comment
                          :current :label
                          :next :directive)))
     (should (equal (64tass--resolve-point-context parsed-line 10)
                    (list :point 10
                          :previous :label
                          :current :directive
                          :next :comment)))
     (should (equal (64tass--resolve-point-context parsed-line 16)
                    (list :point 16
                          :previous :label
                          :current :directive
                          :next :comment)))
     (should (equal (64tass--resolve-point-context parsed-line 36)
                    (list :point 36
                          :previous :label
                          :current :directive
                          :next :comment)))
     (should (equal (64tass--resolve-point-context parsed-line 39)
                    (list :point 39
                          :previous :directive
                          :current :comment
                          :next :label)))
     (should (equal (64tass--resolve-point-context parsed-line 50)
                    (list :point 50
                          :previous :directive
                          :current :comment
                          :next :label)))
     (should (equal (64tass--resolve-point-context parsed-line 62)
                    (list :point 62
                          :previous :directive
                          :current :comment
                          :next :label))))))



;;; 64tass-mode.resolve-point-context.test.el ends here
