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
                          :current :label
                          :next :opcode)))
     (should (equal (64tass--resolve-point-context parsed-line 4)
                    (list :point 4
                          :current :label
                          :next :opcode)))
     (should (equal (64tass--resolve-point-context parsed-line 16)
                    (list :point 16
                          :current :opcode
                          :next :comment)))
     (should (equal (64tass--resolve-point-context parsed-line 18)
                    (list :point 18
                          :current :opcode
                          :next :comment)))
     (should (equal (64tass--resolve-point-context parsed-line 30)
                    (list :point 30
                          :current :comment
                          :next :label)))
     (should (equal (64tass--resolve-point-context parsed-line 35)
                    (list :point 35
                          :current :comment
                          :next :label))))))


;;; 64tass-mode.resolve-point-context.test.el ends here
