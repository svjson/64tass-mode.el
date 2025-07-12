;;; 64tass-common.buffer-query.test.el --- summary -*- lexical-binding: t -*-

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

;; Tests for plist utility functions in 64tass-common.el

;;; Code:

(require '64tass-test-fixtures)
(require '64tass-common)
(require '64tass-parse)


;; 64tass--symbol-at-point

(ert-deftest symbol-at-point--hex-literal-operand ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
              lda #$7▮e
"
   (should (equal (64tass--symbol-at-point)
                  (list :symbol "#$7e"
                        :numeric-value 126
                        :numeric-form (list :dec "126" :hex "$7e" :bin "%01111110"))))))

(ert-deftest symbol-at-point--16bit-mem-address-operand ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
              lda $d▮002
"
   (should (equal (64tass--symbol-at-point)
                  (list :symbol "$d002"
                        :numeric-value 53250
                        :numeric-form (list :dec "53250" :hex "$d002" :bin "%1101000000000010")
                        :memory-address 53250)))))

(ert-deftest symbol-at-point--hex-literal-directive-arg ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
.byte $7▮e
"
   (should (equal (64tass--symbol-at-point nil (64tass--parse-line))
                  (list :symbol "$7e"
                        :numeric-value 126
                        :numeric-form (list :dec "126" :hex "$7e" :bin "%01111110"))))))


;; 64tass--empty-line-p

(ert-deftest empty-line-p--blank-line--no-exceptions ()
  (should (equal (64tass--empty-line-p (list :type :blank))
                 t)))

(ert-deftest empty-line-p--blank-line-with-comment--no-exceptions ()
  (should (equal (64tass--empty-line-p
                  (list :type :blank
                        :comment (list :value ";; I ate the last muffin"
                                       :begin 5
                                       :end 29)))
                 nil)))

(ert-deftest empty-line-p--blank-line-with-comment--comment-exception ()
  (should (equal (64tass--empty-line-p
                  (list :type :blank
                        :comment (list :value ";; I ate the last muffin"
                                       :begin 5
                                       :end 29))
                  :comment)
                 t)))



;;; 64tass-common.buffer-query.test.el ends here
