;;; 64tass-mode.format-number.test.el --- summary -*- lexical-binding: t -*-

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

;; Tests for `64tass-format-number', formatting an integer value
;; as decimal, hexadecimal or binary.

;;; Code:

(require 'ert)
(require '64tass-common)

(ert-deftest format-number--binary ()
  (should (equal (64tass-format-number 240 :bin) "%11110000"))
  (should (equal (64tass-format-number 15 :bin) "%00001111"))
  (should (equal (64tass-format-number 1 :bin) "%00000001"))
  (should (equal (64tass-format-number 0 :bin) "%00000000"))
  (should (equal (64tass-format-number 128 :bin) "%10000000"))
  (should (equal (64tass-format-number 127 :bin) "%01111111")))

(ert-deftest format-number--hex ()
  (should (equal (64tass-format-number 240 :hex) "$f0"))
  (should (equal (64tass-format-number 15 :hex) "$0f"))
  (should (equal (64tass-format-number 1 :hex) "$01"))
  (should (equal (64tass-format-number 0 :hex) "$00"))
  (should (equal (64tass-format-number 128 :hex) "$80"))
  (should (equal (64tass-format-number 127 :hex) "$7f")))

(ert-deftest format-number--decimal ()
  (should (equal (64tass-format-number 240 :dec) "240"))
  (should (equal (64tass-format-number 15 :dec) "15"))
  (should (equal (64tass-format-number 1 :dec) "1"))
  (should (equal (64tass-format-number 0 :dec) "0"))
  (should (equal (64tass-format-number 128 :dec) "128"))
  (should (equal (64tass-format-number 127 :dec) "127")))

;;; 64tass-common.format-number.test.el ends here
