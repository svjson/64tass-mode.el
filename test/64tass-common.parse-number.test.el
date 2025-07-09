;;; 64tass-mode.parse-number.test.el --- summary -*- lexical-binding: t -*-

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

;; Tests for `64tass-parse-number', parsing a string representation of a
;; a numeric value - decimal, hexadecimal or binary - to an integer.

;;; Code:

(require 'ert)
(require '64tass-common)

(ert-deftest parse-number--binary ()
  (should (equal (64tass-parse-number "%11110000") 240))
  (should (equal (64tass-parse-number "%00001111") 15))
  (should (equal (64tass-parse-number "%00000001") 1))
  (should (equal (64tass-parse-number "%00000000") 0))
  (should (equal (64tass-parse-number "%10000000") 128))
  (should (equal (64tass-parse-number "%01111111") 127)))

(ert-deftest parse-number--hex ()
  (should (equal (64tass-parse-number "$f0") 240))
  (should (equal (64tass-parse-number "$0f") 15))
  (should (equal (64tass-parse-number "$01") 1))
  (should (equal (64tass-parse-number "$00") 0))
  (should (equal (64tass-parse-number "$80") 128))
  (should (equal (64tass-parse-number "$7f") 127)))

(ert-deftest parse-number--decimal ()
  (should (equal (64tass-parse-number "240") 240))
  (should (equal (64tass-parse-number "15") 15))
  (should (equal (64tass-parse-number "1") 1))
  (should (equal (64tass-parse-number "0") 0))
  (should (equal (64tass-parse-number "128") 128))
  (should (equal (64tass-parse-number "127") 127)))

;;; 64tass-common.parse-number.test.el ends here
