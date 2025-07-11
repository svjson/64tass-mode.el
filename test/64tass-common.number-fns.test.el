;;; 64tass-mode.number-fns.test.el --- summary -*- lexical-binding: t -*-

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

;; Tests for common functions dealing with numbers and numeric `64tass-format-number', formatting an integer value
;; as decimal, hexadecimal or binary.

;;; Code:

(require 'ert)
(require '64tass-common)



;; 64tass-parse-number

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



;; 64tass-format-number

(ert-deftest format-number--binary ()
  (should (equal (64tass-format-number 240 :bin) "%11110000"))
  (should (equal (64tass-format-number 15 :bin) "%00001111"))
  (should (equal (64tass-format-number 1 :bin) "%00000001"))
  (should (equal (64tass-format-number 0 :bin) "%00000000"))
  (should (equal (64tass-format-number 128 :bin) "%10000000"))
  (should (equal (64tass-format-number 127 :bin) "%01111111"))
  (should (equal (64tass-format-number 65534 :bin) "%1111111111111110")))

(ert-deftest format-number--hex ()
  (should (equal (64tass-format-number 240 :hex) "$f0"))
  (should (equal (64tass-format-number 15 :hex) "$0f"))
  (should (equal (64tass-format-number 1 :hex) "$01"))
  (should (equal (64tass-format-number 0 :hex) "$00"))
  (should (equal (64tass-format-number 128 :hex) "$80"))
  (should (equal (64tass-format-number 127 :hex) "$7f"))
  (should (equal (64tass-format-number 65534 :hex) "$fffe")))

(ert-deftest format-number--decimal ()
  (should (equal (64tass-format-number 240 :dec) "240"))
  (should (equal (64tass-format-number 15 :dec) "15"))
  (should (equal (64tass-format-number 1 :dec) "1"))
  (should (equal (64tass-format-number 0 :dec) "0"))
  (should (equal (64tass-format-number 128 :dec) "128"))
  (should (equal (64tass-format-number 127 :dec) "127"))
  (should (equal (64tass-format-number 65534 :dec) "65534")))


;; 64tass--number-formats

(ert-deftest number-formats--8bit-number ()
  (should (equal (64tass--number-formats 32)
                 (list :dec "32"
                       :hex "$20"
                       :bin "%00100000"))))

(ert-deftest number-formats--16bit-number ()
  (should (equal (64tass--number-formats 53287)
                 (list :dec "53287"
                       :hex "$d027"
                       :bin "%1101000000100111"))))



;;; 64tass-common.number-fns.test.el ends here
