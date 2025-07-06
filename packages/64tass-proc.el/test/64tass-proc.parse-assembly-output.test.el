;;; 64tass-proc.parse-assembly-output.test.el --- summary -*- lexical-binding: t -*-

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

;; Tests for `64tass-proc--parse-assembly-output', that attempts to
;; parse 64tass output into structured data.

;;; Code:

(require '64tass-proc)

(ert-deftest 64tass-proc--parse-assembly-output--no-errors ()
  (let ((output "64tass Turbo Assembler Macro V1.59.3120
64TASS comes with ABSOLUTELY NO WARRANTY; This is free software, and you
are welcome to redistribute it under certain conditions; See LICENSE!

Assembling file:   /tmp/labs/test1/test.asm
Output file:       /tmp/labs/test1/test.prg
Data:         13   $0801-$080d   $000d
Gap:           2   $080e-$080f   $0002
Data:         11   $0810-$081a   $000b
Passes:            2
"))
    (should (equal (64tass-proc--parse-assembly-output output)
                   (list :input-file "/tmp/labs/test1/test.asm"
                         :output-file "/tmp/labs/test1/test.prg"
                         :segments (list (list :type :data
                                               :length 13
                                               :start (list :hex "$0801" :dec 2049)
                                               :end (list :hex "$080d" :dec 2061))
                                         (list :type :gap
                                               :length 2
                                               :start (list :hex "$080e" :dec 2062)
                                               :end (list :hex "$080f" :dec 2063))
                                         (list :type :data
                                               :length 11
                                               :start (list :hex "$0810" :dec 2064)
                                               :end (list :hex "$081a" :dec 2074))
                                         (list :type :passes
                                               :n 2)))))))

(ert-deftest 64tass-proc--parse-assembly-output--one-error ()
  (let ((output " 64tass Turbo Assembler Macro V1.59.3120
64TASS comes with ABSOLUTELY NO WARRANTY; This is free software, and you
are welcome to redistribute it under certain conditions; See LICENSE!

Assembling file:   /tmp/labs/test1/test.asm
/tmp/labs/test1/test.asm:13:23: error: wrong type 'bits'
                 cpxl #$ff
                       ^
Error messages:    1
Passes:            2
Data:         11   $0810-$081a   $000b
Passes:            2
"))
    (should (equal (64tass-proc--parse-assembly-output output)
                   (list :input-file "/tmp/labs/test1/test.asm"
                         :error-count 1
                         :errors (list (list :input-file "/tmp/labs/test1/test.asm"
                                             :line 13
                                             :column 23
                                             :message "wrong type 'bits'"
                                             :source-ref '("                 cpxl #$ff"
                                                           "                       ^")))
                         :segments (list (list :type :passes
                                               :n 2)
                                         (list :type :data
                                               :length 11
                                               :start (list :hex "$0810" :dec 2064)
                                               :end (list :hex "$081a" :dec 2074))
                                         (list :type :passes
                                               :n 2)))))))


;;; 64tass-proc.parse-assembly-output.test.el ends here
