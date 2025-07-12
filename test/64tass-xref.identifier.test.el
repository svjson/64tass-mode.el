;;; 64tass-xref.identifier.test.el --- summary -*- lexical-binding: t -*-

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

;; Tests for `64tass--identifier-at-point'.

;;; Code:

(require 'ert)
(require '64tass-test-fixtures)
(require '64tass-xref)




(ert-deftest xref-identifier-at-point---jmp-label ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
          jmp main▮loop
"
   (should (equal (64tass-xref--identifier-at-point)
                  "mainloop"))))


(ert-deftest xref-identifier-at-point--sta-x-indexed ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
          sta screen▮_ptr,x
"
   (should (equal (64tass-xref--identifier-at-point)
                  "screen_ptr"))))


(ert-deftest xref-identifier-at-point--sta-x-indexed-with-spacing ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
          sta screen▮_ptr, x
"
   (should (equal (64tass-xref--identifier-at-point)
                  "screen_ptr"))))

(ert-deftest xref-identifier-at-point--sta-x-indexed-with-mod ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
          sta screen▮_ptr+1,x
"
   (should (equal (64tass-xref--identifier-at-point)
                  "screen_ptr"))))


(ert-deftest xref-identifier-at-point--sta-x-indexed-with-mod-and-spacing ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
          sta screen▮_ptr+1, x
          sta colram_ptr + 2, x
"
   (should (equal (64tass-xref--identifier-at-point)
                  "screen_ptr"))

   (forward-line 1)
   (move-to-column 20)
   (should (equal (64tass-xref--identifier-at-point)
                  "colram_ptr"))))




;;; 64tass-xref.identifier.test.el ends here

