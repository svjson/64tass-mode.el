;;; 64tass-xref.buffer-source.test.el --- summary -*- lexical-binding: t -*-

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

;; Tests for `64tass-xref--buffer-definitions'.

;;; Code:

(require 'ert)
(require '64tass-test-fixtures)
(require '64tass-xref)



(defconst 64tass-xref--read-joystick-example
  "
*=$2000

read_joystick_port2:
                                    lda $dc00
                                    ldx #$00
                                    ldy #$00

read_joystick_port2_chk_up          lsr
                                    bcs read_joystick_port2_chk_down
                                    dey

read_joystick_port2_chk_down        lsr
                                    bcs read_joystick_port2_chk_left
                                    iny

read_joystick_port2_chk_left        lsr
                                    bcs read_joystick_port2_chk_right
                                    dex

read_joystick_port2_chk_right       lsr
                                    bcs read_joystick_done
                                    inx

read_joystick_done                  rts
")

(ert-deftest xref-buffer-definitions--existing-label ()
  (with-cursor-in-64tass-mode-buffer
   nil
   64tass-xref--read-joystick-example

   (should (equal (64tass-xref--buffer-definitions "read_joystick_port2_chk_right")
                  '(("read_joystick_port2_chk_right" . (:file nil :line 21 :pos 620)))))))

(ert-deftest xref-buffer-definitions--read-all ()
  (with-cursor-in-64tass-mode-buffer
   nil
   64tass-xref--read-joystick-example

   (should (equal (64tass-xref--buffer-definitions nil)
                  '(("read_joystick_done" . (:file nil :line 25 :pos 760))
                    ("read_joystick_port2_chk_right" . (:file nil :line 21 :pos 620))
                    ("read_joystick_port2_chk_left" . (:file nil :line 17 :pos 469))
                    ("read_joystick_port2_chk_down" . (:file nil :line 13 :pos 319))
                    ("read_joystick_port2_chk_up" . (:file nil :line 9 :pos 169))
                    ("read_joystick_port2" . (:file nil :line 4 :pos 11)))))))







;;; 64tass-xref.buffer-source.test.el ends here
