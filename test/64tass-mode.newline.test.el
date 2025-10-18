;;; 64tass-mode.newline.test.el --- summary -*- lexical-binding: t -*-

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

;; Tests for newline behavior via `64tass-newline'.

;;; Code:

(require 'ert)
(require '64tass-test-fixtures)
(require '64tass-mode)
(require '64tass-xref)


;; Instruction lines

(ert-deftest newline--end-of-instruction--with-comment ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
game_tick:
                    jsr read_joystick_port2

                    inx                             ;; Move player sprite
                    lda plr_move_table, x▮           ;; Load player increment for dir in x
                    clc
                    adc $d000
                    sta $d000
"
   (message (buffer-string))
   (64tass-newline)
   (should (equal (buffer-string)
                  "
game_tick:
                    jsr read_joystick_port2

                    inx                             ;; Move player sprite
                    lda plr_move_table, x           ;; Load player increment for dir in x
                    
                    clc
                    adc $d000
                    sta $d000
"))
   (should (equal (point)
                  242))))

(ert-deftest newline--start-of-instruction--with-comment ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
game_tick:
                    jsr read_joystick_port2

                    inx                             ;; Move player sprite
                    ▮lda plr_move_table, x           ;; Load player increment for dir in x
                    clc
                    adc $d000
                    sta $d000
"
   (message (buffer-string))
   (64tass-newline)
   (should (equal (buffer-string)
                  "
game_tick:
                    jsr read_joystick_port2

                    inx                             ;; Move player sprite

                    lda plr_move_table, x           ;; Load player increment for dir in x
                    clc
                    adc $d000
                    sta $d000
"))
   (should (equal (point)
                  153))))

(ert-deftest newline--start-of-instruction-line--with-comment ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
game_tick:
                    jsr read_joystick_port2

                    inx                             ;; Move player sprite
▮                    lda plr_move_table, x           ;; Load player increment for dir in x
                    clc
                    adc $d000
                    sta $d000
"
   (message (buffer-string))
   (64tass-newline)
   (should (equal (buffer-string)
                  "
game_tick:
                    jsr read_joystick_port2

                    inx                             ;; Move player sprite

                    lda plr_move_table, x           ;; Load player increment for dir in x
                    clc
                    adc $d000
                    sta $d000
"))
   (should (equal (point)
                  133))))


;; "Blank" lines

(ert-deftest newline--blank-line--empty-bolp ()
  (with-cursor-in-64tass-mode-buffer
   nil
   "
▮

"
   (message (buffer-string))
   (64tass-newline)
   (should (equal (buffer-string)
                  "



"))
   (should (equal (point)
                  3))))





;;; 64tass-mode.newline.test.el ends here
