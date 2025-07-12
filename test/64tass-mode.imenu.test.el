;;; 64tass-mode.imenu.test.el --- summary -*- lexical-binding: t -*-

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

;; Tests for `imenu' integration in 64tass-mode.el

;;; Code:

(require 'ert)
(require '64tass-test-fixtures)
(require '64tass-mode)
(require '64tass-xref)

(defconst 64tass-imenu-test-contents "
*=$1000
mainloop:
                    jmp mainloop

game_irq_tick:
                    jsr game_tick

                    asl $d019
                    jmp $ea81

game_tick:
                    jsr read_joystick_port2

                    inx
                    lda plr_move_table, x

read_joystick_port2:
                    rts

*=$2000

plr_move_table:
.byte $fe
.byte $00
.byte $02
")


;; Indexing

(ert-deftest create-imenu-index--force-buffer-scan ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-xref-definition-source-order . '(buffer))
    (64tass-xref-definition-source-alist . `((buffer . ,#'64tass-xref--buffer-definitions))))

   64tass-imenu-test-contents

   (should (equal (funcall imenu-create-index-function)
                  '(("plr_move_table" . 343)
                    ("read_joystick_port2" . 288)
                    ("game_tick" . 165)
                    ("game_irq_tick" . 54)
                    ("mainloop" . 10))))))


;; Lookup


(ert-deftest imenu-lookup--buffer-source ()
  (with-cursor-in-64tass-mode-buffer
   ((64tass-xref-definition-source-order . '(buffer))
    (64tass-xref-definition-source-alist . `((buffer . ,#'64tass-xref--buffer-definitions))))

   64tass-imenu-test-contents

   (imenu "read_joystick_port2")

   (should (equal (point) 288))))







;;; 64tass-mode.imenu.test.el ends here
