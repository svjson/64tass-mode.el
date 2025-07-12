;;; 64tass-proc.parse-label-dump.test.el --- summary -*- lexical-binding: t -*-

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

;; Tests for `64tass-proc--parse-label-dump-buffer', that parses label
;; definitions, as well as file and assembly locations from a 64tass label
;; output contents in the current buffer.

;;; Code:

(require '64tass-proc)



(ert-deftest parse-label-dump-output--test-content ()
  (with-temp-buffer
    (insert (string-trim "
main.asm:674:1: current_level = $0bdf
main.asm:99:1: game_tick = $0897
main.asm:583:1: plr_move_table = $0b8e
main.asm:323:1: next_brick_slot = $0a1b
main.asm:701:1: bat_tilt = $4000
main.asm:702:1: bat_tilt_momentum = $4001
main.asm:8:1: initialize = $0810
main.asm:577:1: ball_cell_y = $0b8a
main.asm:178:1: no_wall_coll_x = $0928
main.asm:195:1: no_wall_coll_y = $094d
main.asm:710:1: ball_latch = $400e
main.asm:164:1: ball_wall_coll_x = $0909
main.asm:181:1: ball_wall_coll_y = $092e
main.asm:235:1: handle_brick_coll = $0991
main.asm:704:1: bat_tilt_decay = $4007
main.asm:681:1: level_1_data = $0be4
main.asm:93:1: game_irq_tick = $088e
main.asm:711:1: brick_anim_tick = $400f
main.asm:365:1: read_joystick_port2 = $0a3f
main.asm:22:1: setup_game_mode = $081d
main.asm:576:1: ball_cell_x = $0b89
main.asm:396:1: clear_screen = $0a57"))

    (should (equal (64tass--parse-label-dump-buffer)
                   '(("clear_screen" . (:file "main.asm" :linum "396" :addr "$0a57"))
                     ("ball_cell_x" . (:file "main.asm" :linum "576" :addr "$0b89"))
                     ("setup_game_mode" . (:file "main.asm" :linum "22" :addr "$081d"))
                     ("read_joystick_port2" . (:file "main.asm" :linum "365" :addr "$0a3f"))
                     ("brick_anim_tick" . (:file "main.asm" :linum "711" :addr "$400f"))
                     ("game_irq_tick" . (:file "main.asm" :linum "93" :addr "$088e"))
                     ("level_1_data" . (:file "main.asm" :linum "681" :addr "$0be4"))
                     ("bat_tilt_decay" . (:file "main.asm" :linum "704" :addr "$4007"))
                     ("handle_brick_coll" . (:file "main.asm" :linum "235" :addr "$0991"))
                     ("ball_wall_coll_y" . (:file "main.asm" :linum "181" :addr "$092e"))
                     ("ball_wall_coll_x" . (:file "main.asm" :linum "164" :addr "$0909"))
                     ("ball_latch" . (:file "main.asm" :linum "710" :addr "$400e"))
                     ("no_wall_coll_y" . (:file "main.asm" :linum "195" :addr "$094d"))
                     ("no_wall_coll_x" . (:file "main.asm" :linum "178" :addr "$0928"))
                     ("ball_cell_y" . (:file "main.asm" :linum "577" :addr "$0b8a"))
                     ("initialize" . (:file "main.asm" :linum "8" :addr "$0810"))
                     ("bat_tilt_momentum" . (:file "main.asm" :linum "702" :addr "$4001"))
                     ("bat_tilt" . (:file "main.asm" :linum "701" :addr "$4000"))
                     ("next_brick_slot" . (:file "main.asm" :linum "323" :addr "$0a1b"))
                     ("plr_move_table" . (:file "main.asm" :linum "583" :addr "$0b8e"))
                     ("game_tick" . (:file "main.asm" :linum "99" :addr "$0897"))
                     ("current_level" . (:file "main.asm" :linum "674" :addr "$0bdf")))))))



;;; 64tass-proc.parse-label-dump.test.el ends here
