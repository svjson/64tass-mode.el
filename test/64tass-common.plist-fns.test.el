;;; 64tass-common.plist-fns.test.el --- summary -*- lexical-binding: t -*-

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

(require '64tass-common)


;; 64tass--pruned-plist

(ert-deftest pruned-plist--varargs--all-values ()
  (should (equal (64tass--pruned-plist
                  :instr-indent (list :indent 20 :source :line :line 548)
                  :comment-indent (list :indent 40 :source :line :line 214))
                 (list :instr-indent (list :indent 20 :source :line :line 548)
                       :comment-indent (list :indent 40 :source :line :line 214)))))

(ert-deftest pruned-plist--varargs--nil-value ()
  (should (equal (64tass--pruned-plist
                  :instr-indent (list :indent 20 :source :line :line 548)
                  :comment-indent nil)
                 (list :instr-indent (list :indent 20 :source :line :line 548)))))

(ert-deftest pruned-plist--varargs--all-nil-values ()
  (should (equal (64tass--pruned-plist
                  :instr-indent nil
                  :comment-indent nil)
                 '())))

(ert-deftest pruned-plist--plist--all-values ()
  (should (equal (64tass--pruned-plist
                  (list :instr-indent (list :indent 20 :source :line :line 548)
                        :comment-indent (list :indent 40 :source :line :line 214)))
                 (list :instr-indent (list :indent 20 :source :line :line 548)
                       :comment-indent (list :indent 40 :source :line :line 214)))))

(ert-deftest pruned-plist--plist--nil-value ()
  (should (equal (64tass--pruned-plist
                  (list :instr-indent (list :indent 20 :source :line :line 548)
                        :comment-indent nil))
                 (list :instr-indent (list :indent 20 :source :line :line 548)))))

(ert-deftest pruned-plist--plist--all-nil-values ()
  (should (equal (64tass--pruned-plist
                  (list :instr-indent nil
                        :comment-indent nil))
                 '())))



;;; 64tass-common.plist-fns.test.el ends here
