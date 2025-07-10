;;; 64tass-common.el --- summary -*- lexical-binding: t -*-

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

;; Contains common and generic utility functions supporting 64tass-mode.el

;;; Code:

(require 'cl-lib)


;; Variables
(defvar-local 64tass--inhibit-formatting nil)


;; Buffer/editing convenience functions

(defun 64tass--current-line ()
  "Get the full contents of the current line as string."
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun 64tass--goto-line (line-number)
  "Move the cursor to the beginning of LINE-NUMBER."
  (goto-char (point-min))
  (beginning-of-line line-number))

(defun 64tass--get-line (line-number)
  "Get the line contents of line at LINE-NUMBER."
  (save-excursion
    (64tass--goto-line line-number)
    (64tass--current-line)))

(defun 64tass--clear-line ()
  "Clear the current line."
  (delete-region (line-beginning-position)
                 (line-end-position)))

(defun 64tass--delete-indentation ()
  "Delete all indentation (leading whitespace) from the current line."
  (delete-region (line-beginning-position)
                 (save-excursion (back-to-indentation) (point))))

(defun 64tass--replace-region (start end str)
  "Replace region from START to END with STR."
  (delete-region start end)
  (insert str))

(defun 64tass--walk-lines (direction fn)
  "Walk line-by-line in DIRECTION, calling FN on each line.

FN is a function (LINENUM LINE COLLECTED END-OF-INPUT) that returns one of:
  - nil or :stop             → stop and return nil
  - :continue                → skip this line, continue
  - :collect                 → collect (LINENUM . LINE), continue
  - (:collect VALUE)         → collect VALUE instead
  - anything else             → stop and return that value

The function never returns collected lines unless the lambda chooses to."
  (save-excursion
    (let ((collected '())
          (result nil)
          (done nil))
      (while (not done)
        (let* ((linenum (line-number-at-pos))
               (line (64tass--current-line))
               (at-boundary
                (or (and (= direction -1) (= linenum 1))
                    (and (= direction +1) (>= (point) (point-max)))))
               (ret (funcall fn linenum line collected at-boundary)))
          (cond
           ((or (null ret) (eq ret :stop))
            (setq done t result nil))
           ((eq ret :continue)
            nil)
           ((eq ret :collect)
            (push (cons linenum line) collected))
           ((and (plistp ret) (plist-member ret :collect))
            (push (plist-get ret :collect) collected))
           (t
            (setq done t result ret))))
        (unless done
          (if (or (and (= direction -1) (= (line-number-at-pos) 1))
                  (and (= direction +1) (>= (point) (point-max))))
              (setq done t)
            (forward-line direction))))
      result)))


;; plist functions

(defun 64tass--pruned-plist (&rest values)
  "Create a plist from VALUES, excluding any keys whose values are nil.

This function may be called either with a single plist, or plist contents as
varargs.

Examples:
  (64tass--pruned-plist :a 1 :b nil)     => (:a 1)
  (64tass--pruned-plist \\='(:a 1 :b nil)   => (:a 1)"
  (let ((plist (if (and (= (length values) 1) (plistp (car values)))
                   (car values)
                 values)))
    (cl-loop for (key val) on plist by #'cddr
             when val
             append (list key val))))


;; Numerics and number formats

(defun 64tass-to-decimal-string (input)
  "Convert an INPUT hex string to decimal string.
A string representation of hexadecimal number will be converted a string
containing its corresponding integer value."
  (let (;;(hex (string-match "\\$[[:digit:]]+" input))
        (dec (string-match "[[:digit:]]+" input)))
    (if (= dec 0)
        input
      (number-to-string (string-to-number (substring input 1) 16)))))

(defun 64tass-parse-number (str)
  "Parse integer number from dec, hex or bin STR representation.

e.g,
\"%00000010\" => 2
\"$0f\" => 15
\"12\" => 12"
  (cond
   ((string-match "^\\([0-9]+\\)$" str)
    (string-to-number str 10))

   ((string-match "^\\$\\([0-9a-fA-F]+\\)" str)
    (string-to-number (match-string 1 str) 16))

   ((string-match "^%\\([01]+\\)" str)
    (string-to-number (match-string 1 str) 2))))

(defun 64tass-format-number (num format)
  "Format number of string-representation of number NUM as FORMAT.

Valid format args: are :dec, :hex and :bin."
  (let ((n (if (eq 'string (type-of num))
               (64tass-parse-number num)
             num)))
    (pcase format
      (:dec (number-to-string n))
      (:bin (64tass-to-binary-string n))
      (:hex (format "$%02x" n)))))

(defun 64tass-to-binary-string (n &optional width)
  "Return a binary string for N, optionally padded to WIDTH bits."
  (let ((width (or width 8))
        (s ""))
    (while (> n 0)
      (setq s (concat (if (zerop (logand n 1)) "0" "1") s))
      (setq n (ash n -1)))              ; arithmetic shift right
    (setq s (or s "0"))
    (concat "%" (if width
                    (setq s (substring (concat (make-string width ?0) s) (- width)))
                  s))))



(provide '64tass-common)

;;; 64tass-common.el ends here
