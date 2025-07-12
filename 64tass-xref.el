;;; 64tass-xref.el --- summary -*- lexical-binding: t -*-

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

;; xref backend for 64tass.

;;; Code:

(require 'cl-lib)
(require 'xref)
(require '64tass-parse)



(defun 64tass-xref-backend ()
  "Return the xref backend for 64tass."
  '64tass)

(defun 64tass-xref--find-definitions (identifier)
  "Find definitions for IDENTIFIER in the 64tass source code."
  (save-excursion
    (goto-char (point-min))
    (let (results)
      (while (re-search-forward
              (concat "^\\(" (regexp-quote identifier) "\\)\\s-*\\(:\\|\\s-+\\)")
              nil t)
        (push (xref-make identifier
                         (xref-make-buffer-location (current-buffer)
                                                    (match-beginning 0)))
              results))
      (nreverse results))))

(defun 64tass-xref--identifier-at-point ()
  "Find and return the identifier at point, if any."
  (let* ((parsed (64tass--parse-line))
         (col (current-column))
         (segment-type (cl-loop for (key value) on parsed by #'cddr
                                when (progn
                                       (and (listp value)
                                            (>= col (plist-get value :begin))
                                            (<= col (plist-get value :end))))
                                return key))
         (segment (plist-get parsed segment-type))
         (value (or (plist-get segment :value)
                    (thing-at-point 'symbol t))))
    (pcase segment-type
      (:operand
       (let* ((stripped (car (split-string value "[ \\+\\,]" t)))
              (number (64tass-parse-number (string-replace "#" "" stripped))))
         (cond
          (number stripped)
          (t (replace-regexp-in-string "[#<>]" "" stripped)))))

      (_ value))))

(cl-defmethod xref-backend-definitions ((_backend (eql 64tass)) identifier)
  "Find definitions for IDENTIFIER in the 64tass source code."
  (64tass-xref--find-definitions identifier))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql 64tass)))
  "Return the identifier at point in 64tass source code."
  (64tass-xref--identifier-at-point))

(provide '64tass-xref)

;;; 64tass-xref.el ends here
