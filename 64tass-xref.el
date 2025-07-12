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
(require '64tass-proc)



;; Custom variables

(defcustom 64tass-xref-definition-source-order '(64tass buffer)
  "Search order by which the 64tass-mode xref backend obtains symbol definitions.

The default search order is:
1. ''64tass - Performs a label dump using the 64tass binary and creates
   an index from its output.
2. ''buffer - Performs a scan of the current buffer.

64tass requires the source files to be in a valid state to be able to perform
a label dump, whereas the buffer scan does not.  For any label lookup, 64tass
is the more performant option and should always succeed unless there are
assembly errors, in which the somewhat slower buffer-scan works as a
good-enough fallback."
  :type '(repeat (choice
                  (const 64tass)
                  (const buffer)
                  (symbol :tag "Custom provider")))
  :group '64tass)

(defcustom 64tass-xref-definition-source-alist `((64tass . ,#'64tass-xref--64tass-definitions)
                                                 (buffer . ,#'64tass-xref--buffer-definitions))
  "Function handlers for find-definition sources.

Any symbol appearing in `64tass-xref-definition-source-order' must register a
handler in this a list in order for it to be pollable."
  :type '(alist :key-type (symbol :tag "Source")
                :value-type (function :tag "Handler"))
  :group '64tass)



;; Definition sources

(defun 64tass-xref--64tass-definitions (identifier)
  "Find definition using a 64tass label dump.

Returns all identifiers for a nil value of IDENTIFIER, otherwise the matching
entry, if any."
  (when-let ((entries (64tass-dump-labels)))
    (if identifier
        (when-let ((entry (alist-get identifier entries nil nil #'string-equal)))
          (list entry))
      entries)))


(defun 64tass-xref--buffer-definitions (identifier)
  "Find definitions for IDENTIFIER in the current 64tass source buffer."
  (save-excursion
    (goto-char (point-min))
    (let (results
          (rx (if (null identifier)
                  "[a-zA-Z_][a-zA-Z0-9_]*"
                (regexp-quote identifier))))
      (while (re-search-forward
              (concat "^\\(" rx "\\)\\s-*\\(:\\|\\s-+\\)")
              nil t)
        (push (cons (match-string 1) (list :file (or (buffer-file-name))
                                           :line (line-number-at-pos)
                                           :pos (match-beginning 0)))
              results))
      results)))



;; Utility

(defun 64tass-xref--make-xref (entry identifier)
  "Make xref struct from index ENTRY and IDENTIFIER."
  (let* ((file (when-let ((file-name (plist-get entry :file)))
                 (expand-file-name file-name)))
         (line (plist-get entry :linum))
         (pos (plist-get entry :pos))
         (buffer (if file
                     (find-buffer-visiting file)
                   (current-buffer))))
    (xref-make identifier
               (if buffer
                   (xref-make-buffer-location buffer (or pos
                                                         (with-current-buffer buffer
                                                           (save-excursion
                                                             (64tass--goto-line line)
                                                             (point)))))
                 (xref-make-file-location file
                                          (or line
                                              (with-temp-buffer
                                                (insert-file-contents file)
                                                (goto-char (point-min))
                                                (forward-line (1- line))
                                                (line-number-at-pos)))
                                          0)))))



;; Implementation

(defun 64tass-xref--find-definitions (identifier)
  "Find definition of IDENTIFIER using all available sources."
  (64tass-xref--query-definition-sources
   identifier
   (lambda (results identifier)
     (when-let ((match (alist-get identifier results nil nil #'string-equal)))
       (list (64tass-xref--make-xref match identifier))))))

(defun 64tass-xref--find-apropos (pattern)
  "Find definitions matching PATTERN using all available sources."
  (64tass-xref--query-definition-sources
   nil
   (lambda (results _)
     (cl-loop for (label . entry) in results
              when (string-match-p pattern label)
              collect (64tass-xref--make-xref entry label)))))

(defun 64tass-xref--query-definition-sources (identifier result-transformer)
  "Find definitions for IDENTIFIER in the current 64tass source buffer.

IDENTIFIER may be passed as a string value to find only exact matches, otherwise
each source returns its full index.

RESULT-TRANSFORMER is called with the result from each source, and the provided
function is responsible for transforming any match to xref items."
  (cl-some
   (lambda (source-symbol)
     (let ((handler (alist-get source-symbol 64tass-xref-definition-source-alist)))
       (if handler
           (funcall result-transformer (funcall handler identifier) identifier)
         (progn
           (warn "64tass-xref definition source '%s' is not correctly registered." source-symbol)
           nil))))
   64tass-xref-definition-source-order))


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



;; 64tass xref backend

(defun 64tass-xref-backend ()
  "Return the xref backend for 64tass."
  '64tass)

(cl-defmethod xref-backend-definitions ((_backend (eql 64tass)) identifier)
  "Find definitions for IDENTIFIER in the 64tass source code."
  (64tass-xref--find-definitions identifier))

(cl-defmethod xref-backend-apropos ((_backend (eql 64tass)) pattern)
  "Simplistic exact-match-only apropos backend for PATTERN."
  (64tass-xref--find-apropos pattern))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql 64tass)))
  "Return the identifier at point in 64tass source code."
  (64tass-xref--identifier-at-point))

(provide '64tass-xref)

;;; 64tass-xref.el ends here
