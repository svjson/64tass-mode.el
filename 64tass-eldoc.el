;;; 64tass-eldoc.el --- summary -*- lexical-binding: t -*-

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

;; Contains functions for parsing and reasoning about 64tass source code.

;;; Code:

(require 'cl-lib)
(require '64tass-common)
(require '64tass-parse)
(require '64tass-mem)


;; eldoc formatting

(defun 64tass-eldoc--format-kv (k v)
  "Return K and V string as \"K: V\" with font-locking."
  (concat
   (propertize k 'face 'font-lock-type-face)
   ": "
   (propertize v 'face 'font-lock-builtin-face)))


(defun 64tass-eldoc--format-num-conversions (number-formats)
  "Formats NUMBER-FORMATS as an eldoc line.

NUMBER-FORMATS is expected to have the structure returned by
`64tass--number-formats':
  (:dec \"32\" :hex \"$32\" :bin \"%00100000)"
  (let (parts)
    (when (member :dec number-formats)
      (setq parts (append parts
                          (list (64tass-eldoc--format-kv
                                 "Decimal"
                                 (plist-get number-formats :dec))))))
    (when (member :hex number-formats)
      (setq parts (append parts
                          (list (64tass-eldoc--format-kv
                                 "Hex"
                                 (plist-get number-formats :hex))))))
    (when (member :bin number-formats)
      (setq parts (append parts
                          (list (64tass-eldoc--format-kv
                                 "Binary"
                                 (plist-get number-formats :bin))))))
    (string-join parts "   ")))



;; eldoc generation


(defun 64tass-eldoc--make-memory-map-doc (addr)
  "Return memory map eldoc entry for address ADDR."
  (when addr
    (let ((entry (64tass--lookup-memory-doc addr)))
      (concat
       (if-let ((range (plist-get entry :range)))
           (64tass-eldoc--format-kv
            "Memory Area" (format "%s - %s" (car range) (cdr range)))
         (64tass-eldoc--format-kv
          "Memory Address"
          (format "%s" (plist-get entry :address))))
       "\n\n"
       (string-join (plist-get entry :text) "\n")))))


(defun 64tass-eldoc-function ()
  "Provide eldoc documentation for 64tass / 6502 symbols at point."
  (let* ((symbol (64tass--symbol-at-point nil (64tass--parse-line)))
         (num (plist-get symbol :numeric-value))
         (num-form (plist-get symbol :numeric-form))
         (mem-address (plist-get symbol :memory-address))
         (num-conversions (when num (64tass-eldoc--format-num-conversions num-form)))
         (mem-map-entry (64tass-eldoc--make-memory-map-doc mem-address))
         (entries (cl-remove nil (list mem-map-entry num-conversions))))
    (string-join entries "\n\n")))




(provide '64tass-eldoc)

;;; 64tass-eldoc.el ends here
