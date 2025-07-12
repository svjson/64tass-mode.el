;;; 64tass-parse.el --- summary -*- lexical-binding: t -*-

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
(require 'dash)
(require '64tass-common)


;; Line type segment composition

(defconst 64tass-line-type-cycle-segments
  '((:assembly-address  :address :comment)
    (:blank             :label :opcode :comment)
    (:directive         :directive :comment)
    (:instruction       :label :opcode :comment)
    (:label             :label-standalone :comment)
    (:constant          :constant :comment)
    (:comment           :comment)))


(defconst 64tass-segment-composition
  '((:opcode               :opcode " " :operand)
    (:directive            :directive " " :args)
    (:label-standalone     :label-standalone ":")
    (:constant             :constant "=" :value)
    (:address              -2 "*=" :address)))



;; Source line parsing

(defconst 64tass--line-classifiers
  '((:comment
     "^\\s-*\\(;.*\\)$"
     (lambda (str)
       (list :type :comment :comment (64tass--span 1 str))))

    (:blank
     "^\\s-*$"
     (lambda (_) (list :type :blank)))

    (:label
     "^\\s-*\\([a-zA-Z_][a-zA-Z0-9_]*\\):\\s-*$"
     (lambda (str)
       (list :type :label
             :label-standalone (64tass--span 1 str))))

    (:assembly-address
     "^\\s-*\\(\\*\\)=\\s-*\\([$%]?[0-9a-fA-F]+\\)"
     (lambda (str)
       (list :type :assembly-address
             :address (64tass--span 2 str))))

    (:constant
     "^\\s-*\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*=\\s-*\\(.*\\)"
     (lambda (str)
       (list :type :constant
             :constant (64tass--span 1 str)
             :value (64tass--span 2 str))))

    (:directive
     "^\\s-*\\(\\.[a-z]+\\)\\s-+\\([^;]*\\)"
     (lambda (str)
       (list :type :directive
             :directive (64tass--span 1 str)
             :args (64tass--span 2 str))))

    ;; Instruction: label + opcode [+ operand]
    (:instruction
     "^\\s-*\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-+\\([a-zA-Z]\\{1,3\\}\\)\\(?:\\s-+\\(.*\\)\\)?\\s-*$"
     (lambda (str)
       (let ((parsed (list :type :instruction
                           :label   (64tass--span 1 str)
                           :opcode  (64tass--span 2 str))))
         (when (match-beginning 3)
           (setq parsed (plist-put parsed :operand (64tass--span 3 str))))
         parsed)))

    ;; Instruction: opcode [+ operand]
    (:instruction
     "^\\s-*\\([a-zA-Z]\\{1,3\\}\\)\\(?:\\s-+\\(.*\\)\\)?\\s-*$"
     (lambda (str)
       (let ((parsed (list :type :instruction
                           :opcode  (64tass--span 1 str))))
         (when (match-beginning 2)
           (setq parsed (plist-put parsed :operand (64tass--span 2 str))))
         parsed)))

    ;; Instruction: label only (not ending in :, and thus an :instruction line)
    (:instruction
     "^\\s-*\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*$"
     (lambda (str)
       (list :type :instruction
             :label (64tass--span 1 str))))

    ;; fallback
    (:unknown
     ".*"
     (lambda (_)
       (list :type :unknown)))))

(declare-function 64tass--resolve-local-indent-for "64tass-mode.el")

(defun 64tass--column-bounds (&optional parsed-line line)
  "Return the column bounds of the line at LINE.

Both PARSED-LINE and LINE are optional.  If any of them are provided they
must refer to the same line in order for the bounds calculation to be correct.

All relevant columns for the line type are included in the result, in order,
regardless if they are present or not.  If not present, the column entry will
not have a :content key."
  (save-excursion
    (when line
      (64tass--goto-line line))
    (when (null parsed-line)
      (setq parsed-line (64tass--parse-line)))
    (let* ((line-type (plist-get parsed-line :type))
           (column-segments (alist-get line-type 64tass-line-type-cycle-segments))
           (next-present (lambda (from-index)
                           (cl-some (lambda (seg-type)
                                      (plist-get parsed-line seg-type))
                                    (cl-subseq column-segments from-index))))
           (line-end (- (line-end-position) (line-beginning-position)))
           (columns
            (cl-loop for segment-type in column-segments
                     for i from 0
                     for line-seg = (plist-get parsed-line segment-type)
                     for next-type = (nth (1+ i) column-segments)
                     for next-pres = (funcall next-present (1+ i))
                     for next-pres-begin = (plist-get next-pres :begin)
                     for line-comp = (alist-get segment-type 64tass-segment-composition)
                     collect (let* ((local-def (64tass--resolve-local-indent-for segment-type))
                                    (begin (or (if-let ((line-seg-begin (plist-get line-seg :begin))
                                                        (modifier (when (eq 'integer (type-of (car line-comp))) (car line-comp))))
                                                   (+ modifier line-seg-begin)
                                                 line-seg-begin)
                                               (cond ((and local-def next-pres-begin)
                                                      (min local-def next-pres-begin))

                                                     (local-def (min line-end local-def))
                                                     (next-pres-begin next-pres-begin))))
                                    (content (when-let ((seg-content (plist-get line-seg :value)))
                                               (if-let (line-comp)
                                                   (string-trim
                                                    (string-join
                                                     (cl-remove nil (mapcar
                                                                     (lambda (comp-seg)
                                                                       (pcase (type-of comp-seg)
                                                                         ('string comp-seg)
                                                                         ('symbol (or (-> parsed-line (plist-get comp-seg) (plist-get :value)) ""))
                                                                         (_ "")))
                                                                     line-comp))))
                                                 seg-content)))
                                    (content-end (+ begin (length content)))
                                    (end (let ((seg-end (plist-get line-seg :end))
                                               (calc-end (let ((next-local-def (when next-type (64tass--resolve-local-indent-for next-type))))
                                                           (cond ((and next-local-def next-pres-begin)
                                                                  (min next-local-def next-pres-begin))

                                                                 (next-local-def (min line-end next-local-def))
                                                                 (next-pres-begin next-pres-begin)

                                                                 (t begin)))))
                                           (max calc-end content-end))))
                               (64tass--pruned-plist :bounds (cons begin end)
                                                     :type segment-type
                                                     :content (when content (substring (64tass--current-line) begin end)))))))
      (list :line-number (line-number-at-pos)
            :line-type line-type
            :columns columns))))


(defun 64tass--comment-start-index (line)
  "Return position of comment semicolon in LINE, ignoring strings and chars."
  (let ((i 0)
        (len (length line))
        (in-string nil)
        (in-char nil))
    (catch 'found
      (while (< i len)
        (let ((c (aref line i)))
          (cond
           ((and (not in-string) (not in-char) (eq c ?\"))
            (setq in-string t))
           ((and in-string (eq c ?\"))
            (setq in-string nil))
           ((and (not in-string) (not in-char) (eq c ?\'))
            (setq in-char t))
           ((and in-char (eq c ?\'))
            (setq in-char nil))
           ((and (not in-string) (not in-char) (eq c ?\;))
            (throw 'found i))))
        (setq i (1+ i)))
      nil)))

(defun 64tass--parse-line (&optional line)
  "Classify and parse LINE, returning a plist of describing its segments.
The segments are described by fields like :type, :opcode, :comment, etc.
The exact composition depends on :type.

LINE is optional, and if omitted the full current line at point will be used."
  (when (not line)
    (setq line (64tass--current-line)))
  (setq line (string-trim-right line))
  (with-temp-buffer
    (insert line)
    (goto-char (point-min))
    (let* ((code line)
           (comment nil))
      (when-let* ((comment-start (64tass--comment-start-index line))
                  (_ (> comment-start 0)))
        (setq comment (list :value (substring line comment-start)
                            :begin comment-start
                            :end (length line)))
        (setq code (string-trim-right (substring line 0 comment-start))))
      (cl-loop for (_type regex parser) in 64tass--line-classifiers
               when (string-match regex code)
               return (let ((parsed (funcall parser code)))
                        (if comment
                            (plist-put parsed :comment comment)
                          parsed))))))


(defun 64tass--span (n str)
  "Return a plist of :value, :begin, :end for match group N of STR.

Used by 64tass--parse-line to extract matched groups as segments from
a matched line."
  (when (match-beginning n)
    (list :value (match-string n str)
          :begin (match-beginning n)
          :end (match-end n))))



(provide '64tass-parse)

;;; 64tass-parse.el ends here
