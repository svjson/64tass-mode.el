;;; 64tass-mode.el --- summary -*- lexical-binding: t -*-

;; Author: Sven Johansson <johanssons.sven@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (64tass "0.1.0") (company-64tass "0.1.0") (flycheck-64tass "0.1.0"))
;; Homepage: https://www.github.com/svjson/64tass-mode.el
;; Keywords: c64, Commodore 64, assembly, retro, programming

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

;; commentary

;;; Code:

(require 'cl-lib)
(require 'dash)


;; Custom variables

(defcustom 64tass-left-margin-indent 0
  "Default level for left margin-aligned content.
Should invariably be 0, since 64tass requires this content to have zero
indentation.  Left configurable to enable derived work to configure this."
  :type 'integer
  :group '64tass)

(defcustom 64tass-instruction-column-indent 20
  "Default level of indentation for assembly instructions."
  :type 'integer
  :group '64tass)

(defcustom 64tass-comment-column-indent 40
  "Default level of indentation for post-instruction comments."
  :type 'integer
  :group '64tass)

(defcustom 64tass-insert-basic-header-comment t
  "Insert BASIC source comment before basic header."
  :type 'boolean
  :group 'languages)


;; Regex constants

(defconst 64tass-6502-opcode-list '("adc" "anc" "and" "ane" "arr" "asl" "asr" "bcc" "bcs" "beq" "bit" "bmi" "bne"
                                    "bpl" "brk" "bvc" "bvs" "clc" "cld" "cli" "clv" "cmp" "cpx" "cpy" "dcp" "dec"
                                    "dex" "dey" "eor" "inc" "inx" "iny" "isb" "jam" "jmp" "jsr" "lax" "lda" "lds"
                                    "ldx" "ldy" "lsr" "nop" "ora" "pha" "php" "pla" "plp" "rla" "rol" "ror" "rra"
                                    "rti" "rts" "sax" "sbc" "sbx" "sec" "sed" "sei" "sha" "shs" "shx" "shy" "slo"
                                    "sre" "sta" "stx" "sty" "tax" "tay" "tsx" "txa" "txs" "tya"))

(defconst 64tass-6502-opcode-regex (concat "\\<" (regexp-opt 64tass-6502-opcode-list) "\\>"))

(defconst 64tass-symbol-regex "[a-zA-Z_][a-zA-Z0-9_]+")

(defconst 64tass-constant-decl-regex (concat 64tass-symbol-regex "\s*="))
(defconst 64tass-assembly-address-regex "^\\(\\*=\\)")
(defconst 64tass-instruction-regex (concat "[[:blank:]]+" 64tass-6502-opcode-regex))
(defconst 64tass-banner-label-regex (concat "^" 64tass-symbol-regex ":"))
(defconst 64tass-comment-regex "\\(;.*\\)")


;; Font-locking

(defconst 64tass-font-lock-keywords
  `(;; Preprocessor
    ("\\.[[:alpha:]]+" . font-lock-keyword-face)

    ;; Assembly address
    ("^\\(\\*=\\)\\(\\$[0-9a-fA-F]+\\)"
     (1 font-lock-constant-face)
     (2 font-lock-function-name-face))

    ;; Comment line
    (,(concat  "^" 64tass-comment-regex)
     (1 font-lock-comment-face))

    ;; Constant declaration
    (,(concat "^\\(" 64tass-symbol-regex "\\)[[:blank:]]*=") 1 font-lock-function-name-face)

    ;; Label declaration
    (,(concat "^\\(" 64tass-symbol-regex "\\)\\(:?\\)\\>")
     (1 font-lock-type-face)
     (2 'bold))

    ;; Hi/Lo ref
    (,(concat "\\#[\\>\\<]" 64tass-symbol-regex) . font-lock-function-name-face)

    ;; Constant hex value
    ("=" "\\$[0-9a-fA-F]+" nil nil (0 'bold))

    ;; Highlight opcodes with optimized regexp for all valid 6502/6510 opcodes, including illegal/undocumented opcodes
    (,(concat "[[:blank:]]+" 64tass-6502-opcode-regex) 0 font-lock-keyword-face)

    ;; Comment line
    (,64tass-comment-regex
     (1 font-lock-comment-face))

    ;; Opcode argument value
    (,64tass-6502-opcode-regex "\\#[\\$\\%]\\(?:[0-9a-fA-F]+\\)" nil nil (0 'bold))

    ;; Opcode argument address
    (,64tass-6502-opcode-regex "\\$\\(?:[0-9a-fA-F]+\\)" nil nil (0 'font-lock-variable-name-face))

    ;; Opcode argument label/constant
    (,64tass-instruction-regex ,(concat "[[:blank:]]+\\(" 64tass-symbol-regex "\\)") nil nil (1 'font-lock-preprocessor-face))

    ;; Opcode register arg
    (,(concat "\\([[:blank:]]+" 64tass-6502-opcode-regex "\\).*\\,[[:space:]]?\\([xy]\\)")
     (2 'bold))
    ))


;; Line type segment composition

(defconst 64tass-line-type-cycle-segments
  '((:assembly-address  :address :comment)
    (:blank             :label :opcode :comment)
    (:directive         :directive :comment)
    (:instruction       :label :opcode :comment)
    (:label             :label-standalone :comment)
    (:label+instruction :label :opcode :comment)))

(defconst 64tass-segment-composition
  '((:opcode               :opcode " " :operand)
    (:directive            :directive " " :args)
    (:label-standalone     :label-standalone ":")
    (:address              -2 "*=" :address)))


;; Line parsing for content-aware navigation and formatting

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
             :name (64tass--span 1 str)
             :value (64tass--span 2 str))))

    (:directive
     "^\\s-*\\(\\.[a-z]+\\)\\s-+\\(.*\\)"
     (lambda (str)
       (list :type :directive
             :directive (64tass--span 1 str)
             :args (64tass--span 2 str))))

    (:label+instruction
     "^\\s-*\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-+\\([a-z]\\{3\\}\\)\\(?:\\s-+\\(.*\\)\\)?\\s-*$"
     (lambda (str)
       (let ((parsed (list :type :label+instruction
                           :label (64tass--span 1 str)
                           :opcode (64tass--span 2 str))))
         (if (match-beginning 3)
             (plist-put parsed :operand (64tass--span 3 str))
           parsed))))

    (:instruction
     "^\\s-*\\([a-z]\\{3\\}\\)\\(?:\\s-+\\(.*\\)\\)?\\s-*$"
     (lambda (str)
       (let ((parsed (list :type :instruction
                           :opcode (64tass--span 1 str))))
         (if (match-beginning 2)
             (plist-put parsed :operand (64tass--span 2 str))
           parsed))))

    ;; fallback
    (:unknown
     ".*"
     (lambda (_)
       (list :type :unknown)))))

(defun 64tass--current-line ()
  "Get the full contents of the current line as string."
  (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))

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
           (comment nil)
           (comment-start (when (string-match
                                 "^\\s-*\\(?:[^;[:space:]]+\\s-*\\)+\\(;.*\\)"
                                 line)
                            (match-beginning 1))))
      (when comment-start
        (setq comment (list :value (match-string 1 line)
                            :begin comment-start
                            :end (match-end 1)))
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


;; Formatting and tab behavior

(defun 64tass-align-current-line (&optional prefer-default)
  "Align the current line to the contextual column settings.

By default, the custom variables `64tass-instruction-column-indent' and
`64tass-comment-column-indent' are used to align the instruction and comment,
but if previous lines exist with a different formatting, that will be used
instead.

Looking at previous lines can be disabled by passing a non-nil value for
PREFER-DEFAULT.  In that case, the default values of
`64tass-instruction-column-indent' and `64tass-comment-column-indent' will be
used for alignment."
  (interactive)
  (save-excursion
    (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
           (parsed-line (64tass--parse-line line))
           (local-indentation (if prefer-default
                                  (list :instr-indent 64tass-instruction-column-indent
                                        :comment-indent 64tass-comment-column-indent)
                                (64tass--resolve-local-indentation)))
           (segments (alist-get (plist-get parsed-line :type) 64tass-line-type-cycle-segments))
           (current-format (cl-loop for segment in segments
                                    for pos = (-> parsed-line
                                                  (plist-get segment)
                                                  (plist-get :begin))
                                    when (numberp pos)
                                    collect (cons segment pos)))
           (target-format (cl-mapcar (lambda (seg)
                                       (cons (car seg)
                                             (64tass--resolve-local-indent-for
                                              (car seg)
                                              local-indentation)))
                                     current-format)))

      (unless (equal current-format target-format)

        (delete-region (line-beginning-position) (line-end-position))

        (mapc (lambda (seg)
                (indent-to (cdr seg))
                (when (> (current-column) (cdr seg))
                  (insert " "))
                (if-let ((seg-comp (alist-get (car seg) 64tass-segment-composition)))
                    (dolist (part seg-comp)
                      (cond
                       ((stringp part)
                        (insert part))

                       ((integer-or-marker-p part)
                        (kill-backward-chars 2))

                       (t
                        (insert (-> parsed-line (plist-get part) (plist-get :value))))))
                  (insert (-> parsed-line (plist-get (car seg)) (plist-get :value)))))
              target-format)))))

(defun 64tass--resolve-local-indentation ()
  "Resolve local indentation settings for the current line.

This resolves the case where the closest preceding line of the same
type overrides the column configuration by example."
  (save-excursion
    (let ((instr-indent nil)
          (comment-indent nil)
          (bobp (when (eq 1 (line-number-at-pos)) t)))
      (forward-line -1)
      (while (and (or (not instr-indent)
                      (not comment-indent))
                  (not bobp))
        (let* ((line (64tass--parse-line))
               (type (plist-get line :type))
               (opcode (plist-get line :opcode))
               (comment (plist-get line :comment)))
          (when (member type '(:instruction :label+instruction))
            (setq instr-indent (plist-get opcode :begin))
            (when comment
              (setq comment-indent (plist-get comment :begin))))
          (if (eq 1 (line-number-at-pos))
              (setq bobp t)
            (forward-line -1))))
      (list :instr-indent instr-indent
            :comment-indent comment-indent))))

(defun 64tass--resolve-contextual-indent-for (seg-type &optional local-indentation)
  "Resolve local indentation for SEG-TYPE.

Uses `64tass--resolve-local-indentation' to find the closest preceding
line of the same type, and returns the indentation level for that type.
Or, optionally uses the pre-computed LOCAL-INDENTATION, if provided.

SEG-TYPE can be one of `:left-margin', `:instruction', or `:comment'."
  (let* ((local-indent (or local-indentation (64tass--resolve-local-indentation)))
         (resolved (pcase seg-type
                     (:instruction (plist-get local-indent :instr-indent))
                     (:comment (plist-get local-indent :comment-indent)))))
    (or resolved
        (pcase seg-type
          (:left-margin 0)
          (:instruction 64tass-instruction-column-indent)
          (:comment 64tass-comment-column-indent)))))

(defun 64tass--resolve-local-indent-for (seg-type &optional local-indentation)
  "Resolve local indentation for SEG-TYPE.

Uses `64tass--resolve-local-indentation' to find the closest preceding
line of the same type, and returns the indentation level for that type.

SEG-TYPE can be one of the segment types defined by
`64tass-line-type-cycle-segments`.

LOCAL-INDENTATION can optionally be provided to avoid repeated traversing
for segment types that require resolveing contextual intendtation."
  (pcase seg-type
    (:label 0)
    (:label-standalone 0)
    (:directive 0)
    (:address 2)
    (:opcode (64tass--resolve-contextual-indent-for :instruction local-indentation))
    (:comment (64tass--resolve-contextual-indent-for :comment local-indentation))))

(defun 64tass--resolve-point-context (parsed-line &optional column)
  "Resolve the point context for the current line based on its parsed content.

This function determines whether the point is in the left margin, instruction
column, or comment column based on the PARSED-LINE content.

COLUMN is optional and is derived from (current-column) if omitted.

It returns a plist with contextual hints for next and previous contexts, which
is used for cycling behavior."
  (let* ((column (or column (current-column)))
         (type (plist-get parsed-line :type))
         (segments (alist-get type 64tass-line-type-cycle-segments))
         (positions
          (cl-loop for segment in segments
                   for pos = (or (-> parsed-line
                                     (plist-get segment)
                                     (plist-get :begin))
                                 (64tass--resolve-local-indent-for segment))
                   when (numberp pos)
                   collect (cons segment pos)))
         (next-segments
          (cl-find-if (lambda (pair)
                        (< column (cdr pair)))
                      positions))
         (next (or (car next-segments)
                   (car segments)))
         (current (let ((prev-index (- (cl-position next segments :test #'equal) 1)))
                    (if (= prev-index -1)
                        (car (last segments))
                      (nth prev-index segments)))))
    (list :point column :current current :next next)))

(defun 64tass-align-and-cycle ()
  "Indent/format the current line as needed and cycle through indentation contexts."
  (interactive)
  (let* ((parsed-line (64tass--parse-line))
         (point-context (64tass--resolve-point-context parsed-line)))
    (64tass-align-current-line)
    (let* ((reparsed (64tass--parse-line))
           (next-segment (plist-get point-context :next))
           (target-col (or (-> reparsed (plist-get next-segment) (plist-get :begin))
                           (64tass--resolve-local-indent-for next-segment)))
           (line-len (save-excursion (end-of-line) (current-column))))
      (delete-trailing-whitespace (line-beginning-position) (line-end-position))
      (when (> target-col line-len)
        (move-end-of-line nil)
        (indent-to target-col))
      (move-to-column target-col))))




(defun 64tass-to-decimal-string (input)
  "Convert an INPUT hex string to decimal string.
A string representation of hexadecimal number will be converted a string
containing its corresponding integer value."
  (let (;;(hex (string-match "\\$[[:digit:]]+" input))
        (dec (string-match "[[:digit:]]+" input)))
    (if (= dec 0)
        input
      (number-to-string (string-to-number (substring input 1) 16)))))




(defun 64tass-insert-BASIC-header ()
  "Insert a basic program to bootstrap machine language program at cursor."
  (interactive)
  (let* ((input (string-trim (read-string "Enter program start address ($0810): ")))
         (raw-addr (if (string-empty-p input)
                       "$0810"
                     input))
         (prog-addr (64tass-to-decimal-string raw-addr)))
    (beginning-of-line)
    (insert "*=$0801")
    (newline)
    (when 64tass-insert-basic-header-comment
      (progn
        (insert (concat "; 10 SYS " prog-addr))
        (newline)))
    (insert ".byte $0c, $08, $0a, $00, $9e, $20")
    (newline)
    (insert (concat ".byte " (let ((nums ()))
                               (dotimes (i (length prog-addr))
                                 (push (concat "$3" (substring prog-addr i (+ i 1))) nums))
                               (dotimes (_ (- 7 (length prog-addr)))
                                 (push "$00" nums))
                               (string-join (nreverse nums) ", "))))
    (newline)
    (newline)
    (insert (concat "*=" raw-addr ))
    (newline)))



(defvar 64tass-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-b") #'64tass-insert-BASIC-header)
    map))

;;;###autoload
(define-derived-mode 64tass-mode
  prog-mode
  "64tass"
  "Major mode for 6502/6510 assembly with 64tass."
  (set-syntax-table (make-syntax-table 64tass-mode-syntax-table))
  (setq-local font-lock-defaults '(64tass-font-lock-keywords))
  (setq-local indent-line-function '64tass-align-and-cycle)

  (electric-indent-local-mode -1))


(provide '64tass-mode)

;;; 64tass-mode.el ends here
