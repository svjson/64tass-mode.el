;;; 64tass-mode.el --- summary -*- lexical-binding: t -*-

;; Author: Sven Johansson <johanssons.sven@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (64tass-proc "0.1.0") (company-64tass "0.1.0") (flycheck-64tass "0.1.0"))
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

;; Defines 64tass-mode, a major mode for working with 64tass assembly code.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require '64tass-common)
(require '64tass-parse)
(require '64tass-docblock)
(require '64tass-xref)
(require '64tass-proc)
(require '64tass-mem)
(require 'vice-emu-proc)

(condition-case err
    (progn (require 'flycheck)
           (require 'flycheck-64tass))
  (file-missing
   (message "Flycheck not found (optional): %s" (cadr err))))



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


(defcustom 64tass-on-assembly-success-function #'64tass--on-assembly-success
  "Callback function to execute following successful assembly using 64tass.

Takes one argument - the parsed assembly output, as returned by
`64tass-proc--parse-assembly-output'

This custom variable will shadow its counter-part in 64tass-proc-el in
tass64-mode buffers."
  :type 'function
  :group '64tass)


(defcustom 64tass-on-assembly-error-function #'64tass--on-assembly-error
  "Callback function to execute following assembly failure using 64tass.

Takes two arguments - the parsed assembly output, as returned by
`64tass-proc--parse-assembly-output' and the encountered error.  If multiple
errors were encountered, the first is provided as the second argument.

The full list of errors is available in the assembly-output argument, and
the second argument is provided merely as a convenience.

This custom variable will shadow its counter-part in 64tass-proc-el in
tass64-mode buffers."
  :type 'function
  :group '64tass)


;; Local variables

(defvar-local 64tass--column-cache '()
  "Contains cached lookups of local column layout calculations.")

(defvar-local 64tass--before-change-parsed-line nil
  "Snapshot of the current line composition before the last edit.")

(defvar-local 64tass--before-change-point nil
  "Snapshot of the point position before last edit.")


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

        (64tass--clear-line)

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

(defun 64tass--resolve-local-indentation (&optional line-number)
  "Resolve local indentation settings for the line at LINE-NUMBER.

This resolves the case where the closest preceding line of the same
type overrides the column configuration by example."
  (save-excursion
    (let ((line-number (or line-number (line-number-at-pos)))
          (instr-indent nil)
          (comment-indent nil)
          (bobp (when (eq 1 (line-number-at-pos)) t)))
      (forward-line -1)
      (while (and (or (not instr-indent)
                      (not comment-indent))
                  (not bobp))
        (when-let ((_ (not instr-indent))
                   (cached-instr-indent
                    (plist-get
                     (alist-get (line-number-at-pos) 64tass--column-cache)
                     :instr-indent)))
          (setq instr-indent (list :indent cached-instr-indent
                                   :source :cache
                                   :line (line-number-at-pos))))
        (when-let ((_ (not comment-indent))
                   (cached-comment-indent
                    (plist-get
                     (alist-get (line-number-at-pos) 64tass--column-cache)
                     :comment-indent)))
          (setq comment-indent (list :indent cached-comment-indent
                                     :source :cache
                                     :line (line-number-at-pos))))
        (when (or (not instr-indent)
                  (not comment-indent))
          (let* ((line (64tass--parse-line))
                 (type (plist-get line :type))
                 (opcode (plist-get line :opcode))
                 (comment (plist-get line :comment)))
            (when (eq type :instruction)
              (when (and (not instr-indent) opcode)
                (setq instr-indent (list :indent (plist-get opcode :begin)
                                         :source :line
                                         :line (line-number-at-pos))))
              (when (and (not comment-indent) comment)
                (setq comment-indent (list :indent (plist-get comment :begin)
                                           :source :line
                                           :line (line-number-at-pos)))))
            (if (eq 1 (line-number-at-pos))
                (setq bobp t)
              (forward-line -1)))))
      (64tass--cache-resolved-columns
       line-number
       (64tass--pruned-plist :instr-indent instr-indent
                             :comment-indent comment-indent))
      (list :instr-indent (plist-get instr-indent :indent)
            :comment-indent (plist-get comment-indent :indent)))))


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
         (segment-count (length segments))
         (current-index
          (or (cl-find-if (lambda (i)
                            (>= column (cdr (nth i positions))))
                          (number-sequence (1- segment-count) 0 -1))
              0))
         (previous-index (mod (- current-index 1) segment-count))
         (next-index     (mod (+ current-index 1) segment-count)))
    (list :point column
          :previous (nth previous-index segments)
          :current (nth current-index segments)
          :next (nth next-index segments))))


(defun 64tass--alignment-strategy (parsed-line point-context dir)
  "Determine the alignment strategy of an align-and-cycle operation.

Uses the PARSED-LINE representation of the target line and the resolved
POINT-CONTEXT to determine how alignment and cycling should be performed.

DIR determines the direction of the cursor cycling after alignment.

Most common cases are handled by a default strategy, that adheres to the
order and layout specified by `64tass-line-type-cycle-segments' and
`64tass-segment-composition'.

Exceptions to this symmetrical logic are tested for, before falling
back to the default strategy."
  (cond

   ;; Cycle comment-column on "blank" lines back to margin
   ((and (equal (plist-get parsed-line :type) :blank)
         (member :comment parsed-line)
         (>= (plist-get point-context :point)
             (-> parsed-line (plist-get :comment) (plist-get :begin))))
    (list :strategy :cycle-blank-comment
          :align #'64tass--delete-indentation
          :cycle-to :comment))

   ;; Default strategy
   (t
    (list :strategy :default
          :align #'64tass-align-current-line
          :cycle-to (plist-get point-context (if (equal -1 dir) :previous :next))))))

(defun 64tass-align-and-cycle (&optional dir)
  "Format the current line as needed and cycle through indentation contexts.

Cycles right through the line segments, unless -1 is provided as a value
for DIR."
  (interactive)
  (let* ((dir (or dir 1))
         (parsed-line (64tass--parse-line))
         (point-context (64tass--resolve-point-context parsed-line))
         (strategy (64tass--alignment-strategy parsed-line point-context dir)))
    (when-let ((align-function (plist-get strategy :align)))
      (funcall align-function))
    (let* ((reparsed (64tass--parse-line))
           (target-segment (plist-get strategy :cycle-to))
           (target-col (or (-> reparsed (plist-get target-segment) (plist-get :begin))
                           (64tass--resolve-local-indent-for target-segment))))
      (delete-trailing-whitespace (line-beginning-position) (line-end-position))
      (let ((line-len (save-excursion (end-of-line) (current-column))))
        (when-let ((overlap-segment
                    (cl-loop for (key val) on reparsed by #'cddr
                             when (and (not (equal key :type))
                                       (not (equal key target-segment))
                                       (>= target-col (plist-get val :begin))
                                       (<= target-col (plist-get val :end)))
                             return val)))
          (setq target-col (1+ (plist-get overlap-segment :end))))
        (when (> target-col line-len)
          (move-end-of-line nil)
          (indent-to target-col)))
      (move-to-column target-col))))

(defun 64tass-align-and-cycle-left ()
  "Indent/format the current line as needed and cycle, leftwards, through contexts."
  (interactive)
  (64tass-align-and-cycle -1))

(defun 64tass--enforce-column-layout ()
  "Attempt to uphold the segment columns as they were before and edit.

Designed to be triggered as an `after-change-functions' hook, and relies
on `64tass--before-change-parsed-line' to compare the current line composition
with the pre-edit dito."
  (unless 64tass--inhibit-formatting
    (when-let ((64tass--inhibit-formatting t)
               (before 64tass--before-change-parsed-line)
               (after (64tass--parse-line))
               (pnt (current-column)))
      (let ((next-type nil)
            (next-seg nil))
        (cl-loop for key in before by #'cddr
                 for seg = (plist-get before key)
                 when (not (equal key :type))
                 do (progn
                      (when (and (< pnt (plist-get seg :begin))
                                 (or (null next-seg)
                                     (< (plist-get seg :begin) (plist-get next-seg :begin))))
                        (setq next-seg seg)
                        (setq next-type key))))
        (when-let ((new-seg (plist-get after next-type))
                   (before-begin (plist-get next-seg :begin))
                   (after-begin (plist-get new-seg :begin)))
          (save-excursion
            (move-to-column after-begin)
            (cond

             ((< before-begin after-begin)
              (kill-backward-chars 1))

             ((> before-begin after-begin)
              (insert " ")))))))))



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



; Column resolution cache

(defun 64tass--clear-cache (line-number)
  "Invalidates all cache entries for lines after LINE-NUMBER.

This is called whenever a line is edited, as any cache entries
after the edited line may now be invalid."
  (setq 64tass--column-cache
        (cl-remove-if (lambda (entry)
                        (>= (car entry) line-number))
                      64tass--column-cache)))

(defun 64tass--group-indent-cache-values (resolved)
  "Return descending alist of (line . plist) for RESOLVED indent cache."
  (let* ((key-info
          (cl-loop for (key val) on resolved by #'cddr
                   collect (list key (plist-get val :indent) (plist-get val :line))))
         (line-points (sort (delete-dups (mapcar (lambda (x) (nth 2 x)) key-info)) #'>))
         (result '()))
    (dolist (line line-points)
      (let ((active
             (cl-loop for (key indent src-line) in key-info
                      when (<= src-line line)
                      append (list key indent))))
        (setq result (append result (list (cons line active))))))
    (sort result (lambda (a b) (> (car a) (car b))))))

(defun 64tass--put-indent-cache (line-number new-plist)
  "Insert or update the column indent cache for LINE-NUMBER with NEW-PLIST.
If an entry exists, merge it with NEW-PLIST, preferring NEW-PLIST values."
  (let ((existing (assoc line-number 64tass--column-cache)))
    (if existing
        (setcdr existing
                (let ((old (cdr existing)))
                  ;; Merge, preferring values from new-plist
                  (cl-loop for (key val) on new-plist by #'cddr
                           do (setq old (plist-put old key val)))
                  old))
      (setq 64tass--column-cache
            (cons (cons line-number new-plist)
                  64tass--column-cache)))))

(defun 64tass--cache-resolved-columns (line-number values)
  "Stores column resolution VALUES to `64tass--column-cache'.

The values are stored backwards at intervals of 10 lines from the LINE-NUMBER
for which the resolution was performed up until the source line(s)."
  (let ((cache-entries (64tass--group-indent-cache-values values)))
    (when cache-entries
      (save-excursion
        (64tass--goto-line line-number)
        (64tass--put-indent-cache line-number (cdr (car cache-entries)))
        (while cache-entries
          (forward-line -10)
          (if (<= (line-number-at-pos) (caar cache-entries))
              (progn
                (64tass--goto-line (caar cache-entries))
                (64tass--put-indent-cache (line-number-at-pos) (cdr (car cache-entries)))
                (setq cache-entries (cdr cache-entries)))
            (64tass--put-indent-cache (line-number-at-pos) (cdr (car cache-entries))))))
      (setq 64tass--column-cache (sort 64tass--column-cache (lambda (a b) (< (car a) (car b))))))))



;; Newline and TAB behavior

(defun 64tass-newline ()
  "Contextual newline handler for 64tass-mode."
  (interactive)
  (let* ((line (64tass--current-line))
         (trim-line (string-trim line))
         (parsed (64tass--parse-line))
         (line-type (plist-get parsed :type)))
    (cond
     ((64tass-docblock--is-docblock-line)
      (64tass-docblock--newline))
     ((and (eq line-type :directive)
           (plist-get parsed :args))
      (progn (newline)
             (insert (-> parsed (plist-get :directive) (plist-get :value)))
             (insert " ")))

     ((equal trim-line ".byte")
      (64tass--clear-line))

     (t
      (newline-and-indent)))))

(defun 64tass-indent ()
  "Contextual tab/indent handler for 64tass-mode."
  (interactive)
  (cond
   ((64tass-docblock--is-docblock-line)
    (64tass-docblock--tab))

   (t
    (64tass-align-and-cycle))))

(defun 64tass-deindent ()
  "Contextual backtab/de-indent handler for 64tass-mode."
  (interactive)
  (cond
   ((64tass-docblock--is-docblock-line)
    (64tass-docblock--backtab))

   (t
    (64tass-align-and-cycle-left))))

(defun 64tass-cycle-number-at-point ()
  "Cycle the number format of the value at point.

Cycles in the order of hex -> decimal -> binary,
e.g, $01 -> 1 -> %00000001"
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (numstr (thing-at-point 'symbol t))
         (num (64tass-parse-number numstr))
         (start (car bounds))
         (end (cdr bounds)))
    (cond
     ((string-match "^\\([0-9]+\\)$" numstr)
      (64tass--replace-region start end (64tass-format-number num :hex)))

     ((string-match "^\\$\\([0-9a-fA-F]+\\)" numstr)
      (64tass--replace-region start end (64tass-format-number num :bin)))

     ((string-match "^%\\([01]+\\)" numstr)
      (64tass--replace-region start end (64tass-format-number num :dec))))))


;; Assembly

(defun 64tass-assemble-and-launch ()
  "Assemble the current buffer and launch the result in VICE."
  (interactive)
  (let* ((assembly-output (64tass-assemble-buffer))
         (error-count (plist-get assembly-output :error-count))
         (assembly-success-p (member error-count '(0 nil)))
         (output-file (plist-get assembly-output :output-file)))
    (when assembly-success-p
      (message "Launching '%s'..." output-file)
      (vice-emu--launch-file output-file))))

(defun 64tass--on-assembly-success (assembly-output)
  "Show assembly success message according to ASSEMBLY-OUTPUT.

Default handler for `64tass-on-assembly-success-function'."
  (message "Assembly finished with no errors.  Wrote: %s" (plist-get assembly-output :output-file)))

(defun 64tass--on-assembly-error (assembly-output first-error)
  "React to and handle assembly error according to ASSEMBLY-OUTPUT and FIRST-ERROR.

Default handler for `64tass-on-assembly-error-function'."
  (64tass--display-assembly-error assembly-output first-error)
  (64tass-proc-assembly-log-other-window)
  (64tass--go-to-assembly-error-location assembly-output first-error))

(defun 64tass--display-assembly-error (assembly-output first-error)
  "Show error assembly error message from ASSEMBLY-OUTPUT.

FIRST-ERROR contains the first error encountered in the 64tass output."
  (message (string-join
            (append
             (list (format "Assembly finished with errors: %s" (plist-get assembly-output :error-count))
                   (format "%s:" (plist-get first-error :message)))
             (plist-get first-error :source-ref))
            "\n")))

(defun 64tass--go-to-assembly-error-location (_assembly-output first-error)
  "Show buffer containing assembly error in ASSEMBLY-OUTPUT.

Opens the file containing the reported error that encountered first by 64tass -
FIRST-ERROR - unless already open, and then moves the cursor to the error
location."
  (let* ((source-buffer (find-buffer-visiting (plist-get first-error :input-file))))
    (when (buffer-live-p source-buffer)
      (let ((window (display-buffer source-buffer)))
        (when (window-live-p window)
          (select-window window)
          (goto-char (point-min))
          (forward-line (1- (plist-get first-error :line)))
          (beginning-of-line)
          (forward-char (plist-get first-error :column)))))))



;; eldoc
(defun 64tass-eldoc-function ()
  "Provide eldoc documentation for 64tass / 6502 symbols at point."
  (let* ((sym (thing-at-point 'symbol t))
         (sym-num (and sym (64tass-parse-number sym)))
         (num-conversions (when sym-num
                            (format "Decimal: %s   Hex: %s   Binary: %s"
                                    (64tass-format-number sym-num :dec)
                                    (64tass-format-number sym-num :hex)
                                    (64tass-format-number sym-num :bin))))
         (mem-map-entry (when sym-num
                          (let ((entry (64tass--lookup-memory-doc sym-num)))
                            (concat
                             (if-let ((range (plist-get entry :range)))
                                 (format "Memory Area: %s - %s" (car range) (cdr range))
                               (format "Memory Address: %s" (plist-get entry :address)))
                             "\n\n"
                             (string-join (plist-get entry :text) "\n")))))
         (entries (cl-remove nil (list mem-map-entry num-conversions))))
    (string-join entries "\n\n")))


;; 64tass-mode

(defun 64tass--before-change-function (_beg _end)
  "Before change function hook handler for 64tass-mode."
  (setq 64tass--before-change-parsed-line (64tass--parse-line))
  (setq 64tass--before-change-point (point)))

(defun 64tass--after-change-function (beg _end _len)
  "After change function hook handler for 64tass-mode.

BEG, END and LEN according to the expected format of the
`after-change-functions' hook.

- Invalidates the column cache of all lines following the change.

- Reformats structures that may have become misaligned due to editing,
  such as code columns and docblocks.

Reformatting may not run during undo-operations, as it confuses the undo
marks and potentially destroys buffer contents."
  (64tass--clear-cache (line-number-at-pos beg))
  (when (not (bound-and-true-p undo-in-progress))
    (let* ((line (64tass--parse-line))
           (type (plist-get line :type)))
      (pcase type
        (:comment
         (when (64tass-docblock--is-docblock-line)
           (unless 64tass--inhibit-formatting
             (64tass-docblock--format-block-at (line-number-at-pos beg)))))
        (:instruction
         (64tass--enforce-column-layout))))))

(defconst 64tass-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'64tass-assemble-buffer)
    (define-key map (kbd "C-c C-e") #'64tass-assemble-and-launch)
    (define-key map (kbd "C-c C-n") #'64tass-cycle-number-at-point)
    (define-key map (kbd "C-c i h") #'64tass-insert-BASIC-header)
    (define-key map (kbd "C-c i b") #'64tass-docblock--insert-contextual)
    (define-key map (kbd "<backtab>") #'64tass-deindent)
    (define-key map (kbd "RET") #'64tass-newline)
    map))

;;;###autoload
(define-derived-mode 64tass-mode
  prog-mode
  "64tass"
  "Major mode for 6502/6510 assembly with 64tass."
  (set-syntax-table (make-syntax-table 64tass-mode-syntax-table))
  (setq-local font-lock-defaults '(64tass-font-lock-keywords))
  (setq-local indent-line-function '64tass-indent)
  (setq-local 64tass-proc-on-assembly-success-function 64tass-on-assembly-success-function)
  (setq-local 64tass-proc-on-assembly-error-function 64tass-on-assembly-error-function)
  (setq-local eldoc-documentation-function #'64tass-eldoc-function)

  (add-hook 'before-change-functions #'64tass--before-change-function nil t)
  (add-hook 'after-change-functions #'64tass--after-change-function nil t)

  (add-hook 'xref-backend-functions #'64tass-xref-backend nil t)
  (setq-local indent-tabs-mode nil)

  (when (featurep 'flycheck)
    (declare-function flycheck-64tass-setup 'flycheck-64tass)
    (flycheck-64tass-setup)))


(provide '64tass-mode)

;;; 64tass-mode.el ends here
