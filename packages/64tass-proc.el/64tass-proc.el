;;; 64tass-proc.el --- summary -*- lexical-binding: t -*-

;; Author: Sven Johansson
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Homepage: https://www.github.com/svjson/64tass-mode.el/packages/64tass-proc.el
;; Keywords: c64, Commodore 64, assembly, assembler, retro, programming


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

;; This file contains functions for invoking the 64tass assembler and
;; parsing its output.  It is a utility package that is used by 64tass-mode.el,
;; flycheck-64tass.el and company-64tass.el.

;;; Code:

(require 'time-stamp)
(require 'cl-lib)
(require 'dash)


;; Buffer-local variables

(defvar-local 64tass-proc-output-file nil
  "Output filename for the assembled binary.
If nil, defaults to replacing the buffer's extension with `.prg`
or `64tass-proc-output-file-extension`.")

(defvar-local 64tass-proc-args nil
  "List of additional arguments to pass to 64tass.")



;; Custom variables

(defcustom 64tass-proc-output-file-extension ".prg"
  "File extension for assembled binaries.
If nil, defaults to replacing the buffer's extension with `.prg`"
  :type 'string
  :group '64tass)

(defcustom 64tass-proc-on-assembly-success-function #'ignore
  "Callback function to execute following successful assembly using 64tass.

Takes one argument - the parsed assembly output, as returned by
`64tass-proc--parse-assembly-output'

By default, the function handler is set to `ignore' as 64tass-proc.el is
not intended for standalone usage, and it will be up to the consuming package
to provide suitable handler functions for its purposes."
  :type 'function
  :group '64tass)

(defcustom 64tass-proc-on-assembly-error-function #'ignore
  "Callback function to execute following assembly failure using 64tass.

Takes two arguments - the parsed assembly output, as returned by
`64tass-proc--parse-assembly-output' and the encountered error.  If multiple
errors were encountered, the first is provided as the second argument.

The full list of errors is available in the assembly-output argument, and
the second argument is provided merely as a convenience.

By default, the function handler is set to `ignore' as 64tass-proc.el is
not intended for standalone usage, and it will be up to the consuming package
to provide suitable handler functions for its purposes."
  :type 'function
  :group '64tass)

(defcustom 64tass-proc-log-buffer-name "*64tass assembly log*"
  "Name of the assembly log buffer."
  :type 'string
  :group '64tass)



;; Assembly output

(defun 64tass-proc-output-filename (source-file)
  "Determine the output filename for source file SOURCE-FILE."
  (or 64tass-proc-output-file
      (concat (file-name-sans-extension source-file)
              (or 64tass-proc-output-file-extension ".prg"))))

(defun 64tass-proc--parse-assembly-output (output)
  "Parse 64tass assembler OUTPUT string into structured data."
  (let ((lines (split-string output "\n" t))
        result segments errors)
    (cl-loop while lines
             for line = (pop lines)
             do (cond
                 ;; Input file
                 ((string-match "^Assembling file:\\s-+\\(.+\\)$" line)
                  (setq result (plist-put result :input-file (match-string 1 line))))
                 ;; Output file
                 ((string-match "^Output file:\\s-+\\(.+\\)$" line)
                  (setq result (plist-put result :output-file (match-string 1 line))))
                 ;; Error count
                 ((string-match "^Error messages:\\s-+\\(.+\\)$" line)
                  (setq result (plist-put result :error-count (string-to-number (match-string 1 line)))))
                 ;; Pass count
                 ((string-match "^Passes:\\s-+\\([0-9]+\\)$" line)
                  (push (list :type :passes
                              :n (string-to-number (match-string 1 line)))
                        segments))
                 ;; Data or gap segment
                 ((string-match "^\\(Data\\|Gap\\):\\s-*\\([0-9]+\\)\\s-+\\(\\$[0-9a-fA-F]+\\)-\\(\\$[0-9a-fA-F]+\\)\\s-+\\(\\$[0-9a-fA-F]+\\)$" line)
                  (let* ((type-str (match-string 1 line))
                         (length   (string-to-number (match-string 2 line)))
                         (start-hx (match-string 3 line))
                         (end-hx   (match-string 4 line))
                         (type     (if (string= type-str "Data") :data :gap)))
                    (push (list :type type
                                :length length
                                :start (list :hex start-hx
                                             :dec (string-to-number (substring start-hx 1) 16))
                                :end   (list :hex end-hx
                                             :dec (string-to-number (substring end-hx 1) 16)))
                          segments)))
                 ;; Error message
                 ((and (string-prefix-p (or (plist-get result :input-file) "__INGRESS") line)
                       (string-match "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): error: \\(.*\\)$" line))
                  (let ((file (match-string 1 line))
                        (line (string-to-number (match-string 2 line)))
                        (col (string-to-number (match-string 3 line)))
                        (message (match-string 4 line))
                        (source-ref nil)
                        (enc-ref nil))
                    (while (and lines (not enc-ref))
                      (let ((line (pop lines)))
                        (push line source-ref)
                        (when (string-match "^\\s-*\\^\\s-*$" line)
                          (setq enc-ref t))))
                    (push (list :input-file file
                                :line line
                                :column col
                                :message message
                                :source-ref (nreverse source-ref))
                          errors)))))
    (when errors
      (setq result (plist-put result :errors (nreverse errors))))
    (setq result (plist-put result :segments (nreverse segments)))
    result))


;; Dump output

(defun 64tass--parse-label-dump-buffer ()
  "Parse label dump output in the current buffer.

This function expects to be called with the current buffer set to
a, presumably temporary, buffer containing the label file contents."
  (goto-char (point-min))
    (let (index)
      (while (not (eobp))
        (let ((line (string-split
                     (buffer-substring (line-beginning-position) (line-end-position))
                     "[:=]" t "\\s-")))
          (push (cons (nth 3 line) (list :file (car line)
                                         :linum (string-to-number (nth 1 line))
                                         :addr (nth 4 line)))
                index))
        (forward-line 1))
      index))

(defun 64tass--parse-label-dump-file (label-file)
  "Parse label dump file on disk at location LABEL-FILE."
  (with-temp-buffer
    (insert-file-contents label-file)
    (64tass--parse-label-dump-buffer)))


;; Log

(defun 64tass-proc-assembly-log-other-window (&rest _)
  "Open the assembly log in `other-window'.

Accepts and ignores any number of arguments, to make the function compatible
for use as `64tass-proc-on-assembly-success-function' and/or
`64tass-proc-on-assembly-error-function'."
  (switch-to-buffer-other-window 64tass-proc-log-buffer-name))

(defun 64tass-proc--append-to-log (output &optional log-buffer-name)
  "Append OUTPUT to the 64tass log buffer.

LOG-BUFFER-NAME may be passed to override the default log buffer."
  (with-current-buffer (get-buffer-create (or log-buffer-name
                                              64tass-proc-log-buffer-name))
    (special-mode)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (string-join (list "\n"
                                 "-- "
                                 (time-stamp-string "%:y-%02m-%02d %02H:%02M:%02S")
                                 " -----------------\n")
                           ""))
      (insert output))))



;; 64tass interaction

;;;###autoload
(defun 64tass-exec (args &optional include-proc-args append-to-log)
  "Invoke the `64tass` binary with ARGS and return exit code and console output.

Returns (<exit-code> . <console-output>).

The default process arguments defined in `64tass-proc-args' are prepended to the
supplied argument list for non-nil values of INCLUDE-PROC-ARGS.

If a non-nil value is passed for APPEND-TO-LOG, the console output of the
execution is added to a log buffer.  If the value is a string, this value will
be used for the buffer name, otherwise the default log buffer name defined by
64tass-proc--log-buffer-name will be used."
  (with-temp-buffer
    (let* ((args (if include-proc-args
                     (append 64tass-proc-args args)
                   args))
           (exit-code (apply #'call-process (executable-find "64tass")
                             nil
                             (current-buffer)
                             nil
                             args))
           (console-output (buffer-string)))
      (when append-to-log
        (64tass-proc--append-to-log console-output (when (stringp append-to-log) append-to-log)))
      (cons exit-code console-output))))

;;;###autoload
(defun 64tass-assemble-buffer ()
  "Send the current buffer to 64tass for assembly.

Uses `64tass-proc-output-file` and `64tass-proc-args` to determine the
file name of the output."
  (interactive)
  (let* ((output-file (64tass-proc-output-filename buffer-file-name))
         (exec-result (64tass-exec
                       (list buffer-file-name "-o" output-file)
                       t t))
         (console-output (cdr exec-result)))
    (let ((exit-code (car exec-result))
          (assembly-output (64tass-proc--parse-assembly-output console-output)))
      (if (not (zerop exit-code))
          (let ((first-error (-> assembly-output (plist-get :errors) (car))))
            (when 64tass-proc-on-assembly-error-function
              (funcall 64tass-proc-on-assembly-error-function assembly-output first-error)))
        (when 64tass-proc-on-assembly-success-function
          (funcall 64tass-proc-on-assembly-success-function assembly-output)))
      assembly-output)))

;;;###autoload
(defun 64tass-dump-labels ()
  "Invoke 64tass to dump labels for the source in the current buffer."
  (let* ((source-buffer-file buffer-file-name)
         (tmp-dir (make-temp-file "64tass-labels-" t))
         (label-file (expand-file-name "labels.out" tmp-dir))
         (result (64tass-exec
                    (list source-buffer-file
                          "--no-output"
                          "--dump-labels"
                          "-l"
                          label-file)
                    t nil))
         (label-index (when (zerop (car result)) (64tass--parse-label-dump-file label-file))))
    (delete-directory tmp-dir t)
    label-index))

(provide '64tass-proc)

;;; 64tass-proc.el ends here
