;;; flycheck-64tass.el --- summary -*- lexical-binding: t -*-

;; Author: Sven Johansson
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Homepage: https://www.github.com/svjson/64tass-mode.el/packages/flycheck-64tass.el
;; Keywords: flycheck, syntax-checker, c64, Commodore 64, assembly, assembler, retro, programming


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

;; 64tass syntax and error checking for flycheck.

;;; Code:

(require 'cl-lib)
(require 'flycheck)


;; Configuration

(defvar flycheck-64tass-workspace-function nil
  "If non-nil, function returning the project entry file for 64tass.

When set, Flycheck runs in *project mode*:
- A temporary project workspace is created
- The workspace files/buffer contents are synced to it.
- 64tass is run on the entry file inside that workspace

The function must return a plist in the shape of:
    (:root <project-root-directory>
     :entry-file <entry-file-relative-to-root>
     :files (<list-of-all-project-files-relative-to-root>))")

(make-variable-buffer-local 'flycheck-64tass-entry-file-function)



;; State / Cache

(defvar flycheck-64tass--workspaces (make-hash-table :test 'equal)
  "Hash table mapping entry files to temp workspace directories.")

(defvar-local flycheck-64tass--workspace-root nil)



;; Internal workspace management

(defun flycheck-64tass--workspace (entry-file)
  "Return (and create if needed) the temp workspace for ENTRY-FILE.

Workspaces are keyed on the absolute path of the entry file.  This means
that source project folders with multiple entry points will use a separate
temporary workspace folder containing only the assets required by that
entry file."
  (or (gethash entry-file flycheck-64tass--workspaces)
      (let ((dir (make-temp-file "flycheck-64tass-" t)))
        (puthash entry-file dir flycheck-64tass--workspaces)
        dir)))

(defun flycheck-64tass--sync-workspace ()
  "Sync the configured workspace to its tmp folder.

This implementation greedily syncs all files listed by the
`flycheck-64tass-workspace-function' defined in the source buffer.

It prefers the contents of open file buffers for these files over
disk content."
  (let* ((source-ws (funcall flycheck-64tass-workspace-function))
         (source-root (plist-get source-ws :root))
         (entry-file (plist-get source-ws :entry-file))
         (fc-wc-root (flycheck-64tass--workspace (expand-file-name entry-file source-root))))
    (setq-local flycheck-64tass--workspace-root fc-wc-root)

    (dolist (file (plist-get source-ws :files))
      (let* ((src-file (expand-file-name file source-root))
             (dst-file (expand-file-name file fc-wc-root)))
        (make-directory (file-name-directory dst-file) t)
        (if-let ((buf (get-file-buffer src-file)))
            (with-current-buffer buf
              (write-region nil nil dst-file nil 'silent))
          (copy-file file dst-file t))))
    (expand-file-name entry-file fc-wc-root)))

(defun flycheck-64tass--strip-workspace-prefix (path ws-root)
  "Strip the workspace root WS-ROOT from absolute file name PATH."
  (let ((pos (string-search ws-root path)))
    (if pos
        (string-remove-prefix
         "/" (substring path (+ pos (length ws-root))))
      path)))

(defun flycheck-64tass--error-filter (errors)
  "Strip the tmp workspace directory root from file names in ERRORS."
  (if flycheck-64tass-workspace-function
      (mapcar (lambda (err)
                (setf (flycheck-error-filename err)
                      (flycheck-64tass--strip-workspace-prefix
                       (flycheck-error-filename err)
                       flycheck-64tass--workspace-root))
                err)
              errors)
    errors))


;; Command wrapper

(defun flycheck-64tass-command-wrapper (command)
  "Rewrite Flycheck COMMAND for 64tass project-mode operation.

If `flycheck-64tass-workspace-function' is nil, COMMAND is returned
unchanged (single-file mode).

If non-nil, the command is rewritten to:
- sync the current buffer into a temp project workspace
- run 64tass on the workspace entry file
- set `default-directory' to the workspace root"
  (if flycheck-64tass-workspace-function
      (let* ((entry-file (flycheck-64tass--sync-workspace)))
        (append (list (car command) entry-file) (cddr command)))
    command))



;; Flycheck checker

(flycheck-define-checker 64tass
  "A 6502 assembly syntax checker using 64tass.

Produces a flycheck command expecting that the source file is the entry
file. If the file belongs to a multi-file/include project, the variable
`flycheck-64tass-entry-file-function' can be set to a function returning
the project entry file. In that case, Flycheck will run 64tass on the
project entry file inside a temporary workspace containing the current buffer."
  :command ("64tass"
            source
            "--no-output"
            "-I"
            (eval (file-name-directory buffer-file-name)))
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": " (optional "fatal ") "error: " (message) line-end))
  :error-filter flycheck-64tass--error-filter
  :modes 64tass-mode
  :enabled (lambda () (eq '64tass-mode major-mode)))



;; Setup ergonomics

;;;###autoload
(defun flycheck-64tass-setup (&optional workspace-function)
  "Setup the flycheck-package, optionally with a local WORKSPACE-FUNCTION."
  (interactive)
  (when workspace-function
    (setq-local flycheck-64tass-workspace-function workspace-function)
    (setq-local flycheck-command-wrapper-function #'flycheck-64tass-command-wrapper))
  (add-to-list 'flycheck-checkers '64tass))

(provide 'flycheck-64tass)

;;; flycheck-64tass.el ends here
