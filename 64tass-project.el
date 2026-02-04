;;; 64tass-project.el --- summary -*- lexical-binding: t -*-

;; Copyright (C) 2026 Sven Johansson

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

;; Contains project management functions for 64tass-mode.
;;
;; There is no formal or canonical definition of a 64tass project, but
;; simply a "main" file that is passed to the assembler, and that may include
;; other files.
;;
;; This file provides the struct for describing the project files, highlighting
;; the main file and a default function for assembling a project struct by
;; traversing `.include` directives

;;; Code:

(require 'cl-lib)



;; Custom variables

(defcustom 64tass-default-project-resolver
  #'64tass-project--resolve
  "Default function used to resolve a 64tass project."
  :type 'function
  :group '64tass)



;; Buffer-local variables

(defvar-local 64tass-project-resolver nil
  "Buffer-local project resolver.
If nil, fall back to `64tass-default-project-resolver`.

Modes deriving from 64tass-mode can set this local variable during mode
initialization to provide its own project resolver, ie from some kind of
build system project file or other arbitrary resolution scheme.

To consistently set the variable for specific project, use .dir-locals.el:

((64tass-mode
  . ((64tass-project-resolver . my-custom-project-resolver))))")



;; Struct

(cl-defstruct 64tass-project
  "Describes a 64tass/64tass-mode project.

This is used by various 64tass components to track the project contents
in order to know which file to assemble of run, symbol/label lookup via xref,
etc.

ROOT - the root directory of the project.
ENTRY-FILE - The main file to assemble.
FILES - a list of all project files.
INCLUDE-PATHS - (Optional) include paths to scan."
  root
  entry-file
  files
  include-paths)



;; Project Resolution

(defun 64tass-resolve-project ()
  "Resolve the current buffer's 64tass project using the configured function."
  (let ((fn (or 64tass-project-resolver 64tass-default-project-resolver)))
    (funcall fn)))

(defun 64tass-project--file-contents (file &optional prefer-buffer)
  "Get the contents of FILE, or its file buffer if PREFER-BUFFER."
  (if (and prefer-buffer
           (get-file-buffer file))
      (with-current-buffer (get-file-buffer file)
        (buffer-substring-no-properties (point-min) (point-max)))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun 64tass-project--file-includes (file &optional prefer-buffer)
  "Get a list of files included by FILE.

Scans the content of FILE, looking for `.include \"filename\"` directives.
If PREFER-BUFFER is non-nil, prefers the buffer content even if the file
exists on disk."
  (let ((content (64tass-project--file-contents file prefer-buffer))
        (include-regex "^[ \t]*\\.include[ \t]+\"\\([^\"]+\\)\"[ \t]*$"))
    (cl-remove-duplicates
     (cl-loop for line in (split-string content "\n" t)
              when (string-match include-regex line)
              collect (match-string 1 line))
     :test #'string-equal)))


(defun 64tass-project--resolve ()
  "Resolve the implicit project structure of the project of the current file."
  (let ((entry-file (64tass-project--find-root-file)))
    (when entry-file
      (let ((root (file-name-directory entry-file)))
        (make-64tass-project
         :root (file-name-directory entry-file)
         :entry-file (file-name-nondirectory entry-file)
         :files (mapcar (lambda (f) (file-relative-name f root))
                        (64tass-project--include-tree entry-file)))))))

(defun 64tass-project--find-root-file (&optional file prefer-buffer)
  "Find the root file of FILE.

Scans files adjacent to FILE, looking for `.include \"filename\"` directives.
If PREFER-BUFFER is non-nil, prefers the buffer content for all files that
have open file buffers, even if the file exists on disk."
  (let* ((file (or file (buffer-file-name)))
         (includers (64tass-project--find-including-files file prefer-buffer)))
    (if includers
        (car (cl-remove-duplicates (mapcar (lambda (f) (64tass-project--find-root-file f prefer-buffer)) includers)))
      file)))

(defun 64tass-project--file-list (&optional dir)
  "Retrieves a list of all assembly files in DIR and parent directories.

Looks upwards at parent directories until a directory is encountered that does
not contain any assembly files."
  (let* ((dir (or dir (file-name-directory (buffer-file-name))))
         (files (directory-files dir t "\\.\\(asm\\|inc\\|s\\)\\'")))
    (when files
      (append files (64tass-project--file-list (file-name-parent-directory dir))))))

(defun 64tass-project--find-including-files (&optional file prefer-buffer)
  "Find all source files that refer to FILE with an .include directive.

Scans the content of adjacent files, looking for `.include \"filename\"`
directives. If PREFER-BUFFER is non-nil, prefers file content from open
buffers over disk content."
  (let* ((file (or file (buffer-file-name)))
         (file-dir (file-name-directory file))
         (file-list (64tass-project--file-list file-dir)))
    (cl-loop for f in file-list
             when (member (file-relative-name file (file-name-directory f))
                          (64tass-project--file-includes f prefer-buffer))
             collect f)))

(defun 64tass-project--include-tree (from-file)
  "Recursively find all files included by FROM-FILE."
  (append (list from-file) (mapcan #'64tass-project--include-tree
                                   (mapcar
                                    (lambda (f)
                                      (expand-file-name f (file-name-directory from-file)))
                                    (64tass-project--file-includes from-file)))))


(provide '64tass-project)

;;; 64tass-project.el ends here
