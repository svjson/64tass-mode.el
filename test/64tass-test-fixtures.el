;;; 64tass-test-fixtures.el --- summary -*- lexical-binding: t -*-

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

;; Utility functions and fixtures for ERT tests, maintaining a focused
;; abstraction level on the feature being tested.

;;; Code:

(defmacro with-cursor-in-64tass-mode-buffer (settings content &rest body)
  "Execute BODY in the context of a temp-buffer with SETTINGS and CONTENT.

Temporarily set SETTINGS (alist of variable/value)
Insert CONTENT into a temp buffer with `|` marking point.
Enable `64tass-mode`, then run BODY."
  `(let ((original-settings (mapcar (lambda (pair)
                                      (cons (car pair) (symbol-value (car pair))))
                                    ',settings)))
     (unwind-protect
         (progn
           ,@(mapcar (lambda (pair)
                       `(setf ,(car pair) ,(cdr pair)))
                     settings)
           (with-temp-buffer
             (setq-local font-lock-mode nil)
             (insert ,content)
             (goto-char (point-min))
             (when (search-forward "▮" nil t)
               (delete-char -1))
             (64tass-mode)
             ,@body))
       ;; Restore original settings
       ,@(mapcar (lambda (pair)
                   `(setf ,(car pair) (cdr (assoc ',(car pair) original-settings))))
                 settings))))

(defun sim-type-key (keychar)
  (let ((last-command-event keychar))
    (call-interactively #'self-insert-command)))

(defun sim-type-backspace ()
  (let ((last-command-event 127))
    (call-interactively #'backward-delete-char)))

(provide '64tass-test-fixtures)

;;; 64tass-test-fixtures.el ends here
