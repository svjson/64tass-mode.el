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

;; 64tass syntax checking for flycheck.

;;; Code:

(require 'flycheck)

(flycheck-define-checker 64tass
  "A 6502 assembly syntax checker using 64tass."
  :command ("64tass" source "--no-output" "-I" (eval (file-name-directory buffer-file-name)))
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": " (optional "fatal ") "error: " (message) line-end))
  :modes 64tass-mode
  :enabled (lambda () (eq '64tass-mode major-mode)))

;;;###autoload
(defun flycheck-64tass-setup ()
  "Setup flycheck-package."
  (interactive)
  (add-to-list 'flycheck-checkers '64tass))

(provide 'flycheck-64tass)

;;; flycheck-64tass.el ends here
