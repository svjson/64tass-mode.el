;;; vice-emu-proc.el --- summary -*- lexical-binding: t -*-

;; Author: Sven Johansson
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Homepage: https://www.github.com/svjson/64tass-mode.el/packages/vice-emu-proc.el
;; Keywords: c64, Commodore 64, vice, emulator, retro, process


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

;; This file provides functions to invoke and manage VICE emulator processes.
;; It serves as a utility package for 64tass-mode.el, but is also available for
;; any mode or package needing VICE integration.

;;; Code:

(defun vice-emu--launch-file (file)
  "Launch the VICE emulator with FILE as argument, autostarting that."
  (call-process "x64" nil 0 nil file))

(provide 'vice-emu-proc)

;;; vice-emu-proc.el ends here
