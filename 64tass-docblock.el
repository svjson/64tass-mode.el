;;; 64tass-docblock.el --- summary -*- lexical-binding: t -*-

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

;; Contains functions for generating and managing/editing documentation
;; blocks and banners for 64tass code.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require '64tass-common)
(require '64tass-parse)


;; Docblock line identification / Query docblock lines

(defun 64tass-docblock--is-content-line (&optional str)
  "Test if STR or the current line is a docblock content line.

Content in this case refers to documentation content, ie anything that
has semantic or informational value - even if it is a blank content line -
and that is not decoration, e.g, horizontal dividers in banner blocks.

STR is optional, and if not provided the test is performed on the current
line / line at point."
  (let ((line (or str (64tass--current-line))))
    (string-match "^; ?|" line)))

(defun 64tass-docblock--is-divider-line (&optional str)
  "Test if STR or the current line is a docblock divider line.

Divider in this case refers to lines that divides sections of a docblock.
In a simple doc banner, this is a line that looks like this: \"; +-------+\".

STR is optional, and if not provided the test is performed on the current
line / line at point."
  (let ((line (or str (64tass--current-line))))
    (string-match "^; ?\\+?\\-+\\+?$" line)))

(defun 64tass-docblock--is-docblock-line (&optional line)
  "Return non-nil if LINE is a docblock line (divider or content).

If the line tests true for either `64tass-docblock--is-content-line' or
`64tass-docblock--is-divider-line', the result is non-nil.

LINE is optional.  If omitted, the current line at point is tested.
If provided, LINE may be either:

- an integer (line number, 1-based)
- a string (line content to test)"
  (save-excursion
    (let ((line-content
           (string-trim
            (cond
             ((null line)
              (64tass--current-line))

             ((numberp line)
              (64tass--get-line line))

             ((stringp line)
              line)

             (t
              (error "Unsupported argument type for LINE: %S (%S)" line (type-of line)))))))
      (or (64tass-docblock--is-content-line line-content)
          (64tass-docblock--is-divider-line line-content)))))

(defun 64tass-docblock--boundary-predicate (linenum line collected at-boundary)
  "Predicate function for use with 64tass--walk-lines.

LINENUM, LINE, COLLECTED and AT-BOUNDARY as described by `64tass--walk-lines'.

Finds the last docblock line in the walk direction."
  (let ((docblock-line-p (64tass-docblock--is-docblock-line line)))
    (cond
     (docblock-line-p
      (list :collect (cons linenum line)))

     ((or (not docblock-line-p)
          at-boundary)
      (car collected)))))


(defun 64tass-docblock--block-bounds (&optional line-number)
  "Find the bounds, start and end line, of docblock at LINE-NUMBER.

LINE-NUMBER is optional, and if emitted search will start at POINT.

Returns the start and end line of the block as (<line number> . <line-number>)."
  (save-excursion
    (when line-number
      (64tass--goto-line line-number))
    (when (64tass-docblock--is-docblock-line)
      (cons (car (64tass--walk-lines -1 #'64tass-docblock--boundary-predicate))
            (car (64tass--walk-lines 1 #'64tass-docblock--boundary-predicate))))))



;; Docblock content extraction

(defun 64tass-docblock--line-content (&optional line include-bounds)
  "Extract the content between the first and last | in LINE.

If LINE is nil, use the current line at point.

Returns the string between the first and last | characters,
excluding leading \"; \" and whitespace padding.

Examples:
  ; | hello world |     => \"hello world\"
  ; | just one | pipe  => \"just one | pipe\"

If INCLUDE-BOUNDS is non-nil, the line-content is wrapped in a plist
structure.

Examples:
  ; | hello world |     => (:bounds (3 . 16) :content \"hello world\")
  ; | just one | pipe  => (:bounds (3 . 19) :content \"just one | pipe\")"
  (let* ((raw-line (string-trim (or line (64tass--current-line))))
         (beg (cl-position ?| raw-line))
         (end (when (equal (substring raw-line (1- (length raw-line))) "|")
                (1- (length raw-line))))
         (line-content
          (cond
           ((null beg)
            nil)
           ((null end)
            (string-trim-left (substring raw-line (+ 1 beg))))
           (t
            (string-trim (substring raw-line (1+ beg) end))))))
    (when line-content
      (if include-bounds
          (list :bounds (cons (1+ beg) end)
                :content line-content)
        line-content))))

(defun 64tass-docblock--find-block-start (&optional from-line)
  "Find the start of a docblock, starting from line number FROM-LINE.

LINE is optional, and if omitted the search starts from the current line"
  (save-excursion
    (when from-line
      (64tass--goto-line from-line))
    (64tass--walk-lines -1 #'64tass-docblock--boundary-predicate)))

(defun 64tass-docblock--section-content (&optional line-number)
  "Extract section content from docblock section at LINE-NUMBER."
  (let ((line-no (or line-number (line-number-at-pos))))
    (save-excursion
      (64tass--goto-line line-no)
      (let ((section-start
             (64tass--walk-lines
              -1
              (lambda (_linenum _line collected _at-boundary)
                (cond
                 ((64tass-docblock--is-content-line)
                  :collect)

                 (t
                  (caar collected)))))))
        (when section-start
          (64tass--goto-line section-start)
          (64tass--walk-lines
           1
           (lambda (linenum _line collected _at-boundary)
             (cond
              ((64tass-docblock--is-content-line)
               (list :collect (cons linenum (64tass-docblock--line-content))))

              (t
               (nreverse collected))))))))))

(defun 64tass-docblock--identify-section (content preceding-blocks)
  "Determine docblock section type using CONTENT and PRECEDING-BLOCKS."
  (let ((prev-block-type (plist-get (car (last preceding-blocks)) :type)))
    (cond
     ((equal content '("Arguments:"))
      :arguments-header)
     ((equal content '("Returns:"))
      :returns-header)
     ((null preceding-blocks)
      :function-header)
     ((equal prev-block-type :function-header)
      :description)
     ((equal prev-block-type :arguments-header)
      :arguments)
     ((equal prev-block-type :returns-header)
      :returns)
     (t
      :block))))

(defun 64tass-docblock--parse-docblock (&optional line-number)
  "Parse the docblock at LINE-NUMBER into a structured list.

The parsed docblock format is as follows:

 (:bounds (<linestart> . <lineend>)
  :sections ((:type function-header
              :bounds (<linestart> . <lineend>)
              :content (<line content> <line content>...))))

Returns nil if there is no docblock at the given line number."
  (save-excursion
    (let ((block-bounds (64tass-docblock--block-bounds line-number)))
      (when (car block-bounds)
        (64tass--goto-line (car block-bounds))
        (let ((sections '()))
          (while (<= (line-number-at-pos) (cdr block-bounds))
            (cond
             ((64tass-docblock--is-content-line)
              (let* ((content (64tass-docblock--section-content))
                     (unwrapped (mapcar #'cdr content)))
                (setq sections
                      (append
                       sections
                       (list
                        (list :type (64tass-docblock--identify-section unwrapped
                                                                       sections)
                              :bounds (cons (caar content)
                                            (caar (last content)))
                              :content unwrapped))))
                (64tass--goto-line (caar (last content))))))
            (forward-line 1))
          (list :bounds block-bounds
                :sections sections))))))


;; Query functions for parsed docblock structures

(defun 64tass-docblock--get-section (block section-type)
  "Get section of type SECTION-TYPE from BLOCK."
  (cl-find-if (lambda (section)
                (eq (plist-get section :type) section-type))
              (plist-get block :sections)))

(defun 64tass-docblock--content-length (block)
  "Get the minimum required line length required to display content of BLOCK."
  (let ((len 3))
    (mapc
     (lambda (section)
       (mapc
        (lambda (line)
          (when (length> line len)
            (setq len (length line))))
        (plist-get section :content)))
     (plist-get block :sections))
    len))

(defun 64tass-docblock--next-section (block from-section dir)
  "Return the section of BLOCK that follows FROM-SECTION in the direction of DIR."
  (when-let ((dir (or dir 1))
             (current-index (cl-position-if
                             (lambda (section)
                               (equal from-section (plist-get section :type)))
                             (plist-get block :sections))))
    (nth (+ dir current-index) (plist-get block :sections))))


;; Docblock formatting

(defun 64tass-docblock--format-block-at (&optional line)
  "Format / re-format the docblock at line number LINE."
  (let ((64tass--inhibit-formatting t))
    (let ((point-pos (cons (current-column) (line-number-at-pos))))
      (when line
        (64tass--goto-line line))
      (let* ((block (64tass-docblock--parse-docblock))
             (bounds (plist-get block :bounds))
             (content-length (64tass-docblock--content-length block)))
        (save-excursion
          (64tass--goto-line (car bounds))
          (dotimes (_ (- (cdr bounds) (1- (car bounds))))
            (let ((current-line (64tass--current-line)))
              (cond
               ((64tass-docblock--is-divider-line current-line)
                (progn
                  (64tass--clear-line)
                  (insert (concat "; +" (make-string (+ 2 content-length) ?-) "+"))))
               ((64tass-docblock--is-content-line current-line)
                (let ((line-content (64tass-docblock--line-content)))
                  (64tass--clear-line)
                  (insert (concat "; | " line-content))
                  (indent-to (+ 5 content-length))
                  (insert "|"))))
              (forward-line 1))))
        (move-to-column (car point-pos))))))




;; Docblock generation

(defconst 64tass-docblock--banner-divider "; +---+")

(defun 64tass-docblock--next-docable (&optional line)
  "Find next documentable line, starting at and including line number LINE."
  (save-excursion
    (when line
      (64tass--goto-line line))
    (64tass--walk-lines
     1
     (lambda (linenum line _collected _at-boundary)
       (let ((parsed (64tass--parse-line line)))
         (cond
          ((and parsed (not (equal :blank (plist-get parsed :type))))
           (cons linenum parsed))

          (t
           :continue)))))))

(defun 64tass-docblock--type-structure (parsed-line)
  "Return a docblock structure suitable for PARSED-LINE."
  (pcase (plist-get parsed-line :type)
    (:label
     (list :sections (list :function-header
                           :description
                           :arguments-header
                           :arguments
                           :returns-header
                           :returns)
           :focus :description))))

(defun 64tass-docblock--make-section-content (type parsed-line)
  "Generate the initial content for a docblock section.

Uses the section TYPE to determine content and potentially extract
information from PARSED-LINE."
  (list :type type
        :content
        (pcase type
          (:function-header
           (let ((label (-> parsed-line (plist-get :label-standalone) (plist-get :value))))
             (list
              (replace-regexp-in-string "_" " " (upcase label))
              label)))
          (:description '(""))
          (:arguments-header '("Arguments:"))
          (:returns-header '("Returns:"))
          (:arguments '(""))
          (:returns '("")))))

(defun 64tass-docblock--wrap-content-line (content)
  "Wrap raw docblock CONTENT in content line syntax."
  (format "; | %s |" content))

(defun 64tass-docblock--insert-contextual ()
  "Generate and insert a docblock for the nearest construct at point."
  (interactive)
  (let ((64tass--inhibit-formatting t))
    (let* ((construct (64tass-docblock--next-docable))
           (construct-line (cdr construct))
           (structure (when construct
                        (64tass-docblock--type-structure construct-line))))
      (cond
       ((or (null construct) (null structure))
        (message "Nothing to add a documentation banner to at position"))

       (t
        (let ((section-contents (mapcar
                                 (lambda (section-type)
                                   (64tass-docblock--make-section-content
                                    section-type
                                    construct-line))
                                 (plist-get structure :sections))))
          (64tass--goto-line (car construct))
          (beginning-of-line)
          (let ((focus-line nil))
            (save-excursion
              (insert 64tass-docblock--banner-divider)
              (insert "\n")
              (mapc (lambda (section)
                      (when (equal (plist-get section :type)
                                   (plist-get structure :focus))
                        (setq focus-line (line-number-at-pos)))
                      (mapc
                       (lambda (line)
                         (insert (64tass-docblock--wrap-content-line line))
                         (insert "\n"))
                       (plist-get section :content))
                      (insert 64tass-docblock--banner-divider)
                      (insert "\n"))
                    section-contents))
            (64tass-docblock--format-block-at)
            (when focus-line
              (64tass--goto-line focus-line)
              (forward-char 4)))))))))


;; Docblock editing and navigation

(defun 64tass-docblock--section-at-line (&optional line block)
  "Find the docblock section at LINE.

Optionally pass a pre-parsed BLOCK to avoid excessive re-parsing.
If not provided, the block will be parsed from the block at point, if any.

This function considers a divider line to be part of its preceding content
block.

The divider line that opens a docblock is not considered to be part of any
section and nil for input where `line` corresponds to this line."
  (save-excursion
    (when line
      (64tass--goto-line line))
    (let* ((block (or block (64tass-docblock--parse-docblock)))
           (current-section nil))
      (when (and block (> (line-number-at-pos) (car (plist-get block :bounds))))
        (while (and (null current-section)
                    (>= (line-number-at-pos) (car (plist-get block :bounds))))
          (when-let ((section (cl-find-if
                               (lambda (section)
                                 (and
                                  (>= (line-number-at-pos) (car (plist-get section :bounds)))
                                  (<= (line-number-at-pos) (cdr (plist-get section :bounds)))))
                               (plist-get block :sections))))
            (setq current-section section))
          (forward-line -1)))
      current-section)))

(defun 64tass-docblock--goto-next-section (&optional dir)
  "Move cursor to the beginning of the next content section.

A negative or positive value for DIR can be provided to move backwards or
in larger steps.  A nil value is treated as 1.

If no next content section exists, the cursor moves to the beginning
of the following line."
  (let* ((block (64tass-docblock--parse-docblock))
         (current-section (64tass-docblock--section-at-line nil block))
         (next-section (if (equal (line-number-at-pos)
                                  (car (plist-get block :bounds)))
                           (car (plist-get block :sections))
                         (64tass-docblock--next-section
                          block
                          (plist-get current-section :type)
                          (or dir 1)))))
    (if next-section
        (progn
          (64tass--goto-line (car (plist-get next-section :bounds)))
          (forward-char 4))
      (forward-line 1))))

(defun 64tass-docblock--replace-content (line content)
  "Replace the docblock content of LINE with CONTENT."
  (save-excursion
    (when line
      (64tass--goto-line line))
    (64tass--clear-line)
    (insert (64tass-docblock--wrap-content-line content))))

(defun 64tass-docblock--insert-content (line content position)
  "Insert docblock CONTENT relative to LINE, as determined by POSITION.

Valid values for POSITION are:
  :after-line"
  (save-excursion
    (when line
      (64tass--goto-line line))
    (let ((content-line (64tass-docblock--wrap-content-line content)))
      (pcase position
        (:after-line
         (progn (end-of-line) (newline)))
        (_
         (progn (end-of-line) (newline))))
      (insert content-line))))

(defun 64tass-docblock--tab ()
  "Contextually handle tab command on docblock lines."
  (cond
   ((and (64tass-docblock--is-content-line) (< (current-column) 4))
    (move-to-column 4))

   (t
    (64tass-docblock--goto-next-section))))

(defun 64tass-docblock--backtab ()
  "Contextually handle backtab command on docblock lines."
  (64tass-docblock--goto-next-section -1))

(defun 64tass-docblock--newline ()
  "Contextually handle newline command on docblock lines.

When point is on docblock line containing doc content, a new content
line is created within the same content section, pushing any content
on the right hand side of point to that new line.

When point is on a divider line, point moves to content start of
the following section.

When point is at the beginning of the line of the first divider line,
the entire docblock is pushed down one line (regular newline behavior)."
  (cond
   ((64tass-docblock--is-content-line)
    (let* ((line-content (64tass-docblock--line-content nil t))
           (content (plist-get line-content :content))
           (bounds (plist-get line-content :bounds))
           (content-pos (max 0 (- (current-column) (1+ (car bounds))))))
      (cond
       ((length> content content-pos)
        (progn
          (let ((64tass--inhibit-formatting t))
            (64tass-docblock--replace-content nil (substring content 0 content-pos))
            (64tass-docblock--insert-content nil (substring content content-pos) :after-line))
          (forward-line 1)
          (move-to-column 4)
          (64tass-docblock--format-block-at)))
       (t
        (progn
          (64tass-docblock--insert-content nil "" :after-line)
          (forward-line 1)
          (move-to-column 4))))))
   ((and (= 0 (current-column))
         (64tass-docblock--is-divider-line)
         (= (car (64tass-docblock--block-bounds))
            (line-number-at-pos)))
    (newline))
   ((64tass-docblock--is-divider-line)
    (64tass-docblock--goto-next-section))))




(provide '64tass-docblock)

;;; 64tass-docblock.el ends here
