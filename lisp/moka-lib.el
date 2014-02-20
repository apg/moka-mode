;;; moke-cleanup.el --- moka related functionality for Java development

;; Copyright (C) 2007-2011 Johan Dykstrom
;; Copyright (C) 2013 Andrew Gwozdziewycz

;; Author: Johan Dykstrom <jody4711-sf@yahoo.se>
;; Created: Oct 2007
;; Version: 0.3.0
;; Keywords: languages, tools
;; Original URL: http://jtags.sourceforge.net
;; URL: https://github.com/apg/moka-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is NOT part of GNU emacs. It is however part of moka-mode

;;; Commentary:

;; This file defines utility functions which the rest of the mode relies
;; upon.

(defun moka-uniqify-list (list)
  "Return a copy of LIST with all duplicates removed.
The original list is not modified. Example:

\(moka-uniqify-list '\(1 3 1 5 3 1\)\) -> \(1 3 5\)"
  (let (unique)
    (while list
      (unless (member (car list) unique)
        (setq unique (cons (car list) unique)))
      (setq list (cdr list)))
    (nreverse unique)))

(defun moka-filter-list (list predicate)
  "Return a list containing all elements in LIST that satisfy PREDICATE.
The original LIST is not modified. PREDICATE should be a function of one
argument that returns non-nil if the argument should be part of the result
list. Example:

\(moka-filter-list '\(1 2 3 4 5\) \(lambda \(x\) \(> x 3\)\)\) -> \(4 5\)"
  (let (result)
    (while list
      (if (funcall predicate (car list))
          (setq result (cons (car list) result)))
      (setq list (cdr list)))
    (nreverse result)))

(defsubst moka-rotate-left (src)
  "Rotate the list SRC one position left, and return the result.
The original list is not modified."
  (if src
      (append (cdr src) (list (car src)))))

(defsubst moka-get-line ()
  "Return the line number of the current buffer position."
  (save-excursion
    (beginning-of-line)
    (1+ (count-lines 1 (point)))))

(defsubst moka-line-to-point (line)
  "Convert LINE to a position in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defun moka-file-name-directory (filename)
  "Return the directory component in file name FILENAME.
Unlike the built-in function `file-name-directory', this function also
normalizes the file name. File name components such as `..' and `~' are
expanded. Back slashes are replaced with forward slashes. The returned
string always ends with a slash."
  (setq filename (file-truename filename))
  (if (string-match "^[A-Za-z]:" filename)
      (setq filename (concat (downcase (substring filename 0 1))
                             (substring filename 1))))
  (if (not (file-directory-p filename))
      (setq filename (file-name-directory filename)))
  (setq filename (file-name-as-directory filename))
  (setq filename (replace-regexp-in-string "\\\\" "/" filename)))


(defun moka-message (string &rest args)
  "Display message STRING at the bottom of the screen if tracing is ON.
The message also goes into the `*Messages*' buffer.

The first argument is a format control string, and the rest are data
to be formatted under control of the string. See `format' for details.

See `moka-trace-flag' on how to turn tracing ON and OFF."
  (when moka-trace-flag
    (save-excursion
      (save-match-data

        ;; Get name of calling function
        (let* ((frame-number 0)
               (function-list (backtrace-frame frame-number))
               (function-name nil))
          (while function-list
            (if (symbolp (cadr function-list))
                (setq function-name (symbol-name (cadr function-list)))
              (setq function-name "<not a symbol>"))
            (if (and (string-match "^moka-tags-" function-name)
                     (not (string-match "^moka-message$" function-name)))
                (setq function-list nil)
              (setq frame-number (1+ frame-number))
              (setq function-list (backtrace-frame frame-number))))

          ;; Update argument list
          (setq args (append (list (concat "%s:\t" string) function-name) args)))

        ;; Print message
        (apply 'message args)))))

(defun moka-match-index (list s)
  "Return index of first regexp in LIST that matches string S.
Return nil if no regexp in LIST matches S."
  (let ((index 0))
    (block while-list
      (while list
        (if (string-match (car list) s)
            (return-from while-list index))
        (setq list (cdr list))
        (setq index (1+ index))))))


(provide 'moka-lib)
