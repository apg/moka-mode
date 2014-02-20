;;; moka-yas.el --- helpers for combining moka and yasnippet

;; Copyright (C) 2014 Andrew Gwozdziewycz

;; Version: 0.1
;; Author: Andrew Gwozdziewycz <git@apgwoz.com>
;; Created: Feb 2014
;; Keywords: languages, tools, compilation, maven
;; URL: https://github.com/apg/moka-mode

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; This file is NOT part of GNU emacs. It is however part of moka-mode

;;; Commentary:

(defun moka-yas-down-first (str)
  "Downcases the first character in str. Useful for dealing with
camelCase"
  (if (and str (> (length str) 0))
      (let ((first (substring str 0 1))
            (rest (substring str 1)))
        (concat (downcase first) rest))
    ""))

(defun moka-yas-up-first (str)
  "Upcases the first character in str. Useful for dealing with
camelCase"
  (if (and str (> (length str) 0))
      (let ((first (substring str 0 1))
            (rest (substring str 1)))
        (concat (upcase first) rest))
    ""))

(defun moka-yas-class-name ()
  (file-name-nondirectory
   (file-name-sans-extension
    (or (buffer-file-name) (buffer-name (current-buffer))))))

(defun moka-yas-package-name ()
  "Finds the likely package in `.` notation.

NOTE: This is likely better off in moka-lib"
  (let* ((dir-name (file-name-directory (expand-file-name (buffer-file-name))))
         (dir-name-len (length dir-name))
         (root (expand-file-name (moka-mvn-find-root)))
         (root-len (length root)))
    (if (and (> dir-name-len root-len)
             (string-equal (substring dir-name 0 root-len) root))
        (let ((rest-dir-name (substring dir-name root-len (- dir-name-len 1))))
          (if (= (string-match "src/\\(test\\|main\\)/java/" rest-dir-name) 0)
              (replace-regexp-in-string "/" "." (substring rest-dir-name 14))
            "???")))))

(provide 'moka-yas)
