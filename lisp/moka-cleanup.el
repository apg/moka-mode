;;; moka-cleanup.el --- moka related functionality for Java development

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

;; The following interactive functions are defined within this file:
;;
;; - moka-cleanup-add-import:       add an import statement for the identifier
;;                                  around or before point
;; - moka-cleanup-organize-imports: order and format the import statements in
;;                                  the current source code file
;; - moka-cleanup-close-project     close all buffers associated with current project.

;; Configuration:

;; To automatically organize the import statements after adding a new one, set
;; `moka-cleanup-organize-after-add-flag' to non-nil. To change how the import
;; statements are sorted, edit `moka-cleanup-import-order-list'. Both variables
;; can be set using customize. Type `M-x customize-group' and enter group name
;; "moka".
;;
;; To make moka-cleanup work as expected, you must also configure package
;; moka, see the configuration section in file "moka.el".
;;
;; The moka-cleanup package defines two key bindings in the `moka-mode-map':
;;

;;; TODO: These likely change
;; - C-c + is bound to `moka-cleanup-add-import'
;; - C-c = is bound to `moka-cleanup-organize-imports'

;;; Code:

(eval-when-compile (require 'cl))

;; ----------------------------------------------------------------------------
;; Customization:
;; ----------------------------------------------------------------------------

(defcustom moka-cleanup-import-order-list
  '("^java\\."
    "^javax\\."
    "-"
    "^org\\."
    "^net\\."
    "^com\\."
    "-")
  "*List of regexps that defines how to order import statements in a file.
Each element is a regexp that matches a group of import statements, or a
hyphen (-).

Import statements are sorted according to which group they belong to. Import
statements that belong to the same group are sorted alphabetically within the
group. To separate groups of import statements with a blank line, insert a
hyphen in the list."
  :type '(repeat regexp)
  :group 'moka)

(defcustom moka-clean-organize-after-add-flag 't
  "*Non-nil means organize imports automatically after adding a new import.
A nil value means that new imports will be added last in the list."
  :type 'boolean
  :group 'moka)

;; ----------------------------------------------------------------------------
;; Generic functions:
;; ----------------------------------------------------------------------------


;; ----------------------------------------------------------------------------
;; Private variables:
;; ----------------------------------------------------------------------------

(defconst moka-cleanup-version "0.3.0"
  "The current version of `moka-cleanup'.")

(defconst moka-cleanup-not-an-import "!NOT-AN-IMPORT!")

;; ----------------------------------------------------------------------------
;; Main functions:
;; ----------------------------------------------------------------------------

(defun moka-cleanup-add-import ()
  "Add an import statement for the identifier around or before point.

Find the definition of the identifier in the tags table files, check that it
is not already imported, and insert a new import statement after the last one
in the list."
  (interactive)

  ;; Find definition of identifier
  (let ((definition (moka-tags-find-tag)))

    (if (null definition)
        (message "Tag not found!")

      ;; Check that identifier is a class, interface, or constructor
      (if (or (equal (moka-tags-definition-type definition) "class")
              (equal (moka-tags-definition-type definition) "interface")
              (equal (moka-tags-definition-type definition) "enum")
              (equal (moka-tags-definition-type definition)
                     (moka-tags-definition-name definition)))

          ;; Insert import statement
          (moka-cleanup-add-import-statement definition)
        (message "Tag not a type!")))))

(defun moka-cleanup-organize-imports ()
  "Order and format the import statements in the current source code file.

Sort the import statements in the order they are listed in
`moka-cleanup-import-order-list', insert blank lines between the
statements as specified in `moka-cleanup-import-order-list'."
  (interactive)

  ;; Find the region that contains all import statements
  (let ((import-region (moka-cleanup-find-import-region)))

    (when import-region
      ;; Remove unused imports, and compute a new region
      (moka-cleanup-remove-unused-imports (car import-region) (cdr import-region))
      (setq import-region (moka-cleanup-find-import-region))

      ;; Remove all blank lines, and compute a new region
      (moka-cleanup-clean-ws-region (car import-region) (cdr import-region))
      (setq import-region (moka-cleanup-find-import-region))

      ;; Sort all lines in the region, according to moka-cleanup-import-order-list
      (moka-cleanup-sort-import-region (car import-region) (cdr import-region))

      ;; Insert new blank lines, according to moka-cleanup-import-order-list
      (moka-cleanup-insert-ws-region (car import-region) (cdr import-region)))))


(defun moka-cleanup-close-project ()
  "Closes all open buffers which are in the current mvn project, and remove the
associated state for this current project."
  (interactive)
  (let* ((buffers (buffer-list))
         (project-prefix (expand-file-name (moka-mvn-find-root)))
         (prefix-length (length project-prefix)))
    (while buffers
      (let* ((current (car buffers))
             (name (buffer-file-name current)))
        (if (and
             name
             (string-equal (substring name 0 (min prefix-length (length name)))
                           project-prefix))
            (kill-buffer current)
          (with-current-buffer current
            (when (and (equalp major-mode 'dired-mode)
                       (let ((name (dired-current-directory)))
                         (string-equal (substring name 0 (min prefix-length (length name)))
                                       project-prefix)))
              (kill-buffer current))))
        (setq buffers (cdr buffers))))))

;; ----------------------------------------------------------------------------
(defun moka-cleanup-remove-unused-imports (start end)
  "Replaces import lines with spaces to be cleaned up in the next stage of
import organization. This function searches for the class or static method
that was being imported outside of the import area."
  (interactive "r")
  (save-excursion
    (moka-message "Removing old imports within region (%d %d)" start end)
    (goto-char start)
    (let ((import (moka-cleanup-sort-import-startkeyfun)))
      (while (< (point) end)
        (unless (string-equal moka-cleanup-not-an-import import)
          (let ((item (car
                       (reverse (split-string import "[.;]" t))))
                (found nil))
            (when (and item (not (string-equal "*" item)))
              (save-excursion
                (goto-char end)
                (setq found (re-search-forward item (point-max) t)))
              (unless found
                (moka-message "Removing import for line %s. %s not found in file" import item)
                (let ((e (save-excursion (end-of-line) (point))))
                  (beginning-of-line)
                  (delete-char (- e (point)))
                  (insert-char ?  (- e (point))))))))
        (forward-line 1)
        (setq import (moka-cleanup-sort-import-startkeyfun))))))

(defun moka-cleanup-add-import-statement (definition)
  "Add an import statement for the identifier in DEFINITION.
Check that the class or interface is not already imported, move to after
the last existing import statement, and insert a new import statement."
  (save-excursion
    (moka-message "Definition=%s" definition)

    ;; Get existing imports
    (let ((import-list (moka-tags-find-imports))
          (class-import (concat (moka-tags-definition-package definition)
                                "."
                                (moka-tags-definition-class definition)))
          (star-import (concat (moka-tags-definition-package definition) ".*")))
      ;; (moka-message "Imports=%S" import-list)

      ;; Check that identifier is not already imported
      (if (or (member class-import import-list)
              (member star-import import-list))
          (message "Tag already imported!")

        ;; Find last import
        (moka-cleanup-find-last-import)

        ;; If there are no import statements, but a package statement, insert
        ;; an extra newline to make things look better
        (if (and (= (length import-list) 2)
                 (moka-tags-find-package))
            (insert "\n"))

        ;; Insert import statement
        (insert "import " class-import ";\n")

        ;; If there are no import statements, and no package statement,
        ;; insert an extra newline to make things look better
        (if (and (= (length import-list) 1)
                 (not (moka-tags-find-package)))
            (insert "\n"))

        ;; Organize import statements, including the new one
        (if moka-cleanup-organize-after-add-flag
            (moka-cleanup-organize-imports))

        (message "Added import %s" class-import)))))

(defun moka-cleanup-find-last-import ()
  "Move to the line after the last package or import statement.
Find the last package or import statement, and place the point
at the beginning of the next line."
  (let ((last-import-pos (point-min)))

    (goto-char (point-min))
    (while (re-search-forward "\\(package\\|import\\)[ \t]+[^\n;]*;[^\n]*[\n]" nil t)
      (goto-char (match-beginning 0))
      (if (not (c-in-literal))
          (setq last-import-pos (match-end 0)))
      (goto-char (match-end 0)))
    (goto-char last-import-pos)))

(defun moka-cleanup-find-import-region ()
  "Find and return the region containing all import statements.
The value returned is a cons of the region's start and end.
If there are no import statements in this file, return nil."
  (save-excursion
    (let (start end)

      (goto-char (point-min))
      (while (re-search-forward "import[ \t]+[^\n;]*;[^\n]*[\n]" nil t)

        ;; Set start to the beginning of the previous line,
        ;; but ignore matches in comments and strings
        (goto-char (match-beginning 0))
        (when (not (c-in-literal))
          (if (null start)
              (setq start (match-beginning 0)))
          (setq end (match-end 0)))
        (goto-char (match-end 0)))

      ;; If we found any imports
      (if start
          (cons start end)))))

(defun moka-cleanup-clean-ws-region (start end)
  "Clean unwanted whitespace in the region between START and END.
Remove blank lines and any whitespace at BOL for all lines in
the given region."
  (save-excursion
    (goto-char end)

    (while (> (point) start)
      (forward-line -1)

      ;; Remove blank line
      (when (looking-at "[ \t]*$")
        (delete-region (match-beginning 0) (1+ (match-end 0))))

      ;; Remove whitespace at bol
      (when (looking-at "[ \t]+")
        (delete-region (match-beginning 0) (match-end 0))))))

(defun moka-cleanup-insert-ws-region (start end)
  "Insert blank lines between import statements belonging to different groups.
The list `moka-cleanup-import-order-list' defines where to insert the blank
lines. Only import statements within the region defined by START and END are
considered."
  (save-excursion
    (goto-char end)
    (forward-line -1)

    (while (> (point) start)

      ;; Get import statements from the previous line and from this line
      (let ((import1 (save-excursion (forward-line -1) (moka-cleanup-sort-import-startkeyfun)))
            (import2 (save-excursion (moka-cleanup-sort-import-startkeyfun))))

        ;; Get indices for the imports
        (let ((index1 (moka-match-index moka-cleanup-import-order-list import1))
              (index2 (moka-match-index moka-cleanup-import-order-list import2)))

          ;; If import was not in list, set index to length of list to make it
          ;; easier to compare
          (if (null index1)
              (setq index1 (length moka-cleanup-import-order-list)))
          (if (null index2)
              (setq index2 (length moka-cleanup-import-order-list)))

          ;; Get index of first hyphen after first import
          (let ((index-hyphen (moka-match-index (nthcdr index1 moka-cleanup-import-order-list) "-")))
            (if (null index-hyphen)
                (setq index-hyphen (length moka-cleanup-import-order-list)))

            ;; If the imports are not in the same group in the list,
            ;; and there is a hyphen between them in the list,
            ;; insert a blank line between them in the source code
            (when (and (not (equal index1 index2))
                       (< index-hyphen (- index2 index1)))
              (insert "\n")
              (forward-line -1)))))

      (forward-line -1))))

(defun moka-cleanup-sort-import-region (start end)
  "Sort import statements according to `moka-cleanup-import-order-list'.
Only import statements within the region defined by START and END are
considered."
  (save-excursion
    (save-restriction
;;       (moka-message "Import order list=%S" moka-cleanup-import-order-list)
      (narrow-to-region start end)
      (goto-char (point-min))
      (if (and (not (featurep 'xemacs)) (< emacs-major-version 22))
          (sort-subr nil
                     'forward-line
                     'end-of-line
                     'moka-cleanup-sort-import-startkeyfun
                     nil)
        (sort-subr nil
                   'forward-line
                   'end-of-line
                   'moka-cleanup-sort-import-startkeyfun
                   nil
                   'moka-cleanup-sort-import-predicate)))))

(defun moka-cleanup-sort-import-startkeyfun ()
  "Return the import statement sort key, i.e. the package name.
Return a dummy string if this line is not an import statement."
  (if (re-search-forward "import[ \t]+\\([^\n;]*;\\)" (point-at-eol) t)
      (buffer-substring-no-properties (match-beginning 1) (match-end 1))
    moka-cleanup-not-an-import))

(defun moka-cleanup-sort-import-predicate (import1 import2)
  "Return t if IMPORT1 should be sorted before IMPORT2.
Import statements are sorted according to which group they belong to. The
groups are defined in `moka-cleanup-import-order-list'. Import statements
that belong to the same group are sorted alphabetically within the group."
;;   (moka-message "Comparing imports `%s' and `%s'" import1 import2)
  (let ((index1 (moka-match-index moka-cleanup-import-order-list import1))
        (index2 (moka-match-index moka-cleanup-import-order-list import2)))

    ;; If import was not in list, set index to length of list to make it easier
    ;; to compare
    (if (null index1)
        (setq index1 (length moka-cleanup-import-order-list)))
    (if (null index2)
        (setq index2 (length moka-cleanup-import-order-list)))

    ;; If the indices are equal, compare the imports alphabetically, since they
    ;; are both in the same group
    (if (= index1 index2)
        (string< import1 import2)
      (< index1 index2))))


(provide 'moka-cleanup)

;;; moka-cleanup.el ends here
