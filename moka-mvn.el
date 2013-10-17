;;; maven.el --- helpers for compiling with maven

;; Copyright (C) 2013 Andrew Gwozdziewycz <git@apgwoz.com>

;; Version: 0.1
;; Keywords: compilation, maven, java

;; This file is NOT part of GNU Emacs

;; This file IS part of Moka-mode

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

(defvar moka-mvn-last-task "compile")
(defvar moka-mvn-build-file-name "pom.xml")
(defvar moka-mvn-command "mvn")

(defvar moka-mvn-tasks-default '("compile" "test" "clean"))

(defun moka-mvn-find-tasks (directory)
  (let ((output (shell-command-to-string (concat *moka-mvn-tasks-command* " "
                                                 directory "/"
                                                 moka-mvn-build-file-name))))
    (message output)
    (if (> (length output) 0)
        (mapcar '(lambda (x) (replace-regexp-in-string ".*<target.*name=\"\\([^\-][^\"]*\\).*" "\\1" x)) 
                (split-string output "[\n]"))
      nil)))

;; should cache tasks from the build file at some point
(defun moka-mvn-tasks (directory)
  (let ((tasks (assoc-string directory *moka-mvn-tasks-cache*)))
    (cdr 
     (or tasks
         (progn 
           (let ((newtasks (or (moka-mvn-find-tasks directory) moka-mvn-tasks-default)))
             (setq *moka-mvn-tasks-cache*
                   (cons (cons directory newtasks) *moka-mvn-tasks-cache*))
             newtasks))))))

(defun moka-mvn-get-task (directory)
  (let ((task (completing-read-multiple (concat "Goal (default): ") 
                                        moka-mvn-tasks-default)))
    (if (> (length task) 0)
        (mapconcat 'identity task " ")
      "")))

(defun moka-mvn-find-root (indicator)
  (let ((cwd default-directory))
    (locate-dominating-file cwd moka-mvn-build-file-name)))

(defun moka-mvn-kill-cache ()
  (interactive)
  (setq *moka-mvn-tasks-cache* '()))

(defun mvn (&optional task args)
  "Run mvn `task` in project root directory."
  (interactive)
  (let ((default-directory (moka-mvn-find-root moka-mvn-build-file-name)))
    (if default-directory
        (let ((task (or task (moka-mvn-get-task default-directory))))
          (setq moka-mvn-last-task task)
          (compile (concat moka-mvn-command " " task " " args)))
      (message "Couldn't find a maven project."))))

(defun moka-mvn-last ()
  "Run the last maven task in project"
  (interactive)
  (mvn (or moka-mvn-last-task "")))

(defun moka-mvn-compile ()
  (interactive)
  (mvn "compile"))

(defun moka-mvn-clean ()
  (interactive)
  (mvn "clean"))

(defun moka-mvn-test (prefix)
  (interactive "MTest: ")
  (if prefix
      (mvn "test" (concat "-Dtest=" prefix))
    (mvn "test")))

(provide 'moka-mvn)
