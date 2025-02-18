;;; org-harvest.el --- Set of utilities for Harvest & org-mode interop -*- lexical-binding: t -*-

;; Copyright (C) 2025  Vitor Leal

;; Author: Vitor Leal <hellofromvitor@gmail.com>
;; URL: https://github.com/nvimtor/org-harvest.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary: TODO
;;
;; The git-commit-jira package automatically inserts JIRA ticket identifiers
;; into git commit messages based on the current branch name.
;;

;;; Code:

(require 'cl-lib)

;; NOTE this is required, see https://github.com/minad/consult/issues/1149
(setq consult-async-min-input 0)

;; NOTE will affect other packages!
;; (setq consult-async-split-style nil)

(defvar org-harvest--projects-cache '() "Recently fetched projects from Harvest.")

(defvar org-harvest-cmd
  (list shell-file-name shell-command-switch "./result/bin/org-harvest"))

(defvar org-harvest--process-name
  "org-harvest-get-tasks")

(defvar org-harvest--output-buffer-name "*org-harvest-output*")

(defun org-harvest--get-output-buffer ()
  (cl-letf*
      (((default-value 'process-environment) process-environment)
       ((default-value 'exec-path) exec-path))
    (let ((buf (generate-new-buffer org-harvest--output-buffer-name)))
      (with-current-buffer buf
        (setq-local process-environment (default-value 'process-environment))
        (setq-local exec-path (default-value 'exec-path)))
      buf)))

(defun org-harvest--group-fn (cand transform)
  "Group" (message "%s %s" cand transform) "all")

(defun org-harvest--sentinel (proc event cb)
  (when (eq 0 (process-exit-status proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-min))
      (let ((tasks (split-string (buffer-string) "\n" t)))
        (when (and cb (functionp cb))
          (funcall cb tasks)))
      (kill-buffer (process-buffer proc)))))

(defun org-harvest--get-tasks-async (cb)
  "Returns all projects from Harvest. Format returned:
clientid,projectid,taskid"
  (let ((cmd (or org-harvest-cmd "python3 main.py"))
         (process-name org-harvest--process-name)
         (process-buffer (org-harvest--get-output-buffer)))
    (when (get-process process-name)
      (kill-process process-name))
    (make-process
     :name process-name
     :buffer process-buffer
     :command cmd
     :noquery t
     :sentinel (lambda (proc event) (org-harvest--sentinel proc event cb)))))

(defun org-harvest--get-tasks-sync ()
  "Return tasks from Harvest as a list of strings, synchronously."
  (let ((buf (org-harvest--get-output-buffer)))
    (unwind-protect
        (let ((exit-code
               (apply #'call-process
                      (car org-harvest-cmd)  ; e.g. /bin/sh
                      nil                    ; no input file
                      buf                    ; capture output in buf
                      nil                    ; don't redisplay
                      (cdr org-harvest-cmd) ; the rest: (-c ./result/bin/org-harvest)
                      )))
          (unless (eq exit-code 0)
            (error "org-harvest-cmd failed with exit code %s" exit-code))
          (with-current-buffer buf
            (split-string (buffer-string) "\n" t)))
      (kill-buffer buf))))

(defun org-harvest--action (action cand)
  (message "elected: %s with action: %s" cand action))

(defun org-harvest-read-projects ()
  (interactive)
  (consult--read
   (consult--dynamic-collection
    (lambda (input cb)
      (ignore input)
      (let ((tasks (org-harvest--get-tasks-sync)))
        (funcall cb tasks))))
   :state 'org-harvest--action
   :group 'org-harvest--group-fn))

;;; scratch.el ends here
