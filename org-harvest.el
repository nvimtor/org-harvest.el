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
;; (setq consult-async-min-input 0)
;; NOTE will affect other packages!
;; (setq consult-async-split-style nil)

;;;;;;;;;;;;
;; custom ;;
;;;;;;;;;;;;

(defgroup org-harvest nil
  "Customization group for the org-harvest.el package."
  :group 'tools
  :prefix "org-harvest-")

(defcustom org-harvest-cmd
  "~/Projects/transcriber/result/bin/org-harvest"
  "Absolute path to the org-harvest executable."
  :type 'string
  :group 'org-harvest)

(defcustom org-harvest-files org-agenda-files
  "Org files that will be used to query for org-harvest logbooks."
  :type 'list
  :group 'org-harvest)

(defcustom org-harvest-state-file
  (expand-file-name "org-harvest-state.csv" temporary-file-directory)
  "Path to the file where org-harvest stores temporary state data."
  :type 'file
  :group 'org-harvest)

;;;;;;;;;;;;;;
;; internal ;;
;;;;;;;;;;;;;;

(defvar org-harvest--projects-cache '() "Recently fetched projects from Harvest.")

(defvar org-harvest--process-name
  "org-harvest-get-tasks")

(defvar org-harvest--output-buffer-name "*org-harvest-output*")

;; NOTE from xah, modified to return instead of insert
(defun org-harvest--xah/get-random-uuid ()
  "Return a UUID string.
This function calls `uuidgen` on MacOS and GNU/Linux,
and calls PowerShell on Microsoft Windows.
URL: http://xahlee.info/emacs/emacs/elisp_generate_uuid.html
Version: 2020-06-04 2023-05-13"
  (cond
   ((eq system-type 'windows-nt)
    (string-trim (shell-command-to-string "pwsh.exe -Command [guid]::NewGuid().toString()")))
   ((or (eq system-type 'darwin) (eq system-type 'gnu/linux))
    (string-trim (shell-command-to-string "uuidgen")))
   (t
    (let* ((xstr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                              (user-uid)
                              (emacs-pid)
                              (system-name)
                              (user-full-name)
                              (current-time)
                              (emacs-uptime)
                              (garbage-collect)
                              (buffer-string)
                              (random)
                              (recent-keys))))
           (uuid (format "%s-%s-4%s-%s%s-%s"
                         (substring xstr 0 8)
                         (substring xstr 8 12)
                         (substring xstr 13 16)
                         (format "%x" (+ 8 (random 4)))
                         (substring xstr 17 20)
                         (substring xstr 20 32))))
      uuid))))

(defun org-harvest--parse-line (line)
  "Parse a CSV line into a list of fields.
Assumes every field is enclosed in double quotes, fields are separated by commas,
and any double quotes inside a field are escaped as \"\"."
  (let ((fields '()))
    (while (string-match "^\"\\(\\(?:[^\"]\\|\"\"\\)*\\)\"\\(,\\|$\\)" line)
      (let ((field (match-string 1 line)))
        (setq field (replace-regexp-in-string "\"\"" "\"" field))
        (push field fields)
        (setq line (substring line (match-end 0)))))
    (nreverse fields)))

(defun org-harvest--propertize-line (line)
  "Parse LINE into fields, then attach them as text properties on the returned string.
Returns the same LINE (string) with text properties storing each field:
org-harvest-0, org-harvest-1, ..., org-harvest-4."
  (let* ((fields (org-harvest--parse-line line)))
    (add-text-properties
     0 (length line)
     (list 'org-harvest-0 (nth 0 fields)
           'org-harvest-1 (nth 1 fields)
           'org-harvest-2 (nth 2 fields)
           'org-harvest-3 (nth 3 fields)
           'org-harvest-4 (nth 4 fields))
     line)
    line))

(defun org-harvest--run-command (subcommand &rest args)
  "Run org-harvest SUBCOMMAND with ARGS and return output lines."
  (let ((output (apply #'process-lines
                       org-harvest-cmd
                       (append (list subcommand) args))))
    output))

(defun org-harvest--get-tasks-sync ()
  "Return tasks from Harvest as a list of strings, synchronously."
  (org-harvest--run-command "get_tasks"))

(defun org-harvest--tasks-group (cand transform)
  "Function for Consult to group or transform a candidate string with text properties."
  (let* ((clientname (get-text-property 0 'org-harvest-0 cand))
         (projname   (get-text-property 0 'org-harvest-2 cand))
         (taskname   (get-text-property 0 'org-harvest-4 cand)))
    (if (not transform)
        (format "%s (%s)" clientname projname)
      taskname)))

;; NOTE 'return action doesn't preserve cand's text properties https://github.com/minad/consult/issues/582
(defun org-harvest--tasks-state (action cand)
  (pcase action
    ('return
     (let* ((projid (get-text-property 0 'org-harvest-1 cand))
            (taskid (get-text-property 0 'org-harvest-3 cand)))
       (when (not (or
                   (org-entry-get nil "HARVEST_UNPUSHED_ID")
                   (org-entry-get nil "HARVEST_TIMESHEET_ID")))
         (org-entry-put
          nil
          "HARVEST_UNPUSHED_ID"
          (org-harvest--xah/get-random-uuid)))
       (org-entry-put nil "HARVEST_PROJECT_ID" projid)
       (org-entry-put nil "HARVEST_TASK_ID" taskid)))))

(defun org-harvest--tasks-lookup (selected cands &rest _)
  (car (member selected cands)))

(defvar org-harvest--org-clock-export-buffer-name
  "*ORG-HARVEST-ORG-CLOCK-EXPORT CSV*")

(defvar org-harvest--org-clock-export-query
  '(and (property "HARVEST_PROJECT_ID")
        (property "HARVEST_TASK_ID")))

;; (defvar org-harvest--org-clock-export-query
;;   '(and (property "HARVEST_PROJECT_ID")
;;         (property "HARVEST_TASK_ID")
;;         (or
;;          (property "HARVEST_UNPUSHED_ID")
;;          (property "HARVEST_TIMESHEET_ID"))))

(defvar org-harvest--org-clock-export-data-format
  '("unpushedid" (or (org-entry-get (point) "HARVEST_UNPUSHED_ID") "null")
    "timesheetid" (or (org-entry-get (point) "HARVEST_TIMESHEET_ID") "null")
    "projid" (org-entry-get (point) "HARVEST_PROJECT_ID")
    "taskid" (org-entry-get (point) "HARVEST_TASK_ID")
    "spent_date" (format "%04d-%02d-%02d"
                         (string-to-number start-year)
                         (string-to-number start-month)
                         (string-to-number start-day))
    "hours" (format "%.2f" (+ (string-to-number total-hours)
                              (/ (string-to-number total-minutes) 60.0)))))

(defun org-harvest--push ()
  (org-harvest--run-command "push_tasks" "--from" org-harvest-state-file))
;;;;;;;;;;;;
;; public ;;
;;;;;;;;;;;;

;;;###autoload
(defun org-harvest-tasks ()
  (interactive)
  (consult--read
   (consult--dynamic-collection
       (lambda (input cb)
         (ignore input)

         (funcall cb org-harvest--projects-cache)

         (let* ((tasks (org-harvest--get-tasks-sync))
                (propertized (mapcar #'org-harvest--propertize-line tasks)))
           (setq org-harvest--projects-cache propertized)
           (funcall cb propertized))))
   :state 'org-harvest--tasks-state
   :lookup 'org-harvest--tasks-lookup
   :group 'org-harvest--tasks-group))

;;;###autoload
(defun org-harvest-push (&optional query)
  "TODO docstring"
  (interactive)
  (let ((org-clock-export-org-ql-query org-harvest--org-clock-export-query)
        (org-clock-export-files org-harvest-files)
        (org-clock-export-export-file-name org-harvest-state-file)
        (org-clock-export-buffer org-harvest--org-clock-export-buffer-name)
        (org-clock-export-delimiter ",")
        (org-clock-export-data-format org-harvest--org-clock-export-data-format))
    (org-clock-export)))

;;; org-harvest.el ends here
