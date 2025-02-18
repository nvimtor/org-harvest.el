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

(defcustom org-harvest-cmd "./result/bin/org-harvest"
  "Path to the org-harvest executable."
  :type 'string
  :group 'org-harvest)

;;;;;;;;;;;;;;
;; internal ;;
;;;;;;;;;;;;;;

(defvar org-harvest--projects-cache '() "Recently fetched projects from Harvest.")

(defvar org-harvest--process-name
  "org-harvest-get-tasks")

(defvar org-harvest--output-buffer-name "*org-harvest-output*")

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

(defun org-harvest--build-command (subcommand)
  "Build the full command string by appending SUBCOMMAND to `org-harvest-cmd`."
  (format "%s %s" org-harvest-cmd subcommand))

(defun org-harvest--run-command (command)
  "Execute the shell COMMAND synchronously.
Return its output as a list of non-empty lines. Signal an error if the command fails."
  (message "Executing command: %s" command)
  ;; `process-lines` calls the shell and returns a list of lines.
  (process-lines shell-file-name shell-command-switch command))

(defun org-harvest--get-tasks-sync ()
  "Return tasks from Harvest as a list of strings, synchronously."
  (org-harvest--run-command (org-harvest--build-command "get_tasks")))

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
       (org-entry-put nil "HARVEST_PROJECT_ID" projid)
       (org-entry-put nil "HARVEST_TASK_ID" taskid)))))

(defun org-harvest--tasks-lookup (selected cands &rest _)
  (car (member selected cands)))

(defvar org-harvest--org-clock-export-buffer-name
  "*ORG-HARVEST-ORG-CLOCK-EXPORT CSV*")

(defvar org-harvest--org-clock-export-query
  '(and (property "HARVEST_PROJECT_ID")
        (property "HARVEST_TASK_ID")))

(defvar org-harvest--org-clock-export-data-format
  '("projid" (org-entry-get (point) "HARVEST_PROJECT_ID")
    "taskid" (org-entry-get (point) "HARVEST_TASK_ID")
    "spent_date" (format "%04d-%02d-%02d"
                         (string-to-number start-year)
                         (string-to-number start-month)
                         (string-to-number start-day))
    "hours" (format "%.2f" (+ (string-to-number total-hours)
                              (/ (string-to-number total-minutes) 60.0)))))

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
        (org-clock-export-files "~/org/zenobe/dailies.org")
        (org-clock-export-export-file-name "~/org/zenobe/export.csv")
        (org-clock-export-buffer org-harvest--org-clock-export-buffer-name)
        (org-clock-export-delimiter ",")
        (org-clock-export-data-format org-harvest--org-clock-export-data-format))
    (org-clock-export)))

;;; org-harvest.el ends here
