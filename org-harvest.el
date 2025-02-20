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

;;;;;;;;;;;;;;
;; requires ;;
;;;;;;;;;;;;;;
(require 'cl-lib)
(require 'org-ql)
(require 'rx)


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

(defcustom org-harvest-state-dir
  ;; (expand-file-name "org-harvest-state.csv" temporary-file-directory)
  temporary-file-directory
  "Path to the file where org-harvest stores temporary state data."
  :type 'directory
  :group 'org-harvest)

;;;;;;;;;;;;
;; consts ;;
;;;;;;;;;;;;
(defconst org-harvest--org-clock-export/clock-re
  (rx (seq bol
     (zero-or-more (any "	 "))
     "CLOCK: ")
      (seq "["
     (group-n 1 (= 4 digit)) ;; start year
     "-"
     (group-n 2 (= 2 digit)) ;; start month
     "-"
     (group-n 3 (= 2 digit)) ;; start day
     (one-or-more space)
     (group-n 4 (= 3 alpha)) ;; start DOW
     (one-or-more space)
     (group-n 5 (= 2 digit)) ;; start hour
     ":"
     (group-n 6 (= 2 digit)) ;; start minute
     "]")
      "--"
      (seq "["
     (group-n 7 (= 4 digit)) ;; end year
     "-"
     (group-n 8 (= 2 digit)) ;; end month
     "-"
     (group-n 9 (= 2 digit)) ;; end day
     (one-or-more space)
     (group-n 10 (= 3 alpha)) ;; end DOW
     (one-or-more space)
     (group-n 11 (= 2 digit)) ;; end hour
     ":"
     (group-n 12 (= 2 digit)) ;; end minute
     "]")
      (seq (one-or-more space)
     "=>"
     (one-or-more space))
      (seq (group-n 15 ;; total time (hh:mm format)
        (group-n 13 (one-or-more digit)) ;; total hours
        ":"
        (group-n 14 (one-or-more digit))))) ;; total minutes
  "Clock line RE.  The groups are explained in the comments.")

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
        (property "HARVEST_TASK_ID")
        (or
         (property "HARVEST_UNPUSHED_ID")
         (property "HARVEST_TIMESHEET_ID"))))

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

(defvar org-harvest--prev-state-file nil)

(defun org-harvest--push (state-file)
  (let* ((out (org-harvest--run-command
               "push_tasks"
               "--from"
               state-file)))
    (when out
      (let* ((pairs (mapcar (lambda (s) (split-string s ",")) out))
             (ht (make-hash-table :test 'equal)))
        (dolist (pair pairs)
          (let ((unpushed-id (car pair))
                (timesheet-id (cadr pair)))
            (puthash unpushed-id timesheet-id ht)))
        (org-ql-select org-harvest-files
          `(or ,@(mapcar (lambda (unpushed-id)
                           `(property "HARVEST_UNPUSHED_ID" ,unpushed-id))
                         (hash-table-keys ht)))
          :action (lambda ()
                    (let* ((uid (org-entry-get nil "HARVEST_UNPUSHED_ID"))
                           (tid (gethash uid ht)))
                      (when tid
                        (org-entry-delete nil "HARVEST_UNPUSHED_ID")
                        (org-entry-put nil "HARVEST_TIMESHEET_ID" tid)))))
        nil))))

(defun org-harvest--get-pushed-ids-from-export-file (path)
  (let ((out '())
        (content (with-temp-buffer
                   (insert-file-contents path)
                   (buffer-string))))
    (dolist (line (cdr (split-string content "\n")))
      (let* ((fields (split-string line ","))
             (timesheetid (nth 1 fields)))
        (when (and timesheetid (not (string= timesheetid "null")))
          (push timesheetid out))))
    out))

(defun org-harvest--get-deletes (prev-path new-path)
  (when (and
         (file-exists-p prev-path)
         (file-exists-p new-path))
    (let* ((old-ids (org-harvest--get-pushed-ids-from-export-file prev-path))
           (new-ids (org-harvest--get-pushed-ids-from-export-file new-path))
           (diff (cl-set-difference
                  old-ids
                  new-ids
                  :test 'equal)))
      diff)))

(defun org-harvest--clean-deletes (deletes)
  (when deletes
    (org-harvest--run-command
     "delete_timesheets"
     "--ids"
     (string-join deletes ",")
     )))

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
(defun org-harvest-push ()
  "TODO docstring"
  (interactive)
  (let* ((new-state-file (expand-file-name
                          (format "%s.csv" (org-harvest--xah/get-random-uuid))
                          org-harvest-state-dir))
         (org-clock-export-org-ql-query org-harvest--org-clock-export-query)
         (org-clock-export-files org-harvest-files)
         (org-clock-export-export-file-name new-state-file)
         (org-clock-export-buffer org-harvest--org-clock-export-buffer-name)
         (org-clock-export-delimiter ",")
         (org-clock-export-data-format org-harvest--org-clock-export-data-format))
    (org-clock-export)
    (org-harvest--push new-state-file)
    (org-clock-export)
    (let ((deletes (org-harvest--get-deletes
                    org-harvest--prev-state-file
                    new-state-file)))
      (org-harvest--clean-deletes deletes)
      (setq org-harvest--prev-state-file new-state-file))))

;;; org-harvest.el ends here
