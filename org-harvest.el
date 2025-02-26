;;; org-harvest.el --- Set of utilities for Harvest & org-mode interop -*- lexical-binding: t -*-

;; Copyright (C) 2025  Vitor Leal

;; Author: Vitor Leal <hellofromvitor@proton.me>
;; URL: https://github.com/nvimtor/org-harvest.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (consult "2.0") (org-ql "0.8.10"))

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

;;; Commentary:
;;
;; The org-harvest.el package allows you to use org-mode to control your Harvest
;; timesheets.
;;

;;; Code:

;;;;;;;;;;;;;;
;; requires ;;
;;;;;;;;;;;;;;
(require 'cl-lib)
(require 'json)
(require 'url)
(require 'org-ql)
(require 'consult)

;;;;;;;;;;;;
;; custom ;;
;;;;;;;;;;;;
(defgroup org-harvest nil
  "Customization group for the org-harvest.el package."
  :group 'tools
  :prefix "org-harvest-")

(defcustom org-harvest-files org-agenda-files
  "Org files that will be used to query for org-harvest logbooks."
  :type 'list
  :group 'org-harvest)

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

(defconst org--harvest-project-assignments-api-url
  "https://api.harvestapp.com/v2/users/me/project_assignments"
  "Harvest API URL for user project assignments.")

(defconst org--harvest-time-entries-api-url
  "https://api.harvestapp.com/v2/time_entries"
  "Harvest API URL for time entries.")

(defvar org-harvest--projects-cache '()
  "Recently fetched projects from Harvest.")

(defun org-harvest--get-authinfo ()
  "Retrieve Harvest authentication info from .authinfo.
Returns a plist with :harvest-pat and :harvest-account-id.
Expects an entry with host \"harvestapp.com\" containing the token as the user
and the account ID as the secret."
  (let* ((entry (car (auth-source-search :host "harvestapp.com"
                                           :require '(:user :secret)
                                           :max 1)))
         (harvest-account-id (plist-get entry :user))
         (harvest-pat (if (functionp (plist-get entry :secret))
                                 (funcall (plist-get entry :secret))
                               (plist-get entry :secret))))
    (unless (and harvest-pat (not (string= harvest-pat "")))
      (error "HARVEST_PAT not found in .authinfo"))
    (unless (and harvest-account-id (not (string= harvest-account-id "")))
      (error "HARVEST_ACCOUNT_ID not found in .authinfo"))
    (list :harvest-pat harvest-pat :harvest-account-id harvest-account-id)))

(defun org-harvest--make-request-headers (authinfo)
  "Create HTTP headers for Harvest API requests using auth info from .authinfo."
  (let ((pat (plist-get authinfo :harvest-pat))
         (account-id (plist-get authinfo :harvest-account-id)))
    `(("Authorization" . ,(format "Bearer %s" pat))
      ("Harvest-Account-Id" . ,account-id))))

(defun org-harvest--get-proj-assignments (auth-headers)
  "Recursively fetch project assignments synchronously.
AUTH-HEADERS is an alist of HTTP headers.
Returns a complete list of assignments."
  (cl-labels ((go (url assignments)
                (if url
                    (let* ((resp (request
                                   url
                                   :headers auth-headers
                                   :parser 'json-read
                                   :sync t))
                           (data (request-response-data resp))
                           (next-page (cdr (assoc 'next_page data)))
                           (proj-assignments (cdr (assoc 'project_assignments data)))
                           (all-assignments (append assignments proj-assignments)))
                      (go next-page all-assignments))
                  assignments)))
    (go org--harvest-project-assignments-api-url nil)))

(defun org-harvest--delete-time-entry (id
                                     projid
                                     taskid
                                     spentdate
                                     hours
                                     headers
                                     cb)
  ())


(defun org-harvest--patch-time-entry (id
                                      content
                                      headers
                                      onerrcb)
  (request
    (concat org--harvest-time-entries-api-url "/" id)
    :type "PATCH"
    :parser 'json-read
    :headers headers
    :data content
    :error (cl-function
            (lambda (&rest _ &key _ &allow-other-keys)
              (funcall onerrcb)))))

(defun org-harvest--post-time-entry (content
                                     headers
                                     cb)
  (request org--harvest-time-entries-api-url
    :type "POST"
    :parser 'json-read
    :headers headers
    :data content
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall cb (alist-get 'id data))))))

(defun org-harvest--tasks-get-cands (project-assignments)
  "Convert PROJECT-ASSIGNMENTS into a list of JSON-encoded strings.
Each string corresponds to a single task in a given project. The
JSON object contains the following fields:

  clientName, projectId, projectName, taskId, taskName

Example of one returned JSON candidate:
{
  \"clientName\": \"Lockheed Martin\",
  \"projectId\": 40603431,
  \"projectName\": \"F-117 Nighthawk\",
  \"taskId\": 11406204,
  \"taskName\": \"TaskName\"
}"
  (cl-loop
   for pa across project-assignments
   append
   (let* ((client        (alist-get 'client pa))
          (client-name   (alist-get 'name client))
          (project       (alist-get 'project pa))
          (project-id    (alist-get 'id project))
          (project-name  (alist-get 'name project))
          (task-assignments (alist-get 'task_assignments pa)))
     (cl-loop
      for ta across task-assignments
      collect
      (let* ((task      (alist-get 'task ta))
             (task-id   (alist-get 'id task))
             (task-name (alist-get 'name task))
             (obj `(("clientName"  . ,client-name)
                    ("projectId"   . ,project-id)
                    ("projectName" . ,project-name)
                    ("taskId"      . ,task-id)
                    ("taskName"    . ,task-name))))
        (json-encode obj))))))

(defun org-harvest--parse-json-cand (json-candidate)
  "Parse the JSON-CANDIDATE string back into an alist"
  (json-parse-string
   json-candidate
   :object-type 'alist
   :array-type 'list
   :null-object nil
   :false-object nil))

(defun org-harvest--tasks-group (cand transform)
  "Function for Consult to group or transform a candidate string with text properties."
  (let-alist (org-harvest--parse-json-cand cand)
    (if (not transform)
        (format "%s (%s)" .clientName .projectName)
      .taskName)))

(defun org-harvest--assign-task (projid taskid)
  (when (not (or
              (org-entry-get nil "HARVEST_UNPUSHED_ID")
              (org-entry-get nil "HARVEST_TIMESHEET_ID")))
    (org-entry-put
     nil
     "HARVEST_UNPUSHED_ID"
     (org-harvest--xah/get-random-uuid)))
  (org-entry-put nil "HARVEST_PROJECT_ID" projid)
  (org-entry-put nil "HARVEST_TASK_ID" taskid))

(defun org-harvest--tasks-state (action cand)
  (pcase action
  ('return
   (let-alist (org-harvest--parse-json-cand cand)
     (org-harvest--assign-task
      (number-to-string .projectId)
      (number-to-string .taskId))))))

;; NOTE sync portion
(defvar org-harvest--export-data-format
  '('unpushedid (org-entry-get (point) "HARVEST_UNPUSHED_ID")
    'timesheetid (org-entry-get (point) "HARVEST_TIMESHEET_ID")
    'projid (org-entry-get (point) "HARVEST_PROJECT_ID")
    'taskid (org-entry-get (point) "HARVEST_TASK_ID")
    'spent_date (format "%04d-%02d-%02d"
                         (string-to-number start-year)
                         (string-to-number start-month)
                         (string-to-number start-day))
    'hours (format "%.2f" (+ (string-to-number total-hours)
                              (/ (string-to-number total-minutes) 60.0)))))

(defmacro org-harvest--parse-clock-lines-in-heading (arg)
  `(save-excursion
     (cl-flet ((get-limit () (or (save-excursion
                                   (end-of-line)
                                   (re-search-forward org-heading-regexp nil t))
                                 (point-max))))
       (cl-loop while
                (re-search-forward org-harvest--org-clock-export/clock-re (get-limit) 'no-error)
                collect
                (cl-flet ((get-match (num) (org-no-properties (match-string num))))
                  (let ((start-year (get-match 1))
                        (start-month (get-match 2))
                        (start-day (get-match 3))
                        (total-hours (get-match 13))
                        (total-minutes (get-match 14)))
                    (list
                     ,@(cl-loop
                        for i from 0 below (length arg) by 2
                        collect `(cons ,(nth i arg)
                                       ,(nth (1+ i) arg))))))))))

(defvar org-harvest--sync-query
  '(and (clocked)
        (property "HARVEST_PROJECT_ID")
        (property "HARVEST_TASK_ID")
        (or
         (property "HARVEST_UNPUSHED_ID")
         (property "HARVEST_TIMESHEET_ID"))))

(defun org-harvest--sync-get-total-hours (logbooks)
  "Sums up the hours for each logbook in LOGBOOKS."
  (seq-reduce #'+
              (mapcar (lambda (logb)
                        (string-to-number (alist-get 'hours logb)))
                      logbooks)
              0))

(defmacro org-harvest--in-marker (marker &rest body)
  "Execute BODY in the buffer of MARKER with point at the marker's position."
  `(with-current-buffer (marker-buffer ,marker)
     (goto-char ,marker)
     ,@body))

(defun org-harvest--get-heading-for-notes ()
  (org-get-heading t t t nil))

(defun org-harvest--get-notes (&optional optmarker)
  (let ((marker (or optmarker (point-marker))))
    (org-harvest--in-marker
     marker
     (if-let* ((customnotes (org-entry-get nil "HARVEST_NOTES")))
         customnotes
       (org-harvest--get-heading-for-notes)))))

(defun org-harvest--sync-logbooks (logbooks headers marker)
  "TODO docstring. MARKER is where the heading is located."
  (dolist (entry logbooks)
    (let-alist entry
      (let* ((notes (org-harvest--get-notes marker))
             (content `(("project_id" . ,.projid)
                        ("task_id"    . ,.taskid)
                        ("spent_date" . ,.spent_date)
                        ("hours"      . ,.hours)
                        ("notes"      . ,notes))))

        (when .unpushedid
          (org-harvest--post-time-entry
           content
           headers
           (lambda
             (newid)
             (org-harvest--in-marker marker
                                     (org-entry-delete nil "HARVEST_UNPUSHED_ID")
                                     (org-entry-put nil "HARVEST_TIMESHEET_ID" (number-to-string newid))))))

        (when .timesheetid
          (org-harvest--patch-time-entry
           .timesheetid
           content
           headers
           (lambda ()
             (org-harvest--post-time-entry
              content
              headers
              (lambda (newid)
                (org-harvest--in-marker marker
                                        (org-entry-put
                                         nil
                                         "HARVEST_TIMESHEET_ID"
                                         (number-to-string newid))))))))))))


(defun org-harvest--sync-action (headers)
  `(let ((marker (point-marker))
        (logbooks (org-harvest--parse-clock-lines-in-heading ,org-harvest--export-data-format)))
    (org-harvest--sync-logbooks logbooks ',headers marker)))

(defun org-harvest--consult-async-split-none (_str &optional _plist)
  "Completely ignore input STR for splitting/filtering."
  (list "" 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autoloads/interactive ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun org-harvest-tasks ()
  (interactive)
  (let* ((consult-async-split-styles-alist
          (append '((org-harvest--none
                     :function org-harvest--consult-async-split-none))
                  consult-async-split-styles-alist))
         (consult-async-split-style 'org-harvest--none)
         (consult-async-min-input 0)
         (consult-async-refresh-delay 0)
         (consult-async-input-throttle 1000)
         (authinfo (org-harvest--get-authinfo))
         (headers (org-harvest--make-request-headers authinfo)))
    (consult--read
     (consult--dynamic-collection
         (lambda (_input cb)
           (funcall cb org-harvest--projects-cache)
           (let* ((res (org-harvest--get-proj-assignments headers))
                  (cands (org-harvest--tasks-get-cands res)))
             (setq org-harvest--projects-cache cands)
             (funcall cb cands))))
     :state 'org-harvest--tasks-state
     :group 'org-harvest--tasks-group)))

;;;###autoload
(defun org-harvest-sync ()
  (interactive)
  (let* ((authinfo (org-harvest--get-authinfo))
         (headers (org-harvest--make-request-headers authinfo)))
    (org-ql-select (or org-harvest-files
                       org-agenda-files)
      org-harvest--sync-query
      :action `(lambda () ,(org-harvest--sync-action headers)))))

(provide 'org-harvest)

;;; org-harvest.el ends here
