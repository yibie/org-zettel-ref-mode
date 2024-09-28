;;; org-zettel-ref-db.el --- Database operations for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains database operation functions for org-zettel-ref.

;;; Code:

(require 'org-roam nil t)  ; Safely attempt to load org-roam
(require 'denote nil t)  ; Safely attempt to load denote

;;;----------------------------------------------------------------------------
;;; Org-roam Database integration
;;;----------------------------------------------------------------------------



(defun org-zettel-ref-update-roam-db (file)
  "Update Org-roam database for FILE using org-roam-db-query."
  (when (featurep 'org-roam)
    (let ((node (org-roam-db-query
                 [:select * :from nodes
                  :where (= file $s1)]
                 file)))
      (if node
          (progn
            ;; add update org-roam-db-query here
            (message "update Org-roam database, node ID: %s, title: %s"
                     (gethash "id" (car node))
                     (gethash "title" (car node))))
        (message "Node not found in Org-roam database: %s" file)))))

(defun org-zettel-ref-db-update (file)
  "Update database for FILE."
  (cond
   ((eq org-zettel-ref-mode-type 'org-roam)
    (org-zettel-ref-update-roam-db file))
   ;; Add other database update methods here if needed
   (t (message "No database update method for current mode type"))))

;;;----------------------------------------------------------------------------
;;; Denote&Normal Index Source File
;;;----------------------------------------------------------------------------

(defvar org-zettel-ref-overview-index (make-hash-table :test 'equal)
  "Hash table storing the mapping of source files to overview files.")

(defun org-zettel-ref-load-index ()
  "Load the overview index from a file."
  (let ((index-file (expand-file-name ".overview-index.el" org-zettel-ref-overview-directory)))
    (if (file-exists-p index-file)
        (with-temp-buffer
          (insert-file-contents index-file)
          (setq org-zettel-ref-overview-index (read (current-buffer))))
      (setq org-zettel-ref-overview-index (make-hash-table :test 'equal)))))

(defun org-zettel-ref-query-index (source-file)
  "Query the overview index for the given SOURCE-FILE.
Return the associated overview file name if it exists, otherwise return nil."
  (let ((overview-file (gethash source-file org-zettel-ref-overview-index)))
    (if overview-file
        (progn
          (message "Found overview file: %s" overview-file)
          overview-file)
      (progn
        (message "No overview file found for source file: %s" source-file)
        nil))))

(defun org-zettel-ref-save-index ()
  "Save the overview index to a file."
  (let ((index-file (expand-file-name ".overview-index.el" org-zettel-ref-overview-directory)))
    (with-temp-file index-file
      (prin1 org-zettel-ref-overview-index (current-buffer)))))

(defun org-zettel-ref-update-index (source-file overview-file)
  "Update the index with a new or existing SOURCE-FILE to OVERVIEW-FILE mapping."
  (puthash source-file overview-file org-zettel-ref-overview-index)
  (org-zettel-ref-save-index)
  (message "Debug: Updated index with %s -> %s" source-file overview-file))

(defun org-zettel-ref-get-overview-from-index (id)
  "Get the overview file for ID from the index."
  (gethash (concat id "__overview") org-zettel-ref-overview-index))

(defun org-zettel-ref-check-and-repair-links ()
  "Check and repair links between source files and overview files."
  (interactive)
  (let ((repaired 0))
    (maphash (lambda (source-file overview-file)
               ;; check source file if exists
               (unless (file-exists-p source-file)
                 (remhash source-file org-zettel-ref-overview-index)
                 (setq repaired (1+ repaired)))
               ;; check overview file if exists
               (unless (file-exists-p overview-file)
                 ;; create missing overview file using `org-zettel-ref-get-overview-file`
                 ;; since `org-zettel-ref-get-overview-file` needs a buffer, we can temporarily create a buffer
                 (with-temp-buffer
                   (insert-file-contents source-file)
                   (let ((source-buffer (current-buffer)))
                     (org-zettel-ref-get-overview-file source-buffer)))
                 (setq repaired (1+ repaired))))
             org-zettel-ref-overview-index)
    (org-zettel-ref-save-index)
    (message "Checked and repaired %d links." repaired)))

(defun org-zettel-ref-rescan-overview-files ()
  "Rescan the overview directory and update the index."
  (interactive)
  (let ((old-index org-zettel-ref-overview-index))
    (setq org-zettel-ref-overview-index (make-hash-table :test 'equal))
    (org-zettel-ref-load-index)
    (org-zettel-ref-check-and-repair-links)
    (message "Rescaned overview files and updated index.")))

(defun org-zettel-ref-status ()
  "Display the current status of org-zettel-ref-mode."
  (interactive)
  (message "org-zettel-ref-mode status:
- Current file: %s
- Overview directory: %s"
           (buffer-file-name)
           org-zettel-ref-overview-directory))

(defun org-zettel-ref-maintenance-menu ()
  "Display a menu for org-zettel-ref-mode maintenance operations."
  (interactive)
  (let ((choice (read-char-choice
                 "Org-zettel-ref maintenance:
s: Show status
q: Quit
Your choice: "
                 '(?s ?q))))
    (cond
     ((eq choice ?s) (org-zettel-ref-status))
     ((eq choice ?q) (message "Quit")))))

(provide 'org-zettel-ref-db)

;;; org-zettel-ref-db.el ends here
