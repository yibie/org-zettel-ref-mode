;;; org-zettel-ref-db.el --- Database operations for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains database operation functions for org-zettel-ref.

;;; Code:

(require 'org-zettel-ref-core)
(require 'org-zettel-ref-org-roam)

;;;----------------------------------------------------------------------------
;;; Org-roam Database intergration
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-update-roam-db (file)
  "Update Org-roam database for FILE using org-roam-db-query."
  (let ((node (org-roam-db-query
               [:select * :from nodes
                :where (= file $s1)]
               file)))
    (if node
        (progn
          ;; add update org-roam-db-query here
          (message "update Org-roam database, node ID: %s, title: %s"
                   (gethash "id" (car node))
                   (gethash "title" (car node)))))))

(defun org-zettel-ref-db-update (file)
  "Update database for FILE."
  (cond
   ((eq org-zettel-ref-mode-type 'org-roam)
    (org-zettel-ref-update-roam-db file))
   ;; Add other database update methods here if needed
   (t (message "No database update method for current mode type"))))



(defun org-zettel-ref-check-roam-db ()
  "Check the status of the org-roam database."
  (interactive)
  (if (require 'org-roam nil t)
      (condition-case err
          (progn
            (message "Org-roam version: %s" (org-roam-version))
            (message "Org-roam directory: %s" org-roam-directory)
            (message "Org-roam database file: %s" org-roam-db-location)
            (if (file-exists-p org-roam-db-location)
                (message "Database file exists")
              (message "Database file does not exist"))
            (let ((node-count (caar (org-roam-db-query [:select (funcall count *) :from nodes]))))
              (message "Number of nodes in database: %d" node-count)))
        (error
         (message "Error checking org-roam database: %S" err)))
    (message "Org-roam is not available")))

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

(defun org-zettel-ref-get-overview-from-index (source-file)
  "Get the overview file for SOURCE-FILE from the index."
  (gethash source-file org-zettel-ref-overview-index))


(provide 'org-zettel-ref-db)

;;; org-zettel-ref-db.el ends here
