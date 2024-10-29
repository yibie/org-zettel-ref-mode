;;; org-zettel-ref-db.el --- Database operations for org-zettel-ref -*- lexical-binding: t; -*-

;;; Code:



;;;----------------------------------------------------------------------------
;;; Core Data Structures
;;;----------------------------------------------------------------------------

(cl-defstruct (org-zettel-ref-entry (:constructor org-zettel-ref-make-entry))
  "Database entry structure."
  id              ; timestamp ID
  source-file     ; source file path
  overview-file   ; overview file path
  title           ; title
  author          ; author
  keywords        ; keywords list
  modified-time   ; modified time
  added-time      ; added time
  metadata)       ; extra metadata (hash table)

(cl-defstruct (org-zettel-ref-db (:constructor org-zettel-ref-make-db))
  "Database structure."
  (version "3.0")                              ; database version
  (timestamp (current-time))                   ; last update time
  (entries (make-hash-table :test 'equal))     ; ID => entry mapping
  (paths (make-hash-table :test 'equal))       ; path => ID mapping
  (dirty nil))                                 ; whether there are unsaved changes

(defcustom org-zettel-ref-db-file 
  (expand-file-name ".zettel-ref-db.el" user-emacs-directory)
  "Database file path."
  :type 'file
  :group 'org-zettel-ref)

(defvar org-zettel-ref--db nil
  "Current database instance.")

(defvar org-zettel-ref-db-state-change-hook nil
  "当条目状态发生变化时运行的钩子。")

;;;----------------------------------------------------------------------------
;;; Database Operations
;;;----------------------------------------------------------------------------

(defmacro org-zettel-ref-with-transaction (&rest body)
  "Execute operation in transaction, support rollback."
  (declare (indent 0) (debug t))
  `(let ((old-state (org-zettel-ref--save-db-state)))
     (condition-case err
         (prog1 (progn ,@body)
           (setf (org-zettel-ref-db-dirty org-zettel-ref--db) t))
       (error
        (org-zettel-ref--restore-db-state old-state)
        (signal (car err) (cdr err))))))

(defun org-zettel-ref--save-db-state ()
  "Save current database state."
  (when org-zettel-ref--db
    (list (copy-hash-table (org-zettel-ref-db-entries org-zettel-ref--db))
          (copy-hash-table (org-zettel-ref-db-paths org-zettel-ref--db)))))

(defun org-zettel-ref--restore-db-state (state)
  "Restore database state."
  (when (and org-zettel-ref--db state)
    (pcase-let ((`(,entries ,paths) state))
      (setf (org-zettel-ref-db-entries org-zettel-ref--db) entries
            (org-zettel-ref-db-paths org-zettel-ref--db) paths))))

(defun org-zettel-ref-db-ensure ()
  "Ensure database is initialized."
  (unless org-zettel-ref--db
    (setq org-zettel-ref--db
          (or (org-zettel-ref-db-load)
              (org-zettel-ref-make-db))))
  org-zettel-ref--db)

;;;----------------------------------------------------------------------------
;;; Entry Management
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-db-create-entry (source-file)
  "Create or update database entry for source file."
  (org-zettel-ref-with-transaction
    (when (and source-file (file-exists-p source-file))
      (let* ((file-info (org-zettel-ref-parse-filename 
                        (file-name-nondirectory source-file)))
             (existing-id (org-zettel-ref-db-get-id-by-path source-file))
             (id (or existing-id (format-time-string "%Y%m%dT%H%M%S")))
             (entry (org-zettel-ref-make-entry
                    :id id
                    :source-file source-file
                    :title (nth 1 file-info)
                    :author (nth 0 file-info)
                    :keywords (nth 2 file-info)
                    :modified-time (current-time)
                    :added-time (current-time)
                    :metadata (make-hash-table :test 'equal))))
        
        ;; update database
        (org-zettel-ref-db-put-entry id entry)
        (org-zettel-ref-db-update-path-index source-file id)
        
        ;; create and update overview file
        (when-let ((overview-file (org-zettel-ref-db-create-overview source-file)))
          (org-zettel-ref-db-update-path-index overview-file id)
          (setf (org-zettel-ref-entry-overview-file entry) overview-file)
          (org-zettel-ref-db-put-entry id entry))
        
        entry))))

(defun org-zettel-ref-db-put-entry (id entry)
  "Put entry into database."
  (org-zettel-ref-db-ensure)
  (puthash id entry (org-zettel-ref-db-entries org-zettel-ref--db)))

(defun org-zettel-ref-db-get-entry (id)
  "获取条目。如果不存在返回nil。"
  (when id
    (gethash id (org-zettel-ref-db-entries (org-zettel-ref-db-ensure)))))

(defun org-zettel-ref-db-update-metadata (id key value)
  "Update entry metadata."
  (when-let ((entry (org-zettel-ref-db-get-entry id)))
    (puthash key value (org-zettel-ref-entry-metadata entry))
    (setf (org-zettel-ref-db-dirty org-zettel-ref--db) t)))

(defun org-zettel-ref-db-get-metadata (id key)
  "Get entry metadata."
  (when-let ((entry (org-zettel-ref-db-get-entry id)))
    (gethash key (org-zettel-ref-entry-metadata entry))))

;;;----------------------------------------------------------------------------
;;; Path Index Operations
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-db-update-path-index (path id)
  "Update path index."
  (org-zettel-ref-db-ensure)
  (puthash path id (org-zettel-ref-db-paths org-zettel-ref--db))
  (setf (org-zettel-ref-db-dirty org-zettel-ref--db) t))

(defun org-zettel-ref-db-get-id-by-path (path)
  "通过文件路径获取ID。"
  (when path
    (gethash path (org-zettel-ref-db-paths (org-zettel-ref-db-ensure)))))

;;;----------------------------------------------------------------------------
;;; File Operations
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-db-create-overview (source-file)
  "Create overview file for source file."
  (org-zettel-ref-with-transaction
    (when-let* ((id (org-zettel-ref-db-get-id-by-path source-file))
                (entry (org-zettel-ref-db-get-entry id)))
      (let* ((overview-dir (org-zettel-ref-get-overview-dir))
             (overview-file (expand-file-name 
                             (concat id "--" 
                                    (file-name-nondirectory source-file)
                                    "__overview.org")
                             overview-dir)))
        ;; create overview file
        (unless (file-exists-p overview-file)
          (make-directory (file-name-directory overview-file) t)
          (with-temp-file overview-file
            (insert (format "#+TITLE: Overview of %s\n" 
                          (org-zettel-ref-entry-title entry)))))
        ;; update entry overview file path
        (setf (org-zettel-ref-entry-overview-file entry) overview-file)
        (org-zettel-ref-db-update-path-index overview-file id)
        overview-file))))

(defun org-zettel-ref-db-handle-rename (old-path new-path)
  "Handle file rename."
  (org-zettel-ref-with-transaction
    (when-let* ((id (org-zettel-ref-db-get-id-by-path old-path))
                (entry (org-zettel-ref-db-get-entry id)))
      ;; update entry
      (if (string= old-path (org-zettel-ref-entry-source-file entry))
          (setf (org-zettel-ref-entry-source-file entry) new-path)
        (setf (org-zettel-ref-entry-overview-file entry) new-path))
      ;; update path index
      (remhash old-path (org-zettel-ref-db-paths org-zettel-ref--db))
      (org-zettel-ref-db-update-path-index new-path id)
      
      entry)))

;;;----------------------------------------------------------------------------
;;; Database Persistence
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-db-save ()
  "Save database to file."
  (when (and org-zettel-ref--db
             (org-zettel-ref-db-dirty org-zettel-ref--db))
    (condition-case err
        (let ((print-length nil)
              (print-level nil))
          (with-temp-file org-zettel-ref-db-file
            (let ((data (list :version (org-zettel-ref-db-version org-zettel-ref--db)
                             :timestamp (current-time)
                             :entries (org-zettel-ref-db-entries org-zettel-ref--db)
                             :paths (org-zettel-ref-db-paths org-zettel-ref--db))))
              (prin1 data (current-buffer))))
          (setf (org-zettel-ref-db-dirty org-zettel-ref--db) nil))
      (error
       (message "Error saving database: %s" (error-message-string err))))))

(defun org-zettel-ref-db-load ()
  "Load database from file."
  (condition-case err
      (when (file-exists-p org-zettel-ref-db-file)
        (with-temp-buffer
          (insert-file-contents org-zettel-ref-db-file)
          (goto-char (point-min))
          (condition-case nil
              (let ((data (read (current-buffer))))
                (org-zettel-ref-make-db
                 :version (or (plist-get data :version) "3.0")
                 :timestamp (or (plist-get data :timestamp) (current-time))
                 :entries (or (plist-get data :entries) 
                            (make-hash-table :test 'equal))
                 :paths (or (plist-get data :paths) 
                          (make-hash-table :test 'equal))))
            (error
             ;; if read failed, return new database
             (org-zettel-ref-make-db)))))
    (error
     (message "Error loading database: %s" (error-message-string err))
     (org-zettel-ref-make-db))))

(defun org-zettel-ref-db-clear ()
  "Clear database."
  (setq org-zettel-ref--db (org-zettel-ref-make-db)))

;;;----------------------------------------------------------------------------
;;; Utility Functions
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-db-find-overview (source-file)
  "Find overview file for source file."
  (when-let* ((id (org-zettel-ref-db-get-id-by-path source-file))
              (entry (org-zettel-ref-db-get-entry id)))
    (org-zettel-ref-entry-overview-file entry)))

(defun org-zettel-ref-db-find-source (overview-file)
  "Find source file for overview file."
  (when-let* ((id (org-zettel-ref-db-get-id-by-path overview-file))
              (entry (org-zettel-ref-db-get-entry id)))
    (org-zettel-ref-entry-source-file entry)))

(defun org-zettel-ref-db-get-id-by-source (source-file)
  "Get entry ID by source file path."
  (let ((found-id nil))
    (maphash
     (lambda (id entry)
       (when (string= (org-zettel-ref-entry-source-file entry) source-file)
         (setq found-id id)))
     (org-zettel-ref-db-entries (org-zettel-ref-db-ensure)))
    found-id))

(defun org-zettel-ref-db-get-by-id (id)
  "Get entry by ID."
  (gethash id (org-zettel-ref-db-entries (org-zettel-ref-db-ensure))))

(defun org-zettel-ref-db-put (id entry)
  "Put entry into database."
  (puthash id entry (org-zettel-ref-db-entries (org-zettel-ref-db-ensure)))
  (setf (org-zettel-ref-db-dirty (org-zettel-ref-db-ensure)) t))

(defun org-zettel-ref-db-has-overview-p (id)
  "检查条目是否有对应的概览文件。"
  (when-let ((entry (org-zettel-ref-db-get-entry id)))
    (and (org-zettel-ref-entry-overview-file entry)
         (file-exists-p (org-zettel-ref-entry-overview-file entry)))))

(defun org-zettel-ref-db-get-overview (id)
  "获取条目的概览文件路径。"
  (when-let ((entry (org-zettel-ref-db-get-entry id)))
    (org-zettel-ref-entry-overview-file entry)))

;; 添加明确的状态判断函数
(defun org-zettel-ref-db-entry-read-p (entry)
  "判断条目是否已读。"
  (and (org-zettel-ref-entry-overview-file entry)
       (file-exists-p (org-zettel-ref-entry-overview-file entry))))

(defun org-zettel-ref-db-entry-unread-p (entry)
  "判断条目是否未读。"
  (not (org-zettel-ref-db-entry-read-p entry)))

(defun org-zettel-ref-db-notify-state-change (entry)
  "通知条目状态变化。"
  (run-hook-with-args 'org-zettel-ref-db-state-change-hook entry))

(provide 'org-zettel-ref-db)
;;; org-zettel-ref-db.el ends here
