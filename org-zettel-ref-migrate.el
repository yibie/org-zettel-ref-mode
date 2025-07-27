;;; org-zettel-ref-migrate.el --- Migration utilities for org-zettel-ref -*- lexical-binding: t; -*-

(require 'org-zettel-ref-db)
(require 'org-zettel-ref-core)
(require 'org-element) ; Ensure org-element is loaded for robust parsing

;;;###autoload
(defun org-zettel-ref-migrate ()
  "Interactive command to migrate from old database format to new structure."
  (interactive)
  ;; Define the hash table file path
  (let ((hash-table-file org-zettel-ref-db-file))
    ;; Check if the hash table file exists  
    (if (not (file-exists-p hash-table-file))
        (message "Hash table file does not exist, cannot perform migration. Path: %s" hash-table-file)
      ;; If it exists, continue with migration
      (when (yes-or-no-p "This will migrate your old database to the new format. Continue? ")
        (let ((old-data (org-zettel-ref-load-data)))
          (if (not old-data)
              (message "Old database not found or invalid, cannot perform migration.")
            (condition-case err
                (let ((new-db (org-zettel-ref-migrate-from-old-format--internal old-data)))
                  ;; Save the new database
                  (org-zettel-ref-db-save new-db)
                  (message "Migration completed successfully. New database has been saved."))
              (error
               (message "Error during migration: %S" err)))))))))

;; Keep the original function but rename it to indicate its internal use
(defun org-zettel-ref-migrate-from-old-format--internal (old-data)
  "Internal function to migrate from old database format to new structure.
OLD-DATA is the old database data structure."
  (message "Starting migration from old format...")
  
  ;; Create a new database instance
  (let ((new-db (make-org-zettel-ref-db)))
    
    ;; Extract the mapping relationship from the old data
    (let ((overview-index (cdr (assq 'overview-index old-data))))
      (message "DEBUG: Old overview-index size: %d" (hash-table-count overview-index))
      (message "DEBUG: Old overview-index content:")
      (maphash (lambda (k v) (message "  %s -> %s" k v)) overview-index)
      
      (maphash
       (lambda (ref-path overview-path)
         (message "DEBUG: Processing ref-path: %s" ref-path)
         (message "DEBUG: Processing overview-path: %s" overview-path)
         
         ;; Create and store reference entry
         (let* ((ref-entry (org-zettel-ref-db-create-ref-entry
                           new-db
                           ref-path
                           (file-name-base ref-path)
                           nil  ; author
                           nil)) ; keywords
                (ref-id (org-zettel-ref-ref-entry-id ref-entry)))
           
           (message "DEBUG: Create reference entry, ID: %s" ref-id)
           
           ;; Add reference entry
           (org-zettel-ref-db-add-ref-entry new-db ref-entry)
           
           ;; Create and store overview entry ID (ensure uniqueness)
           (let* ((base-id (format-time-string "%Y%m%dT%H%M%S"))
                  (counter 0)
                  (overview-id base-id))
             
             ;; Ensure overview-id is unique
             (while (gethash overview-id (org-zettel-ref-db-overviews new-db))
               (setq counter (1+ counter)
                     overview-id (format "%s-%03d" base-id counter)))
             
             ;; Create overview entry
             (let ((overview-entry (make-org-zettel-ref-overview-entry
                                  :id overview-id
                                  :ref-id ref-id
                                  :file-path overview-path
                                  :title (file-name-base overview-path)
                                  :created (current-time)
                                  :modified (current-time))))
               
               (message "DEBUG: Create overview entry, ID: %s" overview-id)
               
               ;; Add overview entry
               (org-zettel-ref-db-add-overview-entry new-db overview-entry)
               
               ;; Establish mapping relationship
               (org-zettel-ref-db-add-map new-db ref-id overview-id)))))
       overview-index))
  
    ;; Migration completed  
    (message "Migration completed.")
    (message "DEBUG: Final refs count: %d" 
             (hash-table-count (org-zettel-ref-db-refs new-db)))
    (message "DEBUG: Final overviews count: %d" 
             (hash-table-count (org-zettel-ref-db-overviews new-db)))
    (message "DEBUG: Final maps count: %d" 
             (hash-table-count (org-zettel-ref-db-map new-db)))
    
    new-db))

;; Keep the original function but rename it to indicate its internal use
(defun org-zettel-ref-load-data ()
  "Load old database data for migration."
  (when (file-exists-p org-zettel-ref-db-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents org-zettel-ref-db-file)
          (read (current-buffer)))
      (error
       (message "Error loading old database: %S" err)
       nil))))
       
(defun org-zettel-ref-convert-highlights-to-list ()
  "Convert all highlight entries in the current Org buffer from headline format to list format.
This function is interactive and should be called in an Org buffer (e.g., an overview file).
This function uses `org-element` for robust parsing and will prompt for confirmation before making changes."
  (interactive)
  (unless (eq major-mode 'org-mode) (user-error "This command can only be run in an Org mode buffer."))

  (when (y-or-n-p "Convert all highlight entries in this buffer from headline to list format? This action is irreversible without undo.")
    (save-excursion
      (let ((converted-count 0)
            (original-buffer-modified-p (buffer-modified-p)))
        (goto-char (point-min))
        ;; Ensure org-element cache is reset for fresh parsing
        (when (fboundp 'org-element-cache-reset) (org-element-cache-reset))

        (org-with-wide-buffer
          ;; Iterate through all level 2 headlines (our highlight entries)
          (while (re-search-forward "^\\*\\* .*$" nil t)
            (let* ((headline-start (match-beginning 0))
                   (headline-end (match-end 0))
                   (original-hl-id nil)
                   (hl-type-sym nil)
                   (hl-text-content nil)
                   (hl-type-name nil)
                   (hl-prefix nil)
                   (hl-img-path nil)
                   (hl-img-desc nil))

              ;; Parse the headline element at its position for robust extraction
              (save-excursion
                (goto-char headline-start)
                (let* ((headline-element (org-element-at-point))
                       (properties (org-element-property :properties headline-element)))
                  (setq original-hl-id (plist-get properties :ORIGINAL_HL_ID))
                  
                  ;; Fallback: try traditional org-entry-properties if org-element fails
                  (when (null original-hl-id)
                    (org-back-to-heading t)
                    (let ((props (org-entry-properties)))
                      (setq original-hl-id (cdr (assoc "ORIGINAL_HL_ID" props)))))
                  
                  (message "DEBUG: Properties from org-element: %s" properties)
                  (message "DEBUG: ORIGINAL_HL_ID from org-element: %s" (plist-get properties :ORIGINAL_HL_ID))
                  (message "DEBUG: ORIGINAL_HL_ID after fallback: %s" original-hl-id)
                  
                  ;; SOURCE_REF_ID is not directly used for formatting, but good to have for context if needed
                  ;; (setq source-ref-id (plist-get properties :SOURCE_REF_ID))

                  ;; Extract prefix and content from headline title
                  (let* ((title (org-element-property :raw-value headline-element)))
                    (setq hl-text-content title) ; Default to full title
                    (cl-block nil
                      (dolist (type-def org-zettel-ref-highlight-types)
                        (let* ((prefix (plist-get (cdr type-def) :prefix))
                               (prefix-re (regexp-quote prefix)))
                          (when (string-match (concat "^" prefix-re " \\(.*\\)$") title)
                            (setq hl-prefix prefix)
                            (setq hl-text-content (match-string 1 title))
                            (setq hl-type-sym (car type-def))
                            (setq hl-type-name (plist-get (cdr type-def) :name))
                            (cl-return))))))

                  ;; Check for image link within the subtree
                  (cl-block nil
                    (org-element-map headline-element 'link
                      (lambda (link)
                        (when (string= (org-element-property :type link) "file")
                          (let ((path (org-element-property :path link)))
                            (when (string-match-p "\\.\\(jpg\\|jpeg\\|png\\|gif\\|svg\\|webp\\)$" path)
                              (setq hl-img-path path)
                              (setq hl-img-desc (org-element-property :description link))
                              (setq hl-type-sym 'image)
                              (setq hl-prefix "🖼️") ; Ensure image prefix
                              (setq hl-text-content (format "[[file:%s]]" hl-img-path)) ; Content is the link
                              (cl-return))))))) ; Found image, break map
                  )) ; End of save-excursion for parsing

            ;; Now, construct the new string and replace
            (message "DEBUG: Found headline with ORIGINAL_HL_ID: %s, hl-text-content: %s" original-hl-id hl-text-content)
            (when (and original-hl-id (or hl-text-content title)) ; Ensure we have essential data
                ;; Delete old headline and its properties
                ;; Need to get the end of the subtree *before* deleting
                (let ((subtree-end (save-excursion (goto-char headline-start) (org-end-of-subtree t t))))
                  (delete-region headline-start subtree-end))
                ;; Insert new list item
                (let ((display-text (if (eq hl-type-sym 'image) (or hl-img-desc hl-type-name) (or hl-text-content title))))
                  (insert (format "- %s %s [[hl:%s][hl-%s]]\n" 
                                 (or hl-prefix "") display-text original-hl-id original-hl-id)))
                (setq converted-count (1+ converted-count))))))

        (if (> converted-count 0)
            (message "Converted %d highlight entries to list format." converted-count)
          (message "No headline-formatted highlight entries found for conversion."))
        (unless original-buffer-modified-p
          (set-buffer-modified-p nil)))))) 

(defun org-zettel-ref-check-for-headline-highlights (buffer)
  "Check if BUFFER contains any highlight entries in headline format.
Returns t if headline highlights are found, nil otherwise."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      ;; Search for level 2 headlines that contain ORIGINAL_HL_ID property
      (re-search-forward "^\\*\\* .*\\n:PROPERTIES:\\n:ORIGINAL_HL_ID: .+\\n:END:" nil t))))

(defun org-zettel-ref-prompt-for-highlight-conversion (buffer)
  "Prompt user to convert headline-formatted highlights to list format in BUFFER.
If user agrees, performs the conversion."
  (interactive)
  (with-current-buffer buffer
    (when (y-or-n-p (format "This overview file contains highlights in headline format, but your 'org-zettel-ref-highlight-format' is set to 'list'. Convert to list format now? (This will modify the file)"))
      (message "Converting highlights to list format...")
      (org-zettel-ref-convert-highlights-to-list)
      (message "Conversion complete."))))
      
(provide 'org-zettel-ref-migrate)
;;; org-zettel-ref-migrate.el ends here





