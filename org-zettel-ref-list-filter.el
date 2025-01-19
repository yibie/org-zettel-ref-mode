;; org-zettel-ref-list-filter.el --- Filtering for org-zettel-ref list -*- lexical-binding: t; -*-

;;;----------------------------------------------------------------------------
;;; Search and Filtering 
;;;----------------------------------------------------------------------------

(defgroup org-zettel-ref-filter nil
  "Customization for org-zettel-ref filtering."
  :group 'org-zettel-ref)

(defvar-local org-zettel-ref-active-filters nil
  "Current active filters list.
Each filter is a (column . predicate) cons cell.")

(defvar org-zettel-ref-filter-history nil
  "History of filter patterns for each column.")

(defvar org-zettel-ref-filter-history-list nil
  "History list for filter patterns.")

(defcustom org-zettel-ref-filter-history-file
  (expand-file-name "org-zettel-ref-filter-history.el" user-emacs-directory)
  "File path to store filter history."
  :type 'file
  :group 'org-zettel-ref-filter)

;;; Filter Predicate Generation Functions
(defun org-zettel-ref-make-string-filter (column pattern)
  "Create a string matching filter.
COLUMN is the column index, PATTERN is the pattern to match."
  (cons column
        (lambda (entry)
          (let ((value (aref (cadr entry) column)))
            (and value (string-match-p pattern value))))))

;;;----------------------------------------------------------------------------   
;;; Filter Commands 
;;;---------------------------------------------------------------------------- 

(defun org-zettel-ref-filter-by-regexp (column)
  "Filter by regular expression for a specific column."
  (interactive
   (list
    (let* ((columns '(("Title" . 0)
                     ("Author" . 1)
                     ("Modified" . 2)
                     ("Keywords" . 3)))  
           (choice (completing-read "Filter column: "
                                  (mapcar #'car columns)
                                  nil t)))
      (cdr (assoc choice columns)))))
  (let* ((prompt (format "Filter by %s (regexp): "
                        (aref tabulated-list-format column)))
         (history-key (format "column-%d" column))
         (pattern (completing-read prompt
                                 (reverse (alist-get history-key org-zettel-ref-filter-history nil nil #'equal))
                                 nil nil nil
                                 'org-zettel-ref-filter-history-list)))
    (if (string-empty-p pattern)
        (org-zettel-ref-remove-filter column)
      (progn
        ;; 保存到历史记录
        (push (cons history-key
                   (cons pattern (delete pattern (alist-get history-key org-zettel-ref-filter-history nil nil #'equal))))
              org-zettel-ref-filter-history)
        ;; 保存到通用历史记录列表
        (add-to-history 'org-zettel-ref-filter-history-list pattern)
        ;; 保存历史记录到文件
        (org-zettel-ref-filter-save-history)
        (org-zettel-ref-add-filter
         (org-zettel-ref-make-string-filter column pattern))))
    (org-zettel-ref-list-refresh)))

(defun org-zettel-ref-filter-by-multiple-conditions ()
  "Filter entries by multiple conditions across different columns.
Allows setting multiple filter conditions interactively.
Use TAB to finish adding conditions and apply filters."
  (interactive)
  (let* ((columns '(("Title" . 0)
                   ("Author" . 1)
                   ("Modified" . 2)
                   ("Keywords" . 3)))
         ;; Add special completion option for finishing
         (completion-choices
          (append (mapcar #'car columns)
                  '("[Done] Apply Filters")))
         (conditions '())
         (continue t))
    
    ;; Clear existing filters
    (setq org-zettel-ref-active-filters nil)
    
    ;; Show initial help message
    (message "Select columns to filter. Press TAB and select [Done] to finish.")
    
    ;; Collect filter conditions
    (while continue
      (condition-case nil
          (let* ((col-name (completing-read 
                           (format "Select column to filter (%d active) [TAB to finish]: "
                                   (length conditions))
                           completion-choices
                           nil t))
                 (column (cdr (assoc col-name columns))))
            
            (if (string= col-name "[Done] Apply Filters")
                (setq continue nil)
              (let* ((prompt (format "Filter %s by (regexp): " col-name))
                     (history-key (format "column-%d" column))
                     (pattern (completing-read prompt
                                            (reverse (alist-get history-key org-zettel-ref-filter-history nil nil #'equal))
                                            nil nil nil
                                            'org-zettel-ref-filter-history-list)))
                (unless (string-empty-p pattern)
                  ;; 保存到历史记录
                  (push (cons history-key
                            (cons pattern (delete pattern (alist-get history-key org-zettel-ref-filter-history nil nil #'equal))))
                        org-zettel-ref-filter-history)
                  ;; 保存到通用历史记录列表
                  (add-to-history 'org-zettel-ref-filter-history-list pattern)
                  ;; 保存历史记录到文件
                  (org-zettel-ref-filter-save-history)
                  (push (org-zettel-ref-make-string-filter column pattern)
                        conditions)
                  (message "Added filter for %s: \"%s\"" col-name pattern)))))
        
        ;; Still keep C-g for emergency exit
        (quit (setq continue nil)
              (message "Filter selection cancelled."))))
    
    ;; Apply all collected conditions
    (when conditions
      (dolist (condition conditions)
        (org-zettel-ref-add-filter condition))
      
      ;; Refresh the display
      (org-zettel-ref-list-refresh)
      
      ;; Show final feedback with correct column name access
      (message "Applied %d filter(s): %s"
               (length conditions)
               (mapconcat
                (lambda (f)
                  (let ((col-idx (car f)))
                    (format "%s" (car (aref tabulated-list-format col-idx)))))
                conditions
                ", ")))
    
    ;; Show message if no filters were applied
    (when (null conditions)
      (message "No filters applied"))))

(defun org-zettel-ref-filter-by-history ()
  "Apply a filter from history."
  (interactive)
  (let* ((entries (org-zettel-ref-filter--get-history-entries))
         (choice (completing-read "Apply filter from history: "
                                (mapcar #'car entries)
                                nil t)))
    (when choice
      (let* ((entry (cdr (assoc choice entries)))
             (column (car entry))
             (pattern (cdr entry)))
        (org-zettel-ref-add-filter
         (org-zettel-ref-make-string-filter column pattern))
        (org-zettel-ref-list-refresh)
        (message "Applied filter: %s" choice)))))

;;;----------------------------------------------------------------------------   
;;; Filter Management 
;;;---------------------------------------------------------------------------- 

(defun org-zettel-ref-add-filter (filter)
  "Add a filter condition."
  (let ((column (car filter)))
    ;; Remove old filter conditions for the same column
    (setq org-zettel-ref-active-filters
          (cl-remove-if (lambda (f) (= (car f) column))
                       org-zettel-ref-active-filters))
    ;; Add new filter condition
    (push filter org-zettel-ref-active-filters)))

(defun org-zettel-ref-remove-filter (column)
  "Remove a filter condition for a specific column."
  (setq org-zettel-ref-active-filters
        (cl-remove-if (lambda (f) (= (car f) column))
                     org-zettel-ref-active-filters))
  (org-zettel-ref-list-refresh))

(defun org-zettel-ref-filter-clear-all ()
  "Clear all filter conditions."
  (interactive)
  (setq org-zettel-ref-active-filters nil)
  (org-zettel-ref-list-refresh))

;;;----------------------------------------------------------------------------
;;; Apply Filters 
;;;---------------------------------------------------------------------------- 

(defun org-zettel-ref-apply-filters (entries)
  "Apply filters to entries."
  (if (null org-zettel-ref-active-filters)
      entries
    (cl-remove-if-not
     (lambda (entry)
       (cl-every (lambda (filter)
                   (funcall (cdr filter) entry))
                org-zettel-ref-active-filters))
     entries)))

;;; Display Filter Status
(defun org-zettel-ref-list--format-filter-info ()
  "Format current filter condition information."
  (if org-zettel-ref-active-filters
      (format " [Filters: %s]"
              (mapconcat
               (lambda (filter)
                 (let ((col (aref tabulated-list-format (car filter))))
                   (format "%s" (car col))))
               org-zettel-ref-active-filters
               ","))
    ""))

(defun org-zettel-ref-filter--get-history-entries ()
  "Get all filter history entries with their column information."
  (let (entries)
    (dolist (entry org-zettel-ref-filter-history)
      (let* ((col-key (car entry))
             (patterns (cdr entry))
             (col-num (when (string-match "column-\\([0-9]+\\)" col-key)
                       (string-to-number (match-string 1 col-key))))
             (col-name (when col-num
                        (aref tabulated-list-format col-num))))
        (when (and col-num col-name)
          (dolist (pattern patterns)
            (push (cons (format "%s: %s" (car col-name) pattern)
                       (cons col-num pattern))
                  entries)))))
    (reverse entries)))

;;;----------------------------------------------------------------------------
;;; History Management
;;;---------------------------------------------------------------------------- 

(defun org-zettel-ref-filter-save-history ()
  "Save filter history to file."
  (when org-zettel-ref-filter-history
    (with-temp-file org-zettel-ref-filter-history-file
      (let ((print-length nil)
            (print-level nil))
        (insert ";; -*- lexical-binding: t; -*-\n")
        (insert ";; Org-zettel-ref filter history\n")
        (insert ";; Automatically generated by org-zettel-ref-mode\n\n")
        (prin1 `(setq org-zettel-ref-filter-history ',org-zettel-ref-filter-history)
               (current-buffer))
        (insert "\n")
        (prin1 `(setq org-zettel-ref-filter-history-list ',org-zettel-ref-filter-history-list)
               (current-buffer))))))

(defun org-zettel-ref-filter-load-history ()
  "Load filter history from file."
  (when (file-exists-p org-zettel-ref-filter-history-file)
    (load org-zettel-ref-filter-history-file)))

;;;----------------------------------------------------------------------------
;;; Initialization 
;;;----------------------------------------------------------------------------  

(defun org-zettel-ref-filter-initialize ()
  "Initialize the filter system."
  (org-zettel-ref-filter-load-history))

(org-zettel-ref-filter-initialize)

(provide 'org-zettel-ref-list-filter)
;;; org-zettel-ref-list-filter.el ends here
