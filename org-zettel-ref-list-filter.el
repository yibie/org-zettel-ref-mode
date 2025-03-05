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

(defvar org-zettel-ref-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "/") 'org-zettel-ref-filter-unified)
    (define-key map (kbd "f") 'org-zettel-ref-filter-by-regexp)
    (define-key map (kbd "F") 'org-zettel-ref-filter-by-multiple-conditions)
    (define-key map (kbd "h") 'org-zettel-ref-filter-by-history)
    (define-key map (kbd "c") 'org-zettel-ref-filter-clear-all)
    map)
  "Keymap for org-zettel-ref filter commands.")

(defun org-zettel-ref-filter-setup-keybindings (mode-map)
  "Set up filter keybindings in MODE-MAP."
  (define-key mode-map (kbd "/") 'org-zettel-ref-filter-unified)
  (define-key mode-map (kbd "f") 'org-zettel-ref-filter-by-regexp)
  (define-key mode-map (kbd "F") 'org-zettel-ref-filter-by-multiple-conditions)
  (define-key mode-map (kbd "h") 'org-zettel-ref-filter-by-history)
  (define-key mode-map (kbd "c") 'org-zettel-ref-filter-clear-all))

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
  (let* ((column-names '((0 . "title") (1 . "author") (2 . "modified") (3 . "keywords")))
         (column-name (cdr (assoc column column-names)))
         (prompt (format "Filter by %s (regexp): "
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
        
        ;; 使用统一过滤器
        (org-zettel-ref-filter-unified (format "%s:%s" column-name pattern))))))

(defun org-zettel-ref-filter-by-multiple-conditions ()
  "Filter entries by multiple conditions across different columns.
Allows setting multiple filter conditions interactively.
Use TAB to finish adding conditions and apply filters."
  (interactive)
  (let* ((columns '(("Title" . 0)
                   ("Author" . 1)
                   ("Modified" . 2)
                   ("Keywords" . 3)))
         (column-names '((0 . "title") (1 . "author") (2 . "modified") (3 . "keywords")))
         ;; Add special completion option for finishing
         (completion-choices
          (append (mapcar #'car columns)
                  '("[Done] Apply Filters")))
         (conditions '())
         (query-parts '())
         (continue t))
    
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
                  
                  ;; 构建查询部分
                  (let ((column-name (cdr (assoc column column-names))))
                    (push (format "%s:%s" column-name pattern) query-parts))
                  
                  (push (org-zettel-ref-make-string-filter column pattern)
                        conditions)
                  (message "Added filter for %s: \"%s\"" col-name pattern)))))
        
        ;; Still keep C-g for emergency exit
        (quit (setq continue nil)
              (message "Filter selection cancelled."))))
    
    ;; Apply all collected conditions using unified filter
    (when query-parts
      (org-zettel-ref-filter-unified (string-join (nreverse query-parts) " ")))))

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
             (pattern (cdr entry))
             (column-names '((0 . "title") (1 . "author") (2 . "modified") (3 . "keywords")))
             (column-name (cdr (assoc column column-names))))
        ;; 使用统一过滤器
        (org-zettel-ref-filter-unified (format "%s:%s" column-name pattern))
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
  (org-zettel-ref-list-refresh)
  (message "All filters cleared"))

;;;----------------------------------------------------------------------------
;;; Enhanced Search and Filtering
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-filter-unified (&optional initial-query)
  "Unified filter interface supporting multiple conditions.
INITIAL-QUERY is an optional starting query string.

The query syntax supports:
- Simple text: matches across all columns
- Column specific: 'title:text' matches only in title column
- Multiple terms: space-separated terms are treated as AND conditions
- Quoted terms: \"exact phrase\" for exact matching
- Negation: '-term' excludes entries containing the term

Examples:
  emacs lisp       - entries containing both 'emacs' and 'lisp' in any column
  title:emacs      - entries with 'emacs' in the title
  author:stallman  - entries with 'stallman' in the author field
  \"org mode\" -emacs - entries with exact phrase 'org mode' but not 'emacs'"
  (interactive)
  (let* ((columns '(("title" . 0)
                    ("author" . 1)
                    ("modified" . 2)
                    ("keywords" . 3)))
         (query (or initial-query
                    (read-string "Filter query: " nil 'org-zettel-ref-filter-history-list)))
         (terms (org-zettel-ref-filter--parse-query query))
         (filters '()))
    
    ;; Clear existing filters
    (setq org-zettel-ref-active-filters nil)
    
    ;; Process each term and create appropriate filters
    (dolist (term terms)
      (let ((column-spec (car term))
            (pattern (cdr term))
            (is-negated (string-prefix-p "-" (cdr term))))
        
        ;; Handle negated terms
        (when is-negated
          (setq pattern (substring pattern 1)))
        
        (if column-spec
            ;; Column-specific filter
            (let ((column-idx (cdr (assoc column-spec columns))))
              (if column-idx
                  (push (cons column-idx
                              (if is-negated
                                  (lambda (entry)
                                    (let ((value (aref (cadr entry) column-idx)))
                                      (not (and value (string-match-p pattern value)))))
                                (lambda (entry)
                                  (let ((value (aref (cadr entry) column-idx)))
                                    (and value (string-match-p pattern value))))))
                        filters)
                (message "Unknown column: %s" column-spec)))
          
          ;; Global search across all columns
          (push (cons 'global
                      (if is-negated
                          (lambda (entry)
                            (let ((values (cadr entry))
                                  (found nil))
                              (dotimes (i (length values))
                                (when (and (aref values i)
                                           (string-match-p pattern (aref values i)))
                                  (setq found t)))
                              (not found)))
                        (lambda (entry)
                          (let ((values (cadr entry))
                                (found nil))
                            (dotimes (i (length values))
                              (when (and (aref values i)
                                         (string-match-p pattern (aref values i)))
                                (setq found t)))
                            found))))
                filters))))
    
    ;; Apply all filters
    (dolist (filter filters)
      (if (eq (car filter) 'global)
          ;; Global filter (special case)
          (push (cons 'global (cdr filter)) org-zettel-ref-active-filters)
        ;; Column-specific filter
        (push filter org-zettel-ref-active-filters)))
    
    ;; Save to history
    (add-to-history 'org-zettel-ref-filter-history-list query)
    (org-zettel-ref-filter-save-history)
    
    ;; Refresh display
    (org-zettel-ref-list-refresh)
    
    ;; Show feedback
    (message "Applied filter: %s" query)))

(defun org-zettel-ref-filter--parse-query (query)
  "Parse QUERY string into a list of search terms.
Returns a list of (column . pattern) pairs, where column is nil for global search."
  (let ((terms '())
        (current-pos 0)
        (query-length (length query)))
    
    (while (< current-pos query-length)
      (let ((column nil)
            (pattern nil)
            (quoted nil))
        
        ;; Skip whitespace
        (while (and (< current-pos query-length)
                    (string-match-p "\\s-" (substring query current-pos (1+ current-pos))))
          (setq current-pos (1+ current-pos)))
        
        (when (< current-pos query-length)
          ;; Check for column specification (e.g., "title:")
          (let ((colon-pos (string-match ":" query current-pos)))
            (when (and colon-pos
                       (< colon-pos query-length)
                       (not (string-match-p "\\s-" (substring query current-pos colon-pos))))
              (setq column (substring query current-pos colon-pos))
              (setq current-pos (1+ colon-pos))))
          
          ;; Check for quoted string
          (when (and (< current-pos query-length)
                     (string= (substring query current-pos (1+ current-pos)) "\""))
            (setq quoted t)
            (setq current-pos (1+ current-pos))
            (let ((end-quote (string-match "\"" query current-pos)))
              (if end-quote
                  (progn
                    (setq pattern (substring query current-pos end-quote))
                    (setq current-pos (1+ end-quote)))
                (setq pattern (substring query current-pos))
                (setq current-pos query-length))))
          
          ;; Non-quoted string (ends at next whitespace)
          (unless quoted
            (let ((space-pos (string-match "\\s-" query current-pos)))
              (if space-pos
                  (progn
                    (setq pattern (substring query current-pos space-pos))
                    (setq current-pos space-pos))
                (setq pattern (substring query current-pos))
                (setq current-pos query-length))))
          
          ;; Add the term if we found a pattern
          (when (and pattern (not (string-empty-p pattern)))
            (push (cons column pattern) terms)))))
    
    (nreverse terms)))

;; Override the apply-filters function to handle global filters
(defun org-zettel-ref-apply-filters (entries)
  "Apply filters to ENTRIES.
Handles both column-specific and global filters."
  (if (null org-zettel-ref-active-filters)
      entries
    (cl-remove-if-not
     (lambda (entry)
       (cl-every
        (lambda (filter)
          (if (eq (car filter) 'global)
              ;; Global filter
              (funcall (cdr filter) entry)
            ;; Column-specific filter
            (funcall (cdr filter) entry)))
        org-zettel-ref-active-filters))
     entries)))

;; Update keybindings for filter functions
;; 移除直接的键绑定定义，避免循环依赖

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
