;;; org-zettel-ref-list.el --- Reading management interface for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides a reading management interface for org-zettel-ref-mode.
;; It manages reading states and integrates with the database system
;; provided by org-zettel-ref-db.el.

;;; Code:

(require 'org-zettel-ref-list-filter)
(require 'org-zettel-ref-db)
(require 'transient)
(require 'cl-lib)  

;;;----------------------------------------------------------------------------
;;; Variables
;;;----------------------------------------------------------------------------

(defcustom org-zettel-ref-directory  (expand-file-name "~/Documents/ref/")
  "File path for the reference directory.")

(defvar org-zettel-ref-filter-presets-file
  (expand-file-name "org-zettel-ref-filters.el" org-zettel-ref-directory)
  "File path for storing filter presets.")

(defvar org-zettel-ref-list-mode-map (make-sparse-keymap)
  "Keymap for org-zettel-ref reading mode.")

(defgroup org-zettel-ref-list nil
  "Customization group for org-zettel-ref reading manager."
  :group 'org-zettel-ref)

(defun org-zettel-ref-ensure-directory ()
  "Ensure the reference directory exists."
  (unless (file-exists-p org-zettel-ref-directory)
    (make-directory org-zettel-ref-directory t))
  org-zettel-ref-directory)

;;;----------------------------------------------------------------------------
;;; Mode Definition
;;;---------------------------------------------------------------------------- 

(define-derived-mode org-zettel-ref-list-mode tabulated-list-mode "Zettel-Ref"
  "Major mode for displaying and managing Zettel references."
  (setq tabulated-list-format
        [("Title" 60 t)
         ("Author" 20 t)
         ("Status" 10 t)
         ("Rating" 20 t)
         ("Modified" 20 t)
         ("Keywords" 30 t)])
  (setq tabulated-list-padding 2)  
  (setq tabulated-list-sort-key (cons "Modified" t))
  (setq org-zettel-ref-list-multi-sort-keys (list (cons "Modified" t)))
  (make-local-variable 'mouse-face-highlight-property)
  (setq mouse-face-highlight-property 'highlight)
  (when (fboundp 'org-zettel-ref-filter-setup-keybindings)
    (org-zettel-ref-filter-setup-keybindings org-zettel-ref-list-mode-map))
  (tabulated-list-init-header)
  (org-zettel-ref-list-setup-sort-keybindings org-zettel-ref-list-mode-map))

(defun org-zettel-ref-list-setup-sort-keybindings (mode-map)
  "Set up sort keybindings in MODE-MAP."
  (define-key mode-map (kbd "S") 'org-zettel-ref-list-sort-by-column)
  (define-key mode-map (kbd "M-S") 'org-zettel-ref-list-add-sort-column)
  (define-key mode-map (kbd "C-c C-s c") 'org-zettel-ref-list-clear-sort)
  (define-key mode-map (kbd "C-c C-s s") 'org-zettel-ref-list-save-sort-config)
  (define-key mode-map (kbd "C-c C-s l") 'org-zettel-ref-list-load-sort-config)
  (define-key mode-map (kbd "?") 'org-zettel-ref-list-help))

;;;----------------------------------------------------------------------------
;;; Sorting Functions
;;;----------------------------------------------------------------------------

(defvar org-zettel-ref-list-multi-sort-keys nil
  "List of sort keys for multi-column sorting.
Each element is a cons cell (COLUMN . FLIP) where COLUMN is either
a column name or index, and FLIP is a boolean indicating whether
to reverse the sort order.")

(defvar org-zettel-ref-list-sort-history nil
  "History of saved sort configurations.")

(defun tabulated-list-get-sort-key ()
  "Get the current sort key from `tabulated-list-sort-key'.
Returns the column name as a string."
  (car tabulated-list-sort-key))
    
(defun tabulated-list-column-number (name)
  "Return the column number for NAME in `tabulated-list-format'."
  (let ((i 0)
        (len (length tabulated-list-format)))
    (while (and (< i len)
                (not (string= name (car (aref tabulated-list-format i)))))
      (setq i (1+ i)))
    (if (< i len) i
      (error "No column named %S" name))))

(defun tabulated-list-column-at (x)
  "Return the column name at position X."
  (let ((columns (mapcar 'car tabulated-list-format))
        (i 0)
        (current-x 0)
        found)
    (while (and (not found) (< i (length columns)))
      (let* ((col-name (nth i columns))
             (col-width (nth 1 (aref tabulated-list-format i))))
        (setq current-x (+ current-x col-width tabulated-list-padding))
        (when (< x current-x)
          (setq found col-name))
        (setq i (1+ i))))
    found))

(defun org-zettel-ref-list-ensure-valid-sort-keys ()
  "Ensure that sort keys are valid.
If tabulated-list-sort-key is nil or invalid, reset it to default (Modified, newest first)."
  (unless (and tabulated-list-sort-key 
               (car tabulated-list-sort-key)
               (stringp (car tabulated-list-sort-key)))

    (setq tabulated-list-sort-key (cons "Modified" t))
    (setq org-zettel-ref-list-multi-sort-keys (list (cons "Modified" t)))
    (setq tabulated-list-sort-key-function nil)
    (message "Reset invalid sort key to default (Modified, newest first)")))

(defun org-zettel-ref-list-sort-by-column (&optional column)
  "Sort the list by COLUMN.
If COLUMN is nil, use the column at point."
  (interactive)
  (org-zettel-ref-list-ensure-valid-sort-keys)
  (unless column
    (let ((x (current-column)))
      (setq column (tabulated-list-column-at x))))
  (if (null column)
    (message "No column at point")
    (setq tabulated-list-sort-key (cons column nil))
    (setq org-zettel-ref-list-multi-sort-keys (list (cons column nil)))
    (tabulated-list-sort)
    (goto-char (point-min))
    (message "Sorted by %s" column)))

(defun org-zettel-ref-list-add-sort-column (&optional _column)
  "Add a secondary sort column selected from a menu.
The optional _COLUMN argument is ignored, as we always prompt for column selection."
  (interactive)
  (let* ((columns (mapcar (lambda (col) (car col)) 
                         (append tabulated-list-format nil)))
         (current-sort-keys (mapcar #'car org-zettel-ref-list-multi-sort-keys))
         (available-columns
          (cl-remove-if (lambda (col)
                      (member col current-sort-keys))
                     columns)))
    
    (if (null available-columns)
        (message "No more columns available for sorting")
      (let ((column-name (completing-read "Add sort column: " available-columns nil t)))
        (when (and column-name (not (string-empty-p column-name)))
          (push (cons column-name nil) org-zettel-ref-list-multi-sort-keys)
          (org-zettel-ref-list-apply-multi-sort)
          (goto-char (point-min))
          (message "Added secondary sort by %s" column-name))))))

(defun org-zettel-ref-list-apply-multi-sort ()
  "Apply multi-column sorting based on org-zettel-ref-list-multi-sort-keys."
  (interactive)
  (if (null org-zettel-ref-list-multi-sort-keys)
      (message "No sort keys defined")
    (let ((primary-key (car org-zettel-ref-list-multi-sort-keys)))
      (if (or (null primary-key) (null (car primary-key)))
          (progn
            (message "Error: Invalid primary sort key: %s" primary-key)
            (setq org-zettel-ref-list-multi-sort-keys nil)
            (setq tabulated-list-sort-key (cons "Title" nil))
            (setq tabulated-list-sort-key-function nil))
        (setq tabulated-list-sort-key primary-key)
        (message "Setting primary sort key to: %s" tabulated-list-sort-key)
        (if (> (length org-zettel-ref-list-multi-sort-keys) 1)
            (setq tabulated-list-sort-key-function 
                  (lambda ()
                    (setq tabulated-list-sort-key primary-key)
                    org-zettel-ref-list-multi-sort-keys))
          (setq tabulated-list-sort-key-function nil))
        (tabulated-list-sort)
        (goto-char (point-min))
        (message "Applied multi-column sort with %d keys" 
                 (length org-zettel-ref-list-multi-sort-keys))))))

(defun org-zettel-ref-list--multi-column-sort-predicate (a b keys)
  "Compare entries A and B using multiple sort KEYS.
Each key in KEYS is a cons cell (COLUMN . FLIP) where COLUMN is the column
name and FLIP is non-nil if the sort order should be reversed."
  (let ((result 0)
        (remaining-keys keys))
    (while (and (= result 0) remaining-keys)
      (let* ((key (car remaining-keys))
             (column-name (car key))
             (flip (cdr key))
             (column-index (tabulated-list-column-number column-name))
             (a-val (aref (cadr a) column-index))
             (b-val (aref (cadr b) column-index)))
        ;; compare values
        (setq result (if (string-lessp a-val b-val) -1 
                       (if (string-lessp b-val a-val) 1 0)))
        ;; if need to reverse sort order
        (when flip
          (setq result (- result)))
        (setq remaining-keys (cdr remaining-keys))))
    result))

(defun org-zettel-ref-list-clear-sort ()
  "Clear all sort keys and reset to default sorting."
  (interactive)
  (setq org-zettel-ref-list-multi-sort-keys nil)
  (setq tabulated-list-sort-key-function nil)
  (setq tabulated-list-sort-key (cons "Modified" t))
  (tabulated-list-sort)
  (goto-char (point-min))
  (message "Reset to default sorting (by Modified, newest first)"))

(defun org-zettel-ref-list-save-sort-config (name)
  "Save current sort configuration with NAME."
  (interactive "sSort configuration name: ")
  (let ((config (cons name org-zettel-ref-list-multi-sort-keys)))
    (add-to-list 'org-zettel-ref-list-sort-history config)
    (message "Saved sort configuration: %s" name)))

(defun org-zettel-ref-list-load-sort-config ()
  "Load a saved sort configuration."
  (interactive)
  (if (null org-zettel-ref-list-sort-history)
      (message "No saved sort configurations")
    (let* ((choices (mapcar #'car org-zettel-ref-list-sort-history))
           (choice (completing-read "Load sort configuration: " choices nil t)))
      (when choice
        (setq org-zettel-ref-list-multi-sort-keys 
              (cdr (assoc choice org-zettel-ref-list-sort-history)))
        (org-zettel-ref-list-apply-multi-sort)
        (goto-char (point-min))))))

;; Add keybindings for sort functions
(define-key org-zettel-ref-list-mode-map (kbd "S") 'org-zettel-ref-list-sort-by-column)
(define-key org-zettel-ref-list-mode-map (kbd "M-S") 'org-zettel-ref-list-add-sort-column)
(define-key org-zettel-ref-list-mode-map (kbd "C-c C-s c") 'org-zettel-ref-list-clear-sort)
(define-key org-zettel-ref-list-mode-map (kbd "C-c C-s s") 'org-zettel-ref-list-save-sort-config)
(define-key org-zettel-ref-list-mode-map (kbd "C-c C-s l") 'org-zettel-ref-list-load-sort-config)
(define-key org-zettel-ref-list-mode-map (kbd "TAB") 'org-zettel-ref-list-goto-column)

;;;----------------------------------------------------------------------------
;;; File Name Parsing and Formatting
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-update-overview-source-file (db ref-id old-file new-file)
  "Update SOURCE_FILE reference in overview file when source file is renamed.
DB is the database object, REF-ID is the reference ID, OLD-FILE is the old file path,
NEW-FILE is the new file path."
  (when (and (not org-zettel-ref-use-single-overview-file))
    (let ((overview-entry (org-zettel-ref-db-get-overview-by-ref-id db ref-id)))
      (when (and overview-entry 
                (org-zettel-ref-overview-entry-file-path overview-entry)
                (file-exists-p (org-zettel-ref-overview-entry-file-path overview-entry)))
        (condition-case update-err
            (with-temp-buffer
              (insert-file-contents (org-zettel-ref-overview-entry-file-path overview-entry))
              ;; 更新overview文件中的:SOURCE_FILE:属性
              (goto-char (point-min))
              ;; 尝试多种可能的路径格式进行匹配和替换
              (let ((old-file-basename (file-name-nondirectory old-file))
                    (new-file-basename (file-name-nondirectory new-file))
                    (updated nil))
                ;; 尝试匹配绝对路径
                (when (re-search-forward (format ":SOURCE_FILE:[ \t]+%s" (regexp-quote old-file)) nil t)
                  (replace-match (format ":SOURCE_FILE: %s" new-file))
                  (setq updated t))
                ;; 尝试匹配相对路径（如果绝对路径匹配失败）
                (unless updated
                  (goto-char (point-min))
                  (when (re-search-forward (format ":SOURCE_FILE:[ \t]+%s" (regexp-quote old-file-basename)) nil t)
                    (replace-match (format ":SOURCE_FILE: %s" new-file-basename))
                    (setq updated t)))
                ;; 尝试匹配不带引号的路径
                (unless updated
                  (goto-char (point-min))
                  (when (re-search-forward (format ":SOURCE_FILE:[ \t]+%s" (regexp-quote (file-name-sans-extension old-file-basename))) nil t)
                    (replace-match (format ":SOURCE_FILE: %s" (file-name-sans-extension new-file-basename)))
                    (setq updated t)))
                (when updated
                  (write-file (org-zettel-ref-overview-entry-file-path overview-entry))
                  (message "DEBUG: Updated SOURCE_FILE reference in overview file"))
                (unless updated
                  (message "DEBUG: Could not find SOURCE_FILE reference to update in overview file"))))
          (error
           (message "Warning: Could not update SOURCE_FILE in overview: %s" 
                    (error-message-string update-err))))))))

(defconst org-zettel-ref-author-regexp "^\\(.*?\\)__"
  "Match the author part in the file name.") 

(defconst org-zettel-ref-title-regexp "\\(?:^\\|__\\)\\(.*?\\)\\(?:==\\|$\\)"
  "Match the title part in the file name.")

(defconst org-zettel-ref-keywords-regexp "==\\(.*?\\)\\(?:--\\|$\\|\\.[^.]*$\\)"
  "Match the keywords part in the file name.")

(defconst org-zettel-ref-status-rating-regexp "--\\([^-]+\\)-\\([0-5]\\)\\(?:\\.[^.]*\\)?$"
  "Match the status and rating part in the file name.")

(defun org-zettel-ref-clean-title-for-display (title)
  "Clean TITLE for display, removing status and rating suffixes.
  
  Parameters:
  - TITLE: Original title string
  
  Input: Title possibly containing --done-4 etc. suffixes
  Output: Cleaned title
  
  This function removes status and rating suffixes, like '--done-4', '--reading-2' etc.,
  to make list panel display cleaner titles."
  (when title
    ;; Remove status and rating suffixes (--status-rating format)
    (let ((clean-title (replace-regexp-in-string "--[^-]+-[0-5]$" "" title)))
      ;; Further clean possible other variants
      (setq clean-title (replace-regexp-in-string "--[^-]+$" "" clean-title))
      (string-trim clean-title))))

;; Parse file name
(defun org-zettel-ref-parse-filename (filename)
  "Parse FILENAME into a list of (author title keywords status rating)."
  (let (author title keywords status rating)
    ;; Remove .org extension
    (setq filename (file-name-sans-extension filename))
    
    ;; Parse author using regexp
    (when (string-match org-zettel-ref-author-regexp filename)
      (setq author (match-string 1 filename)))
    
    ;; Parse title 
    (if author
        (when (string-match "__\\([^=]+\\)" filename)
          (setq title (match-string 1 filename)))
      (when (string-match "^\\([^=]+\\)" filename)
        (setq title (match-string 1 filename))))
    
    ;; Parse keywords
    (when (string-match org-zettel-ref-keywords-regexp filename)
      (setq keywords (split-string (match-string 1 filename) "_")))

    ;; Parse status and rating - find the last matching status and rating
    (let ((start 0)
          (last-status nil)
          (last-rating nil))
      (while (string-match "--\\([^-]+\\)-\\([0-5]\\)" filename start)
        (setq last-status (intern (match-string 1 filename))
              last-rating (string-to-number (match-string 2 filename))
              start (match-end 0)))
      (setq status (or last-status 'unread)
            rating (or last-rating 0)))
    
    ;; Trim all parts
    (setq author (when author (string-trim author))
          title (when title (string-trim title))
          keywords (when keywords (mapcar #'string-trim keywords)))
    
    (list author title keywords status rating)))

;; Format file name
(defun org-zettel-ref-format-filename (author title keywords &optional status rating)
  "Generate a standard file name: AUTHOR__TITLE==KEYWORDS--STATUS-RATING.org
For example: Stallman__GNUEmacs==editor_lisp--reading-3.org"
  (let* ((base-name (concat 
                    (when author (concat author "__"))
                    title
                    (when keywords (concat "==" (string-join keywords "_")))))
         ;; Remove any existing status and rating markers
         (clean-name (replace-regexp-in-string "--[^-]+-[0-5]\\(?:\\.[^.]*\\)?$" "" base-name)))
    (concat clean-name
            (when (or status rating)
              (concat "--" (symbol-name (or status 'unread)) "-" 
                      (number-to-string (or rating 0))))
            ".org")))

;; Get file modified time
(defun org-zettel-ref-get-modified-time (file)
  "Get the modified time of FILE."
  (format-time-string "%Y-%m-%d %H:%M:%S"
                     (nth 5 (file-attributes file))))

;; Parse file info
(defun org-zettel-ref-parse-file (file)
  "Parse FILE to extract basic information."
  (let* ((filename (file-name-nondirectory file))
         (parsed (org-zettel-ref-parse-filename filename)))
    (list :title (nth 1 parsed)
          :author (nth 0 parsed)
          :keywords (nth 2 parsed))))

;;;----------------------------------------------------------------------------
;;; Display Interface
;;;----------------------------------------------------------------------------

;; Refresh list

(defun org-zettel-ref-list-refresh ()
  "Refresh the reference list display."
  (interactive)
  (when (eq major-mode 'org-zettel-ref-list-mode)
    (let* ((marked-files org-zettel-ref-marked-files)
           (current-pos (point))
           (inhibit-read-only t))
      (org-zettel-ref-list-ensure-valid-sort-keys)
      ;; clear all overlays
      (when (boundp 'org-zettel-ref-mark-overlays)
        (dolist (ov org-zettel-ref-mark-overlays)
          (delete-overlay ov))
        (setq org-zettel-ref-mark-overlays nil))
      ;; clear buffer
      (erase-buffer)
      (setq tabulated-list-entries (org-zettel-ref-list--get-entries))
      ;; apply filters
      (when (and (boundp 'org-zettel-ref-active-filters)
                 org-zettel-ref-active-filters)
        (setq tabulated-list-entries
              (org-zettel-ref-apply-filters tabulated-list-entries)))
       ;; redisplay
      (tabulated-list-print t)
        (when (and (boundp 'org-zettel-ref-marked-files)
                 marked-files)
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((file (org-zettel-ref-list-get-file-at-point)))
              (when (member file marked-files)
                (let* ((beg (line-beginning-position))
                       (end (1+ (line-end-position)))
                       (ov (make-overlay beg end)))
                  (overlay-put ov 'face 'org-zettel-ref-marked-face)
                  (overlay-put ov 'org-zettel-ref-marked t)
                  (push ov org-zettel-ref-mark-overlays))))
            (forward-line 1))))
       (if (< current-pos (point-max))
          (goto-char current-pos)
        (goto-char (point-min))))))

(defun org-zettel-ref-column-author ()
  "Author column definition."
  (list "Author" 15 t))

(defun org-zettel-ref-column-title ()
  "Title column definition."
  (list "Title" 50 t))

(defun org-zettel-ref-column-status ()
  "Status column definition."
  (list "Status" 10 t))

(defun org-zettel-ref-column-rating ()
  "Rating column definition."
  (list "Rating" 10 t))

(defun org-zettel-ref-column-modified ()
  "Modified time column definition."
  (list "Modified" 20 t))

(defun org-zettel-ref-column-keywords ()
  "Keywords column definition."
  (list "Keywords" 30 t))

(defun org-zettel-ref-list-columns ()
  "Return a list of column definitions."
  (list (org-zettel-ref-column-title)
        (org-zettel-ref-column-author)
        (org-zettel-ref-column-status)
        (org-zettel-ref-column-rating)
        (org-zettel-ref-column-modified)
        (org-zettel-ref-column-keywords)))

;;;----------------------------------------------------------------------------
;;; Display Content: Entry Formatting
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-list--format-entry (id entry)
  "Format entry for table list display, using cleaned title.
  
  Parameters:
  - ID: Entry identifier
  - ENTRY: org-zettel-ref-entry struct
  
  Input: Entry ID and entry data
  Output: Formatted vector for table display
  
  This function uses information stored in the database, rather than re-parsing the file name,
  to display clean titles without status and rating suffixes."
  (if (null entry)
      (progn 
        (message "Warning: Nil entry for id %s" id)
        (make-vector (length (org-zettel-ref-list-columns)) ""))
    (let* ((file-path (org-zettel-ref-ref-entry-file-path entry))
           ;; Use information stored in the database, rather than re-parsing the file name
           (raw-title (or (org-zettel-ref-ref-entry-title entry)
                         (file-name-base file-path)))
           ;; Further clean title to ensure no suffix
           (clean-title (org-zettel-ref-clean-title-for-display raw-title))
           (title-with-props  ; Add file path property to title
            (propertize (or clean-title "Untitled")
                       'file-path file-path
                       'help-echo file-path))
           (status-str (pcase (org-zettel-ref-ref-entry-read-status entry)
                        ('unread "⚪")
                        ('reading "🔵")
                        ('done "✅")
                        (_ "⚪")))
           (rating (org-zettel-ref-ref-entry-rating entry))
           (rating-str (make-string (or rating 0) ?⭐)))
      (vector
       title-with-props      ; Title (with file path property)
       (or (org-zettel-ref-ref-entry-author entry) "")
       status-str           ; Status
       rating-str          ; Rating
       (format-time-string "%Y-%m-%d %H:%M:%S"
                          (org-zettel-ref-ref-entry-modified entry))
       (if-let* ((keywords (org-zettel-ref-ref-entry-keywords entry)))
           (string-join keywords ", ")
         "")))))

(defun org-zettel-ref-list--get-entries ()
  "Get entries for table list display, using cleaned title.
  
  Input: None
  Output: Formatted entry list
  
  This function uses information stored in the database, rather than re-parsing the file name,
  to display clean titles without status and rating suffixes."
  (let ((entries '())
        (db (org-zettel-ref-ensure-db)))
    (maphash
     (lambda (id entry)
       (when-let* ((file-path (org-zettel-ref-ref-entry-file-path entry))
                   (exists (file-exists-p file-path)))
         ;; Use information stored in the database, rather than re-parsing the file name
         (let* ((raw-title (org-zettel-ref-ref-entry-title entry))
                (clean-title (org-zettel-ref-clean-title-for-display raw-title))
                (author (org-zettel-ref-ref-entry-author entry))
                (keywords (org-zettel-ref-ref-entry-keywords entry))
                (status (org-zettel-ref-ref-entry-read-status entry))
                (rating (org-zettel-ref-ref-entry-rating entry))
                ;; If database information is incomplete, parse the file name as a fallback
                (parsed-info (when (or (not raw-title) (not author))
                              (org-zettel-ref-parse-filename 
                               (file-name-nondirectory file-path))))
                ;; Use database information or parsed results
                (final-title (or clean-title 
                               (org-zettel-ref-clean-title-for-display (nth 1 parsed-info))
                               "Untitled"))
                (final-author (or author (nth 0 parsed-info) ""))
                (final-keywords (or keywords (nth 2 parsed-info) '()))
                (final-status (or status 
                                (nth 3 parsed-info) 
                                'unread))
                (final-rating (or rating 
                                (nth 4 parsed-info) 
                                0))
                ;; Convert status to display string (unicode symbols)
                (status-str (pcase final-status
                            ('unread "⚪")
                            ('reading "🔵")
                            ('done "✅")
                            (_ "⚪")))
                ;; Convert rating to stars
                (rating-str (make-string final-rating ?⭐)))
           (push (list file-path
                      (vector
                       ;; Title column - use cleaned title
                       (propertize 
                        final-title
                        'file-path file-path
                        'help-echo file-path)
                       ;; Author column 
                       (propertize 
                        final-author
                        'help-echo (format "Author: %s" (or final-author "Unknown")))
                       ;; Status column
                       status-str
                       ;; Rating column
                       rating-str
                       ;; Modified time column
                       (format-time-string "%Y-%m-%d %H:%M:%S"
                                         (org-zettel-ref-ref-entry-modified entry))
                       ;; Keywords column
                       (string-join final-keywords ", ")))
                 entries))))
     (org-zettel-ref-db-refs db))
    (org-zettel-ref-debug-message-category 'core 
      "Retrieved %d entries" (length entries))
    (nreverse entries)))

(defun org-zettel-ref-ref-entry-p (obj)
  "Check if OBJ is a valid ref-entry struct."
  (and (recordp obj)
       (eq (type-of obj) 'org-zettel-ref-ref-entry)))




;;;----------------------------------------------------------------------------
;;; List Operation: Mark
;;;---------------------------------------------------------------------------- 

(defvar-local org-zettel-ref-marked-files nil
  "List of files marked for deletion.")

(defvar-local org-zettel-ref-mark-overlays nil
  "List of overlays for marked lines.")

(defface org-zettel-ref-marked-face
  '((t :inherit warning :extend t))
  "Highlight style for marked files.")

(defun org-zettel-ref-mark-file ()
  "Mark the current file for deletion."
  (interactive)
  (when-let* ((file (org-zettel-ref-list-get-file-at-point))
              (beg (line-beginning-position))
              (end (1+ (line-end-position))))
    ;; Create highlight overlay
    (let ((ov (make-overlay beg end)))
      (overlay-put ov 'face 'org-zettel-ref-marked-face)
      (overlay-put ov 'org-zettel-ref-marked t)
      (push ov org-zettel-ref-mark-overlays)
      (push file org-zettel-ref-marked-files))
    (forward-line 1)
    (message "Marked %s for deletion" file)))

(defun org-zettel-ref-unmark-file ()
  "Unmark the current file."
  (interactive)
  (when-let* ((file (org-zettel-ref-list-get-file-at-point)))
    ;; Remove current line overlay
    (let ((overlays (overlays-in (line-beginning-position)
                                (1+ (line-end-position)))))
      (dolist (ov overlays)
        (when (overlay-get ov 'org-zettel-ref-marked)
          (setq org-zettel-ref-mark-overlays
                (delq ov org-zettel-ref-mark-overlays))
          (delete-overlay ov))))
    ;; Remove file from marked list
    (setq org-zettel-ref-marked-files 
          (delete file org-zettel-ref-marked-files))
    (forward-line 1)
    (message "Unmarked %s" file)))

(defun org-zettel-ref-unmark-all ()
  "Unmark all files."
  (interactive)
  ;; Clear all overlays
  (dolist (ov org-zettel-ref-mark-overlays)
    (delete-overlay ov))
  (setq org-zettel-ref-mark-overlays nil
        org-zettel-ref-marked-files nil)
  (message "Unmarked all files"))

;;;-------------------------------------------------------------------------- 
;;; File Operation: Rename  
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-get-existing-authors (db)
  "Get a list of all existing authors from the database."
  (let ((authors '()))
    (maphash
     (lambda (id entry)
       (when-let* ((author (org-zettel-ref-ref-entry-author entry)))
         (push author authors)))
     (org-zettel-ref-db-refs db))
    (delete-dups authors)))

(defun org-zettel-ref-get-existing-titles (db)
  "Get a list of all existing titles from the database."
  (let ((titles '()))
    (maphash
     (lambda (id entry)
       (when-let* ((title (org-zettel-ref-ref-entry-title entry)))
         (push title titles)))
     (org-zettel-ref-db-refs db))
    (delete-dups titles)))

(defun org-zettel-ref-get-existing-keywords (db)
  "Get a list of all existing keywords from the database."
  (let ((keywords '()))
    (maphash
     (lambda (id entry)
       (when-let* ((entry-keywords (org-zettel-ref-ref-entry-keywords entry)))
         (setq keywords (append keywords entry-keywords))))
     (org-zettel-ref-db-refs db))
    (delete-dups keywords)))

(defun org-zettel-ref-rename--prompt-title (current-title)
  "Prompt for title input with completion, CURRENT-TITLE is the current title."
  (let* ((db (org-zettel-ref-ensure-db))
         (existing-titles (org-zettel-ref-get-existing-titles db))
         (prompt (format "Title%s: "
                        (if current-title
                            (format " (current: %s)" current-title)
                          ""))))
    (completing-read prompt existing-titles
                    nil nil nil nil current-title)))

(defun org-zettel-ref-rename--prompt-keywords (current-keywords)
  "Prompt for keywords input with completion, CURRENT-KEYWORDS is the current keywords."
  (let* ((db (org-zettel-ref-ensure-db))
         (existing-keywords (org-zettel-ref-get-existing-keywords db))
         (current (if current-keywords
                     (string-join current-keywords ", ")
                   ""))
         (input (completing-read-multiple
                (format "Keywords%s (comma-separated): "
                        (if current
                            (format " (current: %s)" current)
                          ""))
                existing-keywords
                nil nil current)))
    (if (stringp input)
        (split-string input "[,\s]+" t)
      input)))

(defun org-zettel-ref-rename--prompt-author (current-author)
  "Prompt for author input with completion, CURRENT-AUTHOR is the current author."
  (let* ((db (org-zettel-ref-ensure-db))
         (existing-authors (org-zettel-ref-get-existing-authors db))
         (prompt (format "Author%s: "
                        (if current-author
                            (format " (current: %s)" current-author)
                          ""))))
    (completing-read prompt existing-authors
                    nil nil nil nil current-author)))

(defun org-zettel-ref-rename--prompt-title (current-title)
  "Prompt for title input, CURRENT-TITLE is the current title."
  (read-string 
   (format "Title%s: "
           (if current-title
               (format " (current: %s)" current-title)
             ""))
   current-title))

(defun org-zettel-ref-rename--prompt-keywords (current-keywords)
  "Prompt for keywords input, CURRENT-KEYWORDS is the current keywords."
  (let ((current (if current-keywords
                    (string-join current-keywords ", ")
                  "")))
    (split-string
     (read-string 
      (format "Keywords%s (comma-separated): "
              (if current
                  (format " (current: %s)" current)
                ""))
      current)
     "[,\s]+" t)))

(defun org-zettel-ref-list-rename-file ()
  "Rename the file at point."
  (interactive)
  (let* ((db (org-zettel-ref-ensure-db))
         (old-file (org-zettel-ref-list-get-file-at-point)))
    (if (not old-file)
        (message "No file selected")
      (if (and org-zettel-ref-use-single-overview-file
               (string= (expand-file-name old-file)
                        (expand-file-name org-zettel-ref-single-overview-file-path)))
          (message "The single overview file cannot be renamed.")
        ;; Original logic for renaming
        (let* ((dir (file-name-directory old-file))
               (ref-id (when old-file (org-zettel-ref-db-get-ref-id-by-path db old-file))))
          (if (not ref-id)
              (message "Error: Cannot find database entry for file: %s" old-file)
            (let* ((ref-entry (org-zettel-ref-db-get-ref-entry db ref-id))
                   (current-author (org-zettel-ref-ref-entry-author ref-entry))
                   (current-title (org-zettel-ref-ref-entry-title ref-entry))
                   (current-keywords (org-zettel-ref-ref-entry-keywords ref-entry))
                   (current-status (org-zettel-ref-ref-entry-read-status ref-entry))
                   (current-rating (org-zettel-ref-ref-entry-rating ref-entry))
                   (new-author (org-zettel-ref-rename--prompt-author current-author))
                   (new-title (org-zettel-ref-rename--prompt-title current-title))
                   (new-keywords (org-zettel-ref-rename--prompt-keywords current-keywords))
                   (new-file-name (org-zettel-ref-format-filename
                                  new-author new-title new-keywords
                                  current-status current-rating))
                   (new-file-path (expand-file-name new-file-name dir)))

              (when (and (not (equal old-file new-file-path))
                        (y-or-n-p (format "Rename %s to %s? "
                                        (file-name-nondirectory old-file)
                                        (file-name-nondirectory new-file-path))))
                ;; Suspend file monitoring
                (org-zettel-ref-unwatch-directory)

                (condition-case err
                    (progn
                      ;; Rename file
                      (rename-file old-file new-file-path t)
                      ;; Update database
                      (when-let* ((ref-id (org-zettel-ref-db-get-ref-id-by-path db old-file)) ; Re-fetch ref-id just in case, though it should be same
                                (ref-entry (org-zettel-ref-db-get-ref-entry db ref-id)))
                        ;; Get existing overview mapping information to ensure it remains unchanged
                        (let ((existing-overview-id (org-zettel-ref-db-get-maps db ref-id))
                              (existing-overview-entry (org-zettel-ref-db-get-overview-by-ref-id db ref-id)))
                          (message "DEBUG: Renaming file - REF_ID: %s, Overview ID: %s" 
                                   ref-id existing-overview-id)
                          (when existing-overview-entry
                            (message "DEBUG: Existing overview file: %s" 
                                     (org-zettel-ref-overview-entry-file-path existing-overview-entry)))
                          
                          ;; Update file path mapping
                          (remhash old-file (org-zettel-ref-db-ref-paths db))
                          (puthash new-file-path ref-id (org-zettel-ref-db-ref-paths db))
                          
                          ;; Update reference entry information (but keep ref-id unchanged)
                          (setf (org-zettel-ref-ref-entry-file-path ref-entry) new-file-path
                                (org-zettel-ref-ref-entry-title ref-entry) new-title
                                (org-zettel-ref-ref-entry-author ref-entry) new-author
                                (org-zettel-ref-ref-entry-keywords ref-entry) new-keywords)
                          (org-zettel-ref-db-update-ref-entry db ref-entry)
                          
                          ;; Ensure overview mapping relationship remains unchanged
                          ;; No need to change overview mapping, because ref-id didn't change
                          (when existing-overview-id
                            (message "DEBUG: Overview mapping preserved - REF_ID: %s -> Overview ID: %s" 
                                     ref-id existing-overview-id))
                          
                          ;; If overview file exists, update the SOURCE_FILE reference in its content
                          (org-zettel-ref-update-overview-source-file db ref-id old-file new-file-path)
                          
                          (org-zettel-ref-db-save db)))
                      ;; Update database
                      (when-let* ((ref-id (org-zettel-ref-db-get-ref-id-by-path db old-file)) ; Re-fetch ref-id just in case, though it should be same
                                (ref-entry (org-zettel-ref-db-get-ref-entry db ref-id)))
                        (remhash old-file (org-zettel-ref-db-ref-paths db))
                        (puthash new-file-path ref-id (org-zettel-ref-db-ref-paths db))
                        (setf (org-zettel-ref-ref-entry-file-path ref-entry) new-file-path
                              (org-zettel-ref-ref-entry-title ref-entry) new-title
                              (org-zettel-ref-ref-entry-author ref-entry) new-author
                              (org-zettel-ref-ref-entry-keywords ref-entry) new-keywords)
                        (org-zettel-ref-db-update-ref-entry db ref-entry)
                        (org-zettel-ref-db-save db))
                      ;; Update opened buffer
                      (when-let* ((buf (get-file-buffer old-file)))
                        (with-current-buffer buf
                          (set-visited-file-name new-file-path)
                          (set-buffer-modified-p nil)))
                      ;; Refresh display
                      (org-zettel-ref-list-refresh)
                      (message "File renamed from %s to %s"
                              (file-name-nondirectory old-file)
                              (file-name-nondirectory new-file-path)))
                  (error
                   (message "Error during rename: %s" (error-message-string err))))
                (org-zettel-ref-watch-directory)))))))))

(defun org-zettel-ref-list-goto-column ()
  "Prompt for a column and move cursor to it."
  (interactive)
  ;; get all column names
  (let* ((columns (mapcar (lambda (col) (car col)) 
                         (append tabulated-list-format nil)))
         ;; let user choose a column
         (column-name (completing-read "Go to column: " columns nil t)))
    ;; ensure the selected column is valid
    (if (or (null column-name) (string-empty-p column-name))
        (message "No column selected")
      ;; find the column position
      (let* ((column-index (tabulated-list-column-number column-name))
             (column-pos 0)
             (i 0))
        ;; calculate the column position
        (while (< i column-index)
          (let ((col-width (nth 1 (aref tabulated-list-format i))))
            (setq column-pos (+ column-pos col-width tabulated-list-padding))
            (setq i (1+ i))))
        ;; move to the column
        (beginning-of-line)
        (forward-char column-pos)
        (message "Moved to column %s" column-name)))))

;;;-------------------------------------------------------------------------- 
;;; File Operation: Edit Keywords 
;;;---------------------------------------------------------------------------- 

(defun org-zettel-ref-list-edit-keywords ()
  "Add or edit keywords for the file at point or marked files."
  (interactive)
  (let* ((db (org-zettel-ref-ensure-db))
         (marked-files org-zettel-ref-marked-files)
         (files (if (not (null marked-files))
                   marked-files
                 (list (org-zettel-ref-list-get-file-at-point)))))

    (dolist (file files)
      (if (and org-zettel-ref-use-single-overview-file
               (string= (expand-file-name file)
                        (expand-file-name org-zettel-ref-single-overview-file-path)))
          (message "Keywords are not applicable to the single overview file and it was skipped.")
        ;; Original logic for editing keywords for other files
        (let* ((filename (file-name-nondirectory file))
               (components (org-zettel-ref-parse-filename filename))
               ;; Keep the base part of the original file name (up to == or .org)
               (base-with-title (substring filename 0
                                         (or (string-match "==" filename)
                                             (- (length filename) 4)))) ; 减去.org
               (current-keywords (if (nth 2 components)
                                   (string-join (nth 2 components) ", ")
                                 ""))
               (prompt (if (string-empty-p current-keywords)
                          (format "Add keywords (comma-separated) for %s: " filename)
                        (format "Edit keywords (current: %s) for %s: " current-keywords filename)))
               (new-keywords-input (read-string prompt current-keywords)))

          (unless (string-empty-p new-keywords-input)
            (let* ((new-keywords-list (split-string new-keywords-input "[,\\s]+" t))
                   (new-keywords-str (string-join new-keywords-list "_"))
                   (new-filename (concat base-with-title
                                       "==" new-keywords-str
                                       ".org"))
                   (new-filepath (expand-file-name new-filename (file-name-directory file))))

              (when (and (not (equal file new-filepath))
                        (y-or-n-p (format "Rename file to %s?" new-filename)))
                ;; Rename file
                (condition-case err
                    (rename-file file new-filepath t)
                  (error
                   (message "Error renaming file: %s" (error-message-string err))
                   (signal (car err) (cdr err))))
                ;; Update database
                (when-let* ((ref-id (org-zettel-ref-db-get-ref-id-by-path db file))
                           (ref-entry (org-zettel-ref-db-get-ref-entry db ref-id)))
                  (remhash file (org-zettel-ref-db-ref-paths db))
                  (puthash new-filepath ref-id (org-zettel-ref-db-ref-paths db))
                  (setf (org-zettel-ref-ref-entry-file-path ref-entry) new-filepath
                        (org-zettel-ref-ref-entry-keywords ref-entry) new-keywords-list
                        (org-zettel-ref-ref-entry-modified ref-entry) (current-time))
                  
                  ;; Update overview file SOURCE_FILE reference if it exists
                  (org-zettel-ref-update-overview-source-file db ref-id file new-filepath)
                  
                  ;; Update opened buffer
                  (when-let* ((buf (get-file-buffer file)))
                    (with-current-buffer buf
                      (set-visited-file-name new-filepath)
                      (set-buffer-modified-p nil))))))))))
    ;; Save database and refresh display
    (org-zettel-ref-db-save db)
    (org-zettel-ref-list-refresh)
    (org-zettel-ref-unmark-all)
    (message "Keywords updated successfully"))


;;;-------------------------------------------------------------------------
;;; File Operation: Open
;;;--------------------------------------------------------------------------

(defun org-zettel-ref-list-open-file ()
  "Open the file at point."
  (interactive)
  (let ((file (org-zettel-ref-list-get-file-at-point)))
    (when file
      (find-file file))))


;;;-------------------------------------------------------------------------- 
;;; File Operation: Delete  
;;;--------------------------------------------------------------------------

;; Helper function to get overview path safely
(defun org-zettel-ref-list--get-overview-file-path (db ref-id)
  "Get the overview file path for REF-ID from DB."
  (when-let* ((overview-id (org-zettel-ref-db-get-maps db ref-id))
             (overview-entry (when overview-id (gethash overview-id (org-zettel-ref-db-overviews db)))))
    (when overview-entry (org-zettel-ref-overview-entry-file-path overview-entry))))

;; New helper function for deletion logic
(defun org-zettel-ref-list--delete-item (source-file db deletion-type)
  "Perform deletion based on DELETION-TYPE for SOURCE-FILE in DB.
DELETION-TYPE can be 'source, 'overview, or 'both.
Return a list indicating success/failure and type: (success-p type-deleted message).
Example: (t 'source \"Deleted source file only.\") or (nil 'error \"Error message\")"
  (let* ((ref-id (when source-file (org-zettel-ref-db-get-ref-id-by-path db source-file)))
         (overview-file (when ref-id (org-zettel-ref-list--get-overview-file-path db ref-id)))
         (overview-id (when ref-id (org-zettel-ref-db-get-maps db ref-id)))
         (source-deleted nil)
         (overview-deleted nil))

    (unless ref-id
      (return-from org-zettel-ref-list--delete-item (list nil 'error (format "No database entry for %s" source-file))))

    (condition-case err
        (progn
          ;; --- Delete Source File and DB entries ---
          (when (or (eq deletion-type 'source) (eq deletion-type 'both))
            (message "DEBUG: Deleting source file: %s" source-file)
            (delete-file source-file t) ; Use trash if available
            (remhash source-file (org-zettel-ref-db-ref-paths db))
            (remhash ref-id (org-zettel-ref-db-refs db))
            ;; Remove map only if source is gone AND overview still exists
            (when (and overview-id (not (eq deletion-type 'both)))
              (remhash ref-id (org-zettel-ref-db-map db)))
            (setq source-deleted t))

          ;; --- Delete Overview File and DB entries ---
          (when (and (or (eq deletion-type 'overview) (eq deletion-type 'both))
                     overview-id overview-file)
            (message "DEBUG: Deleting overview file: %s" overview-file)
            (when (file-exists-p overview-file) (delete-file overview-file t)) ; Use trash if available
            (remhash overview-file (org-zettel-ref-db-overview-paths db))
            (remhash overview-id (org-zettel-ref-db-overviews db))
            ;; Map entry definitely gone if overview is gone
            (remhash ref-id (org-zettel-ref-db-map db))
            (setq overview-deleted t))

          ;; --- Determine Result ---
          (cond
           ((and source-deleted overview-deleted)
            (list t 'both (format "Deleted source %s and overview %s" (file-name-nondirectory source-file) (when overview-file (file-name-nondirectory overview-file)))))
           (source-deleted
            (list t 'source (format "Deleted source file %s" (file-name-nondirectory source-file))))
           (overview-deleted
            (list t 'overview (format "Deleted overview file %s" (when overview-file (file-name-nondirectory overview-file)))))
           (t (list nil 'none "No files were deleted."))))

      ;; --- Error Handling ---
      (error
       (list nil 'error (format "Error during deletion for %s: %s" (file-name-nondirectory source-file) (error-message-string err)))))))


(defun org-zettel-ref-list-delete-file ()
  "Delete the source file, overview file, or both for the item at point."
  (interactive)
  (let* ((source-file (org-zettel-ref-list-get-file-at-point))
         (db (org-zettel-ref-ensure-db))
         (ref-id (when source-file (org-zettel-ref-db-get-ref-id-by-path db source-file)))
         (overview-file (when ref-id (org-zettel-ref-list--get-overview-file-path db ref-id)))
         (choices `(("Delete Source Only" . source)
                   ,@(when overview-file '(("Delete Overview Only" . overview))) ; Conditionally add overview option
                   ("Delete Both" . both)
                   ("Cancel" . cancel)))
         (prompt (format "Delete action for source '%s'%s? "
                        (file-name-nondirectory source-file)
                        (if overview-file (format " (overview '%s')" (file-name-nondirectory overview-file)) "")))
         (action (completing-read prompt (mapcar #'car choices) nil t nil nil (caar choices))))

    (when (and source-file ref-id) ; Ensure we have a valid item
      (let* ((type (cdr (assoc action choices))))
        (if (or (null type) (eq type 'cancel))
            (message "Deletion cancelled.")
          ;; Call the helper function
          (let ((result (org-zettel-ref-list--delete-item source-file db type)))
            (when (car result) ; If successful
              (org-zettel-ref-db-save db) ; Save DB after successful deletion
              (org-zettel-ref-list-refresh))
            ;; Display result message from helper
            (message (nth 2 result))))))))


(defun org-zettel-ref-list-delete-marked-files ()
  "Delete source, overview, or both for all marked files."
  (interactive)
  (let* ((files org-zettel-ref-marked-files)
         (file-count (length files))
         (db (org-zettel-ref-ensure-db))
         (deleted-source 0)
         (deleted-overview 0)
         (deleted-both 0)
         (errors 0)
         (choices `(("Delete Source Only" . source)
                   ("Delete Overview Only" . overview) ; Ask even if some don't have overviews
                   ("Delete Both" . both)
                   ("Cancel" . cancel)))
         action type)

    (unless files
      (message "No files marked for deletion.")
      (return-from org-zettel-ref-list-delete-marked-files))

    ;; Ask for action once before the loop
    (setq action (completing-read (format "Delete action for %d marked file%s? " file-count (if (= file-count 1) "" "s"))
                                 (mapcar #'car choices) nil t nil nil (caar choices)))
    (setq type (cdr (assoc action choices)))

    (if (or (null type) (eq type 'cancel))
        (message "Deletion cancelled.")
      ;; Process each marked file with the chosen action
      (dolist (file files)
        (let ((result (org-zettel-ref-list--delete-item file db type)))
          (cond
           ((not (car result)); Error occurred
            (message (nth 2 result))
            (cl-incf errors))
           ((eq (cadr result) 'source)
            (cl-incf deleted-source))
           ((eq (cadr result) 'overview)
            (cl-incf deleted-overview))
           ((eq (cadr result) 'both)
            (cl-incf deleted-both)))))

      ;; Save DB once if any changes were potentially made
      (when (> (+ deleted-source deleted-overview deleted-both errors) 0)
         (org-zettel-ref-db-save db))

      ;; Unmark and refresh
      (setq org-zettel-ref-marked-files nil)
      (org-zettel-ref-list-refresh)

      ;; Report final outcome
      (message "Deletion complete: %d source(s) only, %d overview(s) only, %d both, %d error(s)."
               deleted-source deleted-overview deleted-both errors))))


;;;----------------------------------------------------------------------------
;;; List Panel Operations
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-list-cycle-status ()
  "Cycle through reading status for the current entry."
  (interactive)
  (let* ((file (org-zettel-ref-list-get-file-at-point))
         (db (org-zettel-ref-ensure-db))
         (ref-id (when file (org-zettel-ref-db-get-ref-id-by-path db file)))
         (entry (when ref-id (org-zettel-ref-db-get-ref-entry db ref-id))))
    (when entry
      (if (and org-zettel-ref-use-single-overview-file
               (string= (expand-file-name file)
                        (expand-file-name org-zettel-ref-single-overview-file-path)))
          (message "Status is not applicable to the single overview file.")
        (let* ((old-status (org-zettel-ref-ref-entry-read-status entry))
               (new-status (pcase old-status
                            ('unread 'reading)
                            ('reading 'done)
                            ('done 'unread)
                            (_ 'unread)))
               ;; Get current file info
               (dir (file-name-directory file))
               (old-name (file-name-nondirectory file))
               (parsed-info (org-zettel-ref-parse-filename old-name))
               (author (nth 0 parsed-info))
               (title (nth 1 parsed-info))
               (keywords (nth 2 parsed-info))
               (rating (nth 4 parsed-info))
               ;; Generate new file name with updated status
               (new-name (org-zettel-ref-format-filename
                         author title keywords new-status rating))
               (new-file (expand-file-name new-name dir)))

          ;; Suspend file monitoring
          (org-zettel-ref-unwatch-directory)

          (condition-case err
              (progn
                ;; Rename file
                (rename-file file new-file t)
                ;; Update database entry
                (setf (org-zettel-ref-ref-entry-read-status entry) new-status
                      (org-zettel-ref-ref-entry-file-path entry) new-file)
                ;; Update database mappings
                (remhash file (org-zettel-ref-db-ref-paths db))
                (puthash new-file ref-id (org-zettel-ref-db-ref-paths db))
                
                ;; Update overview file SOURCE_FILE reference if it exists
                (org-zettel-ref-update-overview-source-file db ref-id file new-file)
                
                ;; Update any open buffer
                (when-let ((buf (get-file-buffer file)))
                  (with-current-buffer buf
                    (set-visited-file-name new-file)
                    (set-buffer-modified-p nil)))
                ;; Save changes
                (org-zettel-ref-db-save db)
                (org-zettel-ref-list-refresh)
                (message "Reading status changed from %s to %s" old-status new-status))
            (error
             (message "Error updating status: %s" (error-message-string err))))

          ;; Resume file monitoring
          (org-zettel-ref-watch-directory))))))

(defun org-zettel-ref-list-set-rating (rating)
  "Set rating for the current entry.
RATING should be a number between 0 and 5."
  (interactive "nRating (0-5): ")
  (let* ((file (org-zettel-ref-list-get-file-at-point))
         (db (org-zettel-ref-ensure-db))
         (ref-id (when file (org-zettel-ref-db-get-ref-id-by-path db file)))
         (entry (when ref-id (org-zettel-ref-db-get-ref-entry db ref-id))))
    (when entry
      (if (and org-zettel-ref-use-single-overview-file
               (string= (expand-file-name file)
                        (expand-file-name org-zettel-ref-single-overview-file-path)))
          (message "Rating is not applicable to the single overview file.")
        (let* ((new-rating (max 0 (min 5 rating)))
               ;; Get current file info
               (dir (file-name-directory file))
               (old-name (file-name-nondirectory file))
               (parsed-info (org-zettel-ref-parse-filename old-name))
               (author (nth 0 parsed-info))
               (title (nth 1 parsed-info))
               (keywords (nth 2 parsed-info))
               (status (nth 3 parsed-info))
               ;; Generate new file name with updated rating
               (new-name (org-zettel-ref-format-filename
                         author title keywords status new-rating))
               (new-file (expand-file-name new-name dir)))

          ;; Suspend file monitoring
          (org-zettel-ref-unwatch-directory)

          (condition-case err
              (progn
                ;; Rename file
                (rename-file file new-file t)
                ;; Update database entry
                (setf (org-zettel-ref-ref-entry-rating entry) new-rating
                      (org-zettel-ref-ref-entry-file-path entry) new-file)
                ;; Update database mappings
                (remhash file (org-zettel-ref-db-ref-paths db))
                (puthash new-file ref-id (org-zettel-ref-db-ref-paths db))
                
                ;; Update overview file SOURCE_FILE reference if it exists
                (org-zettel-ref-update-overview-source-file db ref-id file new-file)
                
                ;; Update any open buffer
                (when-let ((buf (get-file-buffer file)))
                  (with-current-buffer buf
                    (set-visited-file-name new-file)
                    (set-buffer-modified-p nil)))
                ;; Save changes
                (org-zettel-ref-db-save db)
                (org-zettel-ref-list-refresh)
                (message "Rating set to %d stars" new-rating))
            (error
             (message "Error updating rating: %s" (error-message-string err))))

          ;; Resume file monitoring
          (org-zettel-ref-watch-directory))))))


;;;----------------------------------------------------------------------------
;;; Interactive Menu System
;;;----------------------------------------------------------------------------

(defvar org-zettel-ref-list-actions
  '(("open file" . org-zettel-ref-list-open-file)
    ("open overview" . org-zettel-ref-list-open-overview)
    ("rename file" . org-zettel-ref-list-rename-file)
    ("refresh list" . org-zettel-ref-list-refresh)
    ("delete file" . org-zettel-ref-list-delete-file)
    ("mark file" . org-zettel-ref-mark-file)
    ("unmark file" . org-zettel-ref-unmark-file)
    ("delete marked files" . org-zettel-ref-list-delete-marked-files)
    ("unmark all" . org-zettel-ref-unmark-all)
    ("filter by regexp" . org-zettel-ref-filter-by-regexp)
    ("clear filters" . org-zettel-ref-clear-all-filters)
    ("manage filter presets" . org-zettel-ref-filter-manage-presets)
    ("cycle status" . org-zettel-ref-list-cycle-status)
    ("set rating" . org-zettel-ref-list-set-rating)
    ("link overview" . org-zettel-ref-list-link-overview)
    ("show links" . org-zettel-ref-list-show-links)
    ("unlink overview" . org-zettel-ref-list-unlink-overview))
  "Available actions for reference list management.")

(defun org-zettel-ref-list-menu ()
  "Display reference list management actions in minibuffer."
  (interactive)
  (let* ((choices (mapcar #'car org-zettel-ref-list-actions))
         (choice (completing-read "Select action: " choices nil t))
         (action (cdr (assoc choice org-zettel-ref-list-actions))))
   (when action
      (call-interactively action))))

(defun org-zettel-ref-list-setup-sort-keybindings (mode-map)
  "Set up sort keybindings in MODE-MAP."
  (define-key mode-map (kbd "S") 'org-zettel-ref-list-sort-by-column)
  (define-key mode-map (kbd "M-S") 'org-zettel-ref-list-add-sort-column)
  (define-key mode-map (kbd "C-c C-s c") 'org-zettel-ref-list-clear-sort)
  (define-key mode-map (kbd "C-c C-s s") 'org-zettel-ref-list-save-sort-config)
  (define-key mode-map (kbd "C-c C-s l") 'org-zettel-ref-list-load-sort-config)
  (define-key mode-map (kbd "?") 'org-zettel-ref-list-help))      

;; Add menu key binding while keeping existing bindings
(define-key org-zettel-ref-list-mode-map (kbd "C-c C-m") #'org-zettel-ref-list-menu)
;; Keep existing key bindings
(define-key org-zettel-ref-list-mode-map (kbd "RET") #'org-zettel-ref-list-open-file)
(define-key org-zettel-ref-list-mode-map (kbd "o") #'org-zettel-ref-list-open-file)
(define-key org-zettel-ref-list-mode-map (kbd "r") #'org-zettel-ref-list-rename-file)
(define-key org-zettel-ref-list-mode-map (kbd "g") #'org-zettel-ref-list-refresh)
(define-key org-zettel-ref-list-mode-map (kbd "d") #'org-zettel-ref-list-delete-file)
(define-key org-zettel-ref-list-mode-map (kbd "k") #'org-zettel-ref-list-edit-keywords)
(define-key org-zettel-ref-list-mode-map (kbd "R") #'org-zettel-ref-list-cycle-status)
(define-key org-zettel-ref-list-mode-map (kbd "s") #'org-zettel-ref-list-set-rating)
(define-key org-zettel-ref-list-mode-map (kbd "v") #'org-zettel-ref-list-open-overview)
(define-key org-zettel-ref-list-mode-map (kbd "m") #'org-zettel-ref-mark-file)
(define-key org-zettel-ref-list-mode-map (kbd "u") #'org-zettel-ref-unmark-file)
(define-key org-zettel-ref-list-mode-map (kbd "D") #'org-zettel-ref-list-delete-marked-files)
(define-key org-zettel-ref-list-mode-map (kbd "x") #'org-zettel-ref-list-remove-db-entries)
(define-key org-zettel-ref-list-mode-map (kbd "U") #'org-zettel-ref-unmark-all)
(define-key org-zettel-ref-list-mode-map (kbd "L") #'org-zettel-ref-list-link-overview)
(define-key org-zettel-ref-list-mode-map (kbd "I") #'org-zettel-ref-list-show-links)
(define-key org-zettel-ref-list-mode-map (kbd "C-c C-u") #'org-zettel-ref-list-unlink-overview)
(define-key org-zettel-ref-list-mode-map (kbd "/") nil)  
(define-key org-zettel-ref-list-mode-map (kbd "/ r") #'org-zettel-ref-filter-by-regexp)
(define-key org-zettel-ref-list-mode-map (kbd "/ c") #'org-zettel-ref-clear-all-filters)
;(define-key org-zettel-ref-list-mode-map (kbd "/ p") #'org-zettel-ref-filter-manage-presets)
(define-key org-zettel-ref-list-mode-map (kbd "/ m") #'org-zettel-ref-filter-by-multiple-conditions)



;;;----------------------------------------------------------------------------
;;; org-zettel-ref-list
;;;----------------------------------------------------------------------------

(defvar org-zettel-ref-db-initialized nil
  "Flag indicating whether the database has been initialized.")

(defun org-zettel-ref-ensure-db-initialized ()
  "Ensure the database is initialized and loaded with current files."
  (let ((db (org-zettel-ref-ensure-db)))
    (unless org-zettel-ref-db-initialized
      (message "Initializing org-zettel-ref database...")
      (org-zettel-ref-scan-directory db)
      (setq org-zettel-ref-db-initialized t)
      (message "Database initialization completed"))))

(defun org-zettel-ref-list ()
  "Display reference list."
  (interactive)
  (let* ((db (org-zettel-ref-ensure-db))
         (buffer (get-buffer-create "*Org Zettel Ref List*")))
    
    ;; Only scan directory if database is empty or not initialized
    (when (or (not org-zettel-ref-db-initialized)
              (= 0 (hash-table-count (org-zettel-ref-db-refs db))))
      (message "Initializing database and scanning directory...")
      (org-zettel-ref-scan-directory db)
      (setq org-zettel-ref-db-initialized t))
    
    (with-current-buffer buffer
      (org-zettel-ref-list-mode)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (let ((entries (org-zettel-ref-list--get-entries)))
        (setq tabulated-list-entries entries)
        (tabulated-list-print t))
      (goto-char (point-min)))
    
    (org-zettel-ref-watch-directory)
    (switch-to-buffer buffer)
    buffer))

(defun org-zettel-ref-list-refresh ()
  "Refresh the reference list display."
  (interactive)
  (when (eq major-mode 'org-zettel-ref-list-mode)
    (let* ((marked-files org-zettel-ref-marked-files)
           (current-pos (point))
           (inhibit-read-only t))
      (org-zettel-ref-list-ensure-valid-sort-keys)
      ;; clear all overlays
      (when (boundp 'org-zettel-ref-mark-overlays)
        (dolist (ov org-zettel-ref-mark-overlays)
          (delete-overlay ov))
        (setq org-zettel-ref-mark-overlays nil))
      ;; clear buffer
      (erase-buffer)
      (setq tabulated-list-entries (org-zettel-ref-list--get-entries))
      ;; apply filters
      (when (and (boundp 'org-zettel-ref-active-filters)
                 org-zettel-ref-active-filters)
        (setq tabulated-list-entries
              (org-zettel-ref-apply-filters tabulated-list-entries)))
       ;; redisplay
      (tabulated-list-print t)
        (when (and (boundp 'org-zettel-ref-marked-files)
                 marked-files)
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((file (org-zettel-ref-list-get-file-at-point)))
              (when (member file marked-files)
                (let* ((beg (line-beginning-position))
                       (end (1+ (line-end-position)))
                       (ov (make-overlay beg end)))
                  (overlay-put ov 'face 'org-zettel-ref-marked-face)
                  (overlay-put ov 'org-zettel-ref-marked t)
                  (push ov org-zettel-ref-mark-overlays))))
            (forward-line 1))))
       (if (< current-pos (point-max))
          (goto-char current-pos)
        (goto-char (point-min))))))

(defun org-zettel-ref-handle-file-change (event)
  "Handle file change EVENT from file monitoring system."
  (when (and (bound-and-true-p org-zettel-ref-file-watch-descriptor)
             (buffer-live-p (get-buffer "*Org Zettel Ref List*")))
    (let ((event-type (nth 1 event))
          (file (nth 2 event)))
      (when (and file
                 (stringp file)
                 (string-match-p "\\.org$" file)
                 (not (string-match-p "^\\." (file-name-nondirectory file))))
        ;; Rescan the reference directory and update the database
        (let ((db (org-zettel-ref-ensure-db)))
          (org-zettel-ref-scan-directory db))
        ;; Delay refresh display
        (run-with-timer 
         0.5 nil
         (lambda ()
           (when (buffer-live-p (get-buffer "*Org Zettel Ref List*"))
             (with-current-buffer "*Org Zettel Ref List*"
               (when (eq major-mode 'org-zettel-ref-list-mode)
                 (org-zettel-ref-list-refresh)
                 (message "Refreshed due to file change: %s" 
                         (file-name-nondirectory file)))))))))))

;; Add command to force rescan of the reference directory and update the database
(defun org-zettel-ref-force-rescan ()
  "Force a rescan of the reference directory and update the database."
  (interactive)
  (when (yes-or-no-p "Force rescan of reference directory? ")
    (setq org-zettel-ref-db-initialized nil)
    (org-zettel-ref-ensure-db-initialized)
    (when (get-buffer "*Org Zettel Ref List*")
      (with-current-buffer "*Org Zettel Ref List*"
        (org-zettel-ref-list-refresh)))
    (message "Database rescan completed")))

;; Add helper function to get current item file path
(defun org-zettel-ref-list-get-file-at-point ()
  "Get the file path associated with the current position."
  (or (get-text-property (point) 'file-path)
      (tabulated-list-get-id)
      (error "No file associated with current position")))

;;;----------------------------------------------------------------------------
;;; File Operation: Scan Directory (add new ref files to db)
;;;---------------------------------------------------------------------------- 


(defun org-zettel-ref-scan-directory (db)
  "Scan the reference directory and add files to database.
Only create entries for new files and update entries for modified files.
DB is the database object."
  (let ((files (org-zettel-ref-find-ref-files))
        (new-count 0)
        (update-count 0)
        (skip-count 0)
        (added 0))
    (message "Found %d files to process" (length files))
    
    (dolist (file files)
      (let* ((file-path (expand-file-name file))
             (ref-id (org-zettel-ref-db-get-ref-id-by-path db file-path)))
        (if ref-id
            ;; Existing file - check if it needs update
            (let* ((entry (org-zettel-ref-db-get-ref-entry db ref-id))
                   (db-mtime (org-zettel-ref-ref-entry-modified entry))
                   (file-mtime (file-attribute-modification-time 
                              (file-attributes file-path))))
              (if (and db-mtime 
                      (not (time-less-p db-mtime file-mtime)))
                  (cl-incf skip-count)  ; File not modified - skip
                ;; File modified - update entry
                (let* ((file-name (file-name-nondirectory file))
                      (parsed-info (org-zettel-ref-parse-filename file-name))
                      (title (nth 1 parsed-info))
                      (author (nth 0 parsed-info))
                      (keywords (nth 2 parsed-info))
                      (status (or (nth 3 parsed-info) 'unread))
                      (rating (or (nth 4 parsed-info) 0)))
                  ;; Update entry fields but keep the ID
                  (setf (org-zettel-ref-ref-entry-title entry) title
                        (org-zettel-ref-ref-entry-author entry) author
                        (org-zettel-ref-ref-entry-keywords entry) keywords
                        (org-zettel-ref-ref-entry-read-status entry) status
                        (org-zettel-ref-ref-entry-rating entry) rating
                        (org-zettel-ref-ref-entry-modified entry) (current-time))
                  (cl-incf update-count))))
          ;; New file - create entry
          (let* ((file-name (file-name-nondirectory file))
                 (parsed-info (org-zettel-ref-parse-filename file-name))
                 (title (nth 1 parsed-info))
                 (author (nth 0 parsed-info))
                 (keywords (nth 2 parsed-info))
                 (status (or (nth 3 parsed-info) 'unread))
                 (rating (or (nth 4 parsed-info) 0))
                 (entry.id (org-zettel-ref-db-ensure-ref-entry 
                          db file-path title author keywords status rating)))
            (cl-incf new-count)
            (cl-incf added)
            
            ;; Save database every 100 entries
            (when (zerop (mod added 100))
              (message "Saving database after %d new entries..." added)
              (org-zettel-ref-db-save db))))))
    
    (message "Scan complete: %d new, %d updated, %d unchanged"
             new-count update-count skip-count)
    ;; Final save if there are any changes
    (when (or (> new-count 0) (> update-count 0))
      (org-zettel-ref-db-save db))))

;;;----------------------------------------------------------------------------
;;; File Operation: Remove Invalid Entries
;;;---------------------------------------------------------------------------- 

(defun org-zettel-ref-remove-entry (db id)
  "Remove entry with specified ID from database.
Return t if deletion is successful, nil if entry does not exist."
  (interactive
   (let* ((db (org-zettel-ref-ensure-db))
          (candidates
           (let (items)
             (maphash
              (lambda (id entry)
                (push (format "[%s] %s" 
                            id
                            (or (org-zettel-ref-ref-entry-title entry)
                                (file-name-nondirectory 
                                 (org-zettel-ref-ref-entry-file-path entry))))
                      items))
              (org-zettel-ref-db-refs db))
             (sort items #'string>)))
          (selection (completing-read "Select entry to delete: " candidates nil t)))
     (list db (substring selection 1 (string-match "]" selection)))))
  (when-let* ((entry (org-zettel-ref-db-get-ref-entry db id)))
    ;; Delete entry
    (remhash id (org-zettel-ref-db-refs db))
    ;; Save database
    (org-zettel-ref-db-save db)
    ;; Display result
    (when (called-interactively-p 'any)
      (message "Deleted entry: [%s] %s" 
               id 
               (or (org-zettel-ref-ref-entry-title entry)
                   (file-name-nondirectory 
                    (org-zettel-ref-ref-entry-file-path entry)))))
    t))

;; Batch delete function
(defun org-zettel-ref-remove-entries (db ids)
  "Remove multiple entries from database.
IDS is a list of entry IDs. Return number of deleted entries."
  (interactive
   (let* ((db (org-zettel-ref-ensure-db))
          (candidates
           (let (items)
             (maphash
              (lambda (id entry)
                (push (format "[%s] %s" 
                            id
                            (or (org-zettel-ref-ref-entry-title entry)
                                (file-name-nondirectory 
                                 (org-zettel-ref-ref-entry-file-path entry))))
                      items))
              (org-zettel-ref-db-refs db))
             (sort items #'string>)))
          (selections (completing-read-multiple 
                      "Select entries to delete (comma-separated): " 
                      candidates nil t)))
     (list db 
           (mapcar (lambda (s) 
                    (substring s 1 (string-match "]" s)))
                  selections))))
  (let ((removed 0))
    (dolist (id ids)
      (when-let* ((entry (org-zettel-ref-db-get-ref-entry db id)))
        ;; Delete entry
        (remhash id (org-zettel-ref-db-refs db))
        (cl-incf removed)
        
        ;; Display each deleted entry
        (when (called-interactively-p 'any)
          (message "Deleted entry: [%s] %s" 
                   id 
                   (or (org-zettel-ref-ref-entry-title entry)
                       (file-name-nondirectory 
                        (org-zettel-ref-ref-entry-file-path entry)))))
        ;; Save database
        (org-zettel-ref-db-save db)
        (when (called-interactively-p 'any)
          (message "Deleted %d entries" removed)))
    removed)))

;;;----------------------------------------------------------------------------
;;; Find ref files and parse file info
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-find-ref-files ()
  "Find all referenced files.
Return a list of file paths."
  (when (file-exists-p org-zettel-ref-directory)
    (directory-files org-zettel-ref-directory t "\\.org$")))

;;;----------------------------------------------------------------------------
;;; File Monitoring
;;;----------------------------------------------------------------------------

(defvar org-zettel-ref-file-watch-descriptor nil
  "File monitoring descriptor.")

(defun org-zettel-ref-watch-directory ()
  "Start monitoring changes in the reference file directory."
  (when (file-exists-p org-zettel-ref-directory)
    ;; Ensure no existing watch
    (org-zettel-ref-unwatch-directory)
    
    (condition-case err
        (let ((descriptor (file-notify-add-watch
                          org-zettel-ref-directory
                          '(change attribute-change)
                          #'org-zettel-ref-handle-file-change)))
          (setq-local org-zettel-ref-file-watch-descriptor descriptor)
          (message "Started monitoring directory: %s" org-zettel-ref-directory))
      (error
       (message "Error setting up file watch: %s" (error-message-string err))))))

(defun org-zettel-ref-unwatch-directory ()
  "Stop monitoring the reference file directory."
  (when (bound-and-true-p org-zettel-ref-file-watch-descriptor)
    (condition-case err
        (progn
          (file-notify-rm-watch org-zettel-ref-file-watch-descriptor)
          (setq-local org-zettel-ref-file-watch-descriptor nil)
          (message "Stopped monitoring directory"))
      (error
       (message "Error removing file watch: %s" (error-message-string err))
       (setq-local org-zettel-ref-file-watch-descriptor nil)))))

;;;--------------------------------------------------------------------------
;;; Database Entry Removal (Without File Deletion)
;;;--------------------------------------------------------------------------

(defun org-zettel-ref-list--remove-db-entry-only (source-file db)
  "Remove DB entries associated with SOURCE-FILE, leaving files intact.
Returns t if entry was found and removed, nil otherwise."
  (let* ((ref-id (when source-file (org-zettel-ref-db-get-ref-id-by-path db source-file)))
         (overview-file (when ref-id (org-zettel-ref-list--get-overview-file-path db ref-id)))
         (overview-id (when ref-id (org-zettel-ref-db-get-maps db ref-id))))

    (if (not ref-id)
        (progn
          (message "DEBUG: No DB entry found for %s" source-file)
          nil) ; Entry not found
      (progn
        (message "DEBUG: Removing DB entries for ref-id: %s (source: %s)" ref-id source-file)
        ;; Remove from refs and ref-paths (guaranteed to exist if ref-id exists)
        (remhash source-file (org-zettel-ref-db-ref-paths db))
        (remhash ref-id (org-zettel-ref-db-refs db))

        ;; Remove overview info if it exists
        (when overview-id
          (remhash overview-id (org-zettel-ref-db-overviews db)))
        (when overview-file
           (remhash overview-file (org-zettel-ref-db-overview-paths db)))
        ;; Remove map entry if it exists
        (when ref-id
          (remhash ref-id (org-zettel-ref-db-map db)))
        t)))) ; Indicate success

(defun org-zettel-ref-list-remove-db-entries ()
  "Remove database entries for the item at point or marked items.
This command does NOT delete the actual source or overview files."
  (interactive)
  (let* ((db (org-zettel-ref-ensure-db))
         (marked-files org-zettel-ref-marked-files)
         (target-files (if marked-files
                           marked-files
                         (list (org-zettel-ref-list-get-file-at-point))))
         (target-count (length target-files))
         (removed-count 0)
         (prompt-verb (if marked-files "marked items" "item at point")))

    (unless target-files
      (message "No item at point or marked.")
      (return-from org-zettel-ref-list-remove-db-entries))

    (when (yes-or-no-p (format "Remove %d database entr%s for %s (files will NOT be deleted)? "
                               target-count
                               (if (= target-count 1) "y" "ies")
                               prompt-verb))
      (dolist (file target-files)
        (if (org-zettel-ref-list--remove-db-entry-only file db)
            (cl-incf removed-count)
          (message "Warning: Could not find or remove DB entry for %s" (file-name-nondirectory file))))

      (when (> removed-count 0)
        (org-zettel-ref-db-save db)
        (when marked-files (setq org-zettel-ref-marked-files nil)) ; Clear marks if we removed them
        (org-zettel-ref-list-refresh))

      (message "Removed %d of %d database entries." removed-count target-count))))

;; Add Keybinding (Example: C-c C-k) - Needs to be added where other keys are defined
;; (define-key org-zettel-ref-list-mode-map (kbd "C-c C-k") #'org-zettel-ref-list-remove-db-entries)

(defun org-zettel-ref-cleanup-database-titles ()
  """Clean up titles in the database, removing status and rating suffixes.

  This function scans all entries in the database and cleans the title field
  by removing status and rating suffixes. This ensures that the titles
  stored in the database are clean. This is intended as a one-time
  cleanup operation."""
  (interactive)
  (let* ((db (org-zettel-ref-ensure-db))
         (cleaned-count 0)
         (total-count 0))

    (message "Starting database title cleanup...")

    (maphash
     (lambda (id entry)
       (cl-incf total-count)
       (let* ((original-title (org-zettel-ref-ref-entry-title entry))
              (cleaned-title (when original-title
                              (org-zettel-ref-clean-title-for-display original-title))))
         (when (and original-title cleaned-title
                   (not (string= original-title cleaned-title)))
           (message "Cleaning title: '%s' -> '%s'" original-title cleaned-title)
           (setf (org-zettel-ref-ref-entry-title entry) cleaned-title)
           (org-zettel-ref-db-update-ref-entry db entry)
           (cl-incf cleaned-count))))
     (org-zettel-ref-db-refs db))

    (when (> cleaned-count 0)
      (org-zettel-ref-db-save db)
      (message "Database title cleanup complete! Cleaned titles for %d/%d entries." cleaned-count total-count)
      (when (eq major-mode 'org-zettel-ref-list-mode)
        (org-zettel-ref-list-refresh)))
    (when (= cleaned-count 0)
      (message "Database titles are already clean, no cleanup needed (checked %d entries)." total-count))))


;;;----------------------------------------------------------------------------
;;; Overview Link Management
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-list-open-overview ()
  "Open the overview file for the selected source file in the list.
In single-file mode, navigates to the relevant section within the unified overview.
In multi-file mode, opens the dedicated overview file."
  (interactive)
  (let* ((db (org-zettel-ref-ensure-db))
         (source-file-path (org-zettel-ref-list-get-file-at-point))
         (ref-id (when source-file-path (org-zettel-ref-db-get-ref-id-by-path db source-file-path)))
         (ref-entry (when ref-id (org-zettel-ref-db-get-ref-entry db ref-id))))

    (unless ref-entry
      (message "Could not find database entry for the selected item: %s" source-file-path)
      (cl-return-from org-zettel-ref-list-open-overview nil))

    (if org-zettel-ref-use-single-overview-file
        ;; --- Single-File Mode ---
        (when org-zettel-ref-single-overview-file-path
          (let ((single-overview-path (expand-file-name org-zettel-ref-single-overview-file-path)))
            (find-file single-overview-path) ; Open the single overview file
            (with-current-buffer (find-file-noselect single-overview-path) ; Ensure operations are in its buffer
              (let ((target-heading-point nil))
                (org-map-entries
                 (lambda ()
                   (setq target-heading-point (point-marker))
                   t) ; Stop after first match
                 (format "REF_ID=%s" ref-id) ; Match by REF_IDV
                 'file) ; Search scope is the current file
                (if target-heading-point
                    (progn
                      (goto-char target-heading-point)
                      (org-reveal)
                      (recenter)
                      (message "Opened single overview and navigated to section for %s."
                               (org-zettel-ref-ref-entry-title ref-entry)))
                  (message "Section for %s not found in the single overview file."
                           (org-zettel-ref-ref-entry-title ref-entry)))))))
      ;; --- Multi-File Mode ---
      (let* ((db (org-zettel-ref-ensure-db))
             (overview-file-path (buffer-file-name (current-buffer)))
             (overview-id (when overview-file-path
                           (gethash overview-file-path (org-zettel-ref-db-overview-paths db))))
             (overview-entry (when overview-id
                              (gethash overview-id (org-zettel-ref-db-overviews db))))
             (ref-id (when overview-entry
                      (org-zettel-ref-overview-entry-ref-id overview-entry)))
             (ref-entry (when ref-id
                         (org-zettel-ref-db-get-ref-entry db ref-id))))
        (if ref-entry
            (let ((source-file-path (org-zettel-ref-ref-entry-file-path ref-entry)))
              (if (file-exists-p source-file-path)
                  (progn
                    (find-file source-file-path)
                    (with-current-buffer (find-file-noselect source-file-path)
                      (goto-char (point-min))
                      (if (re-search-forward (format "<<hl-%s>>" path) nil t)
                          (goto-char (match-beginning 0))
                        (message "Highlight %s not found in %s" path (file-name-nondirectory source-file-path)))))
                (message "Source file does not exist: %s" source-file-path)))
          (message "Could not find a source file linked to this overview file. DB path: %s"
                   (org-zettel-ref-db-get-db-file-path db)))))))


(defun org-zettel-ref-list-link-overview ()
  "Link current source file with an overview file."
  (interactive)
  (let* ((db (org-zettel-ref-ensure-db))
         (source-file (org-zettel-ref-list-get-file-at-point))
         (ref-id (org-zettel-ref-db-get-ref-id-by-path db source-file)))
    (if (not ref-id)
        (message "No reference entry found for current file")
      (let* ((ref-entry (org-zettel-ref-db-get-ref-entry db ref-id))
             (existing-overview-id (org-zettel-ref-db-get-maps db ref-id))
             (existing-overview (when existing-overview-id
                                (gethash existing-overview-id 
                                        (org-zettel-ref-db-overviews db))))
             (action (completing-read 
                     (if existing-overview
                         (format "Current overview: %s. Action: "
                                 (org-zettel-ref-overview-entry-file-path existing-overview))
                       "Action: ")
                     '("Create new overview" "Select existing overview" "Cancel")
                     nil t)))
        (pcase action
          ("Create new overview"
           (let* ((base-name (file-name-base source-file))
                  (overview-file (expand-file-name 
                                (format "%s__overview.org" base-name)
                                org-zettel-ref-overview-directory)))
             ;; 确保 overview 目录存在
             (unless (file-exists-p org-zettel-ref-overview-directory)
               (make-directory org-zettel-ref-overview-directory t))
             (when (org-zettel-ref-db-ensure-overview-entry 
                    db ref-entry overview-file)
               (message "Created and linked new overview file: %s" overview-file))))
          
          ("Select existing overview"
           (let* ((overview-files 
                   (when (file-exists-p org-zettel-ref-overview-directory)
                     (directory-files 
                      org-zettel-ref-overview-directory
                      t "_overview\\.org$")))
                  (selected-file 
                   (completing-read "Select overview file: " 
                                  overview-files nil t)))
             (when (and selected-file
                       (org-zettel-ref-db-ensure-overview-entry 
                        db ref-entry selected-file))
               (message "Linked to existing overview file: %s" selected-file))))
          
          (_ (message "Operation cancelled")))))))

(defun org-zettel-ref-list-show-links ()
  "Show current source file's links."
  (interactive)
  (let* ((db (org-zettel-ref-ensure-db))
         (source-file (org-zettel-ref-list-get-file-at-point))
         (ref-id (org-zettel-ref-db-get-ref-id-by-path db source-file)))
    (if (not ref-id)
        (message "No reference entry found for current file")
      (let* ((ref-entry (org-zettel-ref-db-get-ref-entry db ref-id))
             (overview-id (org-zettel-ref-db-get-maps db ref-id))
             (overview-entry (when overview-id
                             (gethash overview-id 
                                     (org-zettel-ref-db-overviews db)))))
        (if overview-entry
            (message "Source: %s\nOverview: %s\nRef ID: %s"
                     source-file
                     (org-zettel-ref-overview-entry-file-path overview-entry)
                     ref-id)
          (message "Source: %s\nNo linked overview file\nRef ID: %s"
                   source-file ref-id))))))

(defun org-zettel-ref-list-unlink-overview ()
  "Unlink current source file from its overview file."
  (interactive)
  (let* ((db (org-zettel-ref-ensure-db))
         (source-file (org-zettel-ref-list-get-file-at-point))
         (ref-id (org-zettel-ref-db-get-ref-id-by-path db source-file)))
    (if (not ref-id)
        (message "No reference entry found for current file")
      (let* ((overview-id (org-zettel-ref-db-get-maps db ref-id))
             (overview-entry (when overview-id
                             (gethash overview-id 
                                     (org-zettel-ref-db-overviews db)))))
        (if (not overview-entry)
            (message "No overview file linked to current file")
          (when (y-or-n-p 
                 (format "Unlink overview file %s? "
                         (org-zettel-ref-overview-entry-file-path overview-entry)))
            ;; Remove mapping
            (remhash ref-id (org-zettel-ref-db-map db))
            ;; Remove overview entry
            (remhash overview-id (org-zettel-ref-db-overviews db))
            ;; Remove path mapping
            (let ((overview-path (org-zettel-ref-overview-entry-file-path overview-entry)))
              (remhash overview-path (org-zettel-ref-db-overview-paths db)))
            ;; Save database
            (org-zettel-ref-db-save db)
            (message "Unlinked overview file")))))))

;;;----------------------------------------------------------------------------
;;; Help and Documentation
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-list-help ()
  "Display help for org-zettel-ref-list mode."
  (interactive)
  (with-help-window "*Org-Zettel-Ref Help*"
    (princ "Org-Zettel-Ref List Mode Help\n")
    (princ "==========================\n\n")
    
    (princ "READING STATUS AND RATING\n")
    (princ "------------------------\n\n")
    (princ "s - Cycle through reading status (unread -> reading -> done)\n")
    (princ "R - Set rating (0-5 stars)\n\n")
    
    (princ "FILTERING\n")
    (princ "---------\n\n")
    (princ "/ - Unified filter (recommended)\n")
    (princ "    Syntax examples:\n")
    (princ "    • emacs lisp       - entries with both 'emacs' and 'lisp' in any column\n")
    (princ "    • title:emacs      - entries with 'emacs' in the title\n")
    (princ "    • author:stallman  - entries with 'stallman' in the author field\n")
    (princ "    • \"org mode\" -emacs - entries with exact phrase 'org mode' but not 'emacs'\n\n")
    
    (princ "f - Filter by regexp on a specific column\n")
    (princ "F - Filter by multiple conditions\n")
    (princ "h - Apply filter from history\n")
    (princ "c - Clear all filters\n\n")
    
    (princ "SORTING\n")
    (princ "-------\n\n")
    (princ "S - Sort by column (primary sort)\n")
    (princ "M-S - Add secondary sort column\n")
    (princ "C-c C-s c - Clear all sort keys (reset to default)\n")
    (princ "C-c C-s s - Save current sort configuration\n")
    (princ "C-c C-s l - Load saved sort configuration\n\n")
    
    (princ "FILE OPERATIONS\n")
    (princ "--------------\n\n")
    (princ "RET - Open file at point\n")
    (princ "o - Open file at point in other window\n")
    (princ "r - Rename file at point\n")
    (princ "k - Edit keywords for file at point\n")
    (princ "m - Mark file for deletion\n")
    (princ "u - Unmark file\n")
    (princ "U - Unmark all files\n")
    (princ "x - Delete marked files\n")
    (princ "R - Cycle reading status\n")
    (princ "s - Set rating\n")
    (princ "g - Refresh list\n\n")
    
    (princ "For more information, see the documentation in the source code.")))



(provide 'org-zettel-ref-list)
