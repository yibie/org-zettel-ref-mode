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
(define-key org-zettel-ref-list-mode-map (kbd "C-c C-s g") 'org-zettel-ref-list-goto-column)
(define-key org-zettel-ref-list-mode-map (kbd "C-c g") 'org-zettel-ref-list-goto-column)

;;;----------------------------------------------------------------------------
;;; File Name Parsing and Formatting
;;;----------------------------------------------------------------------------

(defconst org-zettel-ref-author-regexp "^\\(.*?\\)__"
  "Match the author part in the file name.") 

(defconst org-zettel-ref-title-regexp "\\(?:^\\|__\\)\\(.*?\\)\\(?:==\\|$\\)"
  "Match the title part in the file name.")

(defconst org-zettel-ref-keywords-regexp "==\\(.*?\\)\\(?:\\..*\\)?$"
  "Match the keywords part in the file name.")

;; Parse file name
(defun org-zettel-ref-parse-filename (filename)
  "Parse FILENAME into a list of (author title keywords)."
  (let (author title keywords)
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
    
    ;; Trim all parts
    (setq author (when author (string-trim author))
          title (when title (string-trim title))
          keywords (when keywords (mapcar #'string-trim keywords)))
    
    (list author title keywords)))

;; Format file name
(defun org-zettel-ref-format-filename (author title keywords)
  "Generate a standard file name: AUTHOR__TITLE==KEYWORDS.org"
  (concat 
   (when author (concat author "__"))
     title
     (when keywords (concat "==" (string-join keywords "_")))
     ".org"))

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
        (org-zettel-ref-column-modified)
        (org-zettel-ref-column-keywords)))

;;;----------------------------------------------------------------------------
;;; Display Content: Entry Formatting
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-list--format-entry (id entry)
  "Format entry for tabulated list display.
ID is the entry identifier.
ENTRY is the org-zettel-ref-entry struct."
  (if (null entry)
      (progn 
        (message "Warning: Nil entry for id %s" id)
        (make-vector (length (org-zettel-ref-list-columns)) ""))
    (let* ((file-path (org-zettel-ref-ref-entry-file-path entry))
           (title (or (org-zettel-ref-ref-entry-title entry)
                     (file-name-base file-path)))
           (title-with-props  ; Add file path property to title
            (propertize title
                       'file-path file-path
                       'help-echo file-path)))
      (vector
       title-with-props      ; Title (with file path property)
       (or (org-zettel-ref-ref-entry-author entry) "")
       (format-time-string "%Y-%m-%d %H:%M:%S"
                          (org-zettel-ref-ref-entry-modified entry))
       (if-let* ((keywords (org-zettel-ref-ref-entry-keywords entry)))
           (string-join keywords ", ")
         "")))))

(defun org-zettel-ref-list--get-entries ()
  "Get entries for tabulated list display."
  (let ((entries '())
        (db (org-zettel-ref-ensure-db)))
    (maphash
     (lambda (id entry)
       (when-let* ((file-path (org-zettel-ref-ref-entry-file-path entry))
                   (exists (file-exists-p file-path)))
         (let* ((file-name (file-name-nondirectory file-path))
                (parsed-info (org-zettel-ref-parse-filename file-name))
                (author (nth 0 parsed-info))  
                (title (nth 1 parsed-info))   
                (keywords (nth 2 parsed-info)))
           (push (list file-path
                      (vector
                       ;; Title column 
                       (propertize 
                        (or title "Untitled")
                        'file-path file-path
                        'help-echo file-path)
                       ;; Author column 
                       (propertize 
                        (or author "")
                        'help-echo (format "Author: %s" (or author "Unknown")))
                       ;; Modified time column
                       (format-time-string "%Y-%m-%d %H:%M:%S"
                                         (org-zettel-ref-ref-entry-modified entry))
                       ;; Keywords column
                       (string-join (or keywords '()) ", ")))
                 entries))))
     (org-zettel-ref-db-refs db))
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
      (let* ((dir (file-name-directory old-file))
             (ref-id (when old-file (org-zettel-ref-db-get-ref-id-by-path db old-file))))
        (if (not ref-id)
            (message "Error: Cannot find database entry for file: %s" old-file)
          (let* ((ref-entry (org-zettel-ref-db-get-ref-entry db ref-id))
                 (current-author (org-zettel-ref-ref-entry-author ref-entry))
                 (current-title (org-zettel-ref-ref-entry-title ref-entry))
                 (current-keywords (org-zettel-ref-ref-entry-keywords ref-entry))
                 (new-author (org-zettel-ref-rename--prompt-author current-author))
                 (new-title (org-zettel-ref-rename--prompt-title current-title))
                 (new-keywords (org-zettel-ref-rename--prompt-keywords current-keywords))
                 (new-file-name (org-zettel-ref-format-filename new-author new-title new-keywords))
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
                    (when-let* ((ref-id (org-zettel-ref-db-get-ref-id-by-path db old-file))
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
              (org-zettel-ref-watch-directory))))))))

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
          (let* ((new-keywords-list (split-string new-keywords-input "[,\s]+" t))
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
                      (org-zettel-ref-ref-entry-modified ref-entry) (current-time)))
              ;; Update opened buffer
              (when-let* ((buf (get-file-buffer file)))
                (with-current-buffer buf
                  (set-visited-file-name new-filepath)
                  (set-buffer-modified-p nil))))))))
    ;; Save database and refresh display
    (org-zettel-ref-db-save db)
    (org-zettel-ref-list-refresh)
    (org-zettel-ref-unmark-all)
    (message "Keywords updated successfully")))


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
(defun org-zettel-ref-list-delete-file ()
  "Delete the currently selected file."
  (interactive)
  (let* ((file (org-zettel-ref-list-get-file-at-point))
         (db (org-zettel-ref-ensure-db))
         (ref-id (when file (org-zettel-ref-db-get-ref-id-by-path db file))))
    (when (and file ref-id
               (yes-or-no-p (format "Delete file %s? " file)))
      ;; Delete mapping relationship
      (when-let* ((overview-id (org-zettel-ref-db-get-maps db ref-id)))
        (remhash ref-id (org-zettel-ref-db-map db))
        (remhash overview-id (org-zettel-ref-db-overviews db)))
      ;; Delete reference entry
      (remhash ref-id (org-zettel-ref-db-refs db))
      ;; Delete path index
      (remhash file (org-zettel-ref-db-ref-paths db))
      ;; Delete actual file
      (condition-case err
          (progn
            (delete-file file)
            (org-zettel-ref-db-save db)
            (org-zettel-ref-list-refresh)
            (message "File deleted successfully"))
        (error
         (message "Error deleting file %s: %s" 
                 file (error-message-string err)))))))

(defun org-zettel-ref-list-delete-marked-files ()
  "Delete all marked files."
  (interactive)
  (let* ((files org-zettel-ref-marked-files)
         (file-count (length files))
         (db (org-zettel-ref-ensure-db))
         (deleted 0))
    (when (and files
               (yes-or-no-p 
                (format "Delete %d marked file%s? " 
                        file-count
                        (if (= file-count 1) "" "s"))))
      (dolist (file files)
        (when-let* ((ref-id (org-zettel-ref-db-get-ref-id-by-path db file))
                         (ref-entry (org-zettel-ref-db-get-ref-entry db ref-id)))
          ;; Delete mapping relationship
          (when-let* ((overview-id (org-zettel-ref-db-get-maps db ref-id)))
            (remhash ref-id (org-zettel-ref-db-map db))
            (remhash overview-id (org-zettel-ref-db-overviews db)))
          ;; Delete reference entry
          (remhash ref-id (org-zettel-ref-db-refs db))
          ;; Delete path index
          (remhash file (org-zettel-ref-db-ref-paths db))
          ;; Delete actual file
          (condition-case err
              (progn
                (delete-file file)
                (cl-incf deleted))
            (error
             (message "Error deleting file %s: %s" 
                     file (error-message-string err)))))
        ;; Save database
        (org-zettel-ref-db-save db)
        (when (called-interactively-p 'any)
          (message "Deleted %d entries" deleted)))
    (setq org-zettel-ref-marked-files nil)
    (org-zettel-ref-list-refresh)
    (message "Successfully deleted %d of %d files" 
            deleted file-count))))


;;;----------------------------------------------------------------------------
;;; Interactive Menu System
;;;----------------------------------------------------------------------------

(defvar org-zettel-ref-list-actions
  '(("open file" . org-zettel-ref-list-open-file)
    ("rename file" . org-zettel-ref-list-rename-file)
    ("refresh list" . org-zettel-ref-list-refresh)
    ("delete file" . org-zettel-ref-list-delete-file)
    ("mark file" . org-zettel-ref-mark-file)
    ("unmark file" . org-zettel-ref-unmark-file)
    ("delete marked files" . org-zettel-ref-list-delete-marked-files)
    ("unmark all" . org-zettel-ref-unmark-all)
    ("filter by regexp" . org-zettel-ref-filter-by-regexp)
    ("clear filters" . org-zettel-ref-clear-all-filters)
    ("manage filter presets" . org-zettel-ref-filter-manage-presets))
  "Available actions for reference list management.")

(defun org-zettel-ref-list-menu ()
  "Display reference list management actions in minibuffer."
  (interactive)
  (let* ((choices (mapcar #'car org-zettel-ref-list-actions))
         (choice (completing-read "Select action: " choices nil t))
         (action (cdr (assoc choice org-zettel-ref-list-actions))))
   (when action
      (call-interactively action))))

;; Add menu key binding while keeping existing bindings
(define-key org-zettel-ref-list-mode-map (kbd "C-c C-m") #'org-zettel-ref-list-menu)

;; Keep existing key bindings
(define-key org-zettel-ref-list-mode-map (kbd "RET") #'org-zettel-ref-list-open-file)
(define-key org-zettel-ref-list-mode-map (kbd "o") #'org-zettel-ref-list-open-file)
(define-key org-zettel-ref-list-mode-map (kbd "r") #'org-zettel-ref-list-rename-file)
(define-key org-zettel-ref-list-mode-map (kbd "g") #'org-zettel-ref-list-refresh)
(define-key org-zettel-ref-list-mode-map (kbd "d") #'org-zettel-ref-list-delete-file)
(define-key org-zettel-ref-list-mode-map (kbd "k") #'org-zettel-ref-list-edit-keywords)
(define-key org-zettel-ref-list-mode-map (kbd "m") #'org-zettel-ref-mark-file)
(define-key org-zettel-ref-list-mode-map (kbd "u") #'org-zettel-ref-unmark-file)
(define-key org-zettel-ref-list-mode-map (kbd "D") #'org-zettel-ref-list-delete-marked-files)
(define-key org-zettel-ref-list-mode-map (kbd "U") #'org-zettel-ref-unmark-all)
(define-key org-zettel-ref-list-mode-map (kbd "/") nil)  
(define-key org-zettel-ref-list-mode-map (kbd "/ r") #'org-zettel-ref-filter-by-regexp)
(define-key org-zettel-ref-list-mode-map (kbd "/ c") #'org-zettel-ref-clear-all-filters)
;(define-key org-zettel-ref-list-mode-map (kbd "/ p") #'org-zettel-ref-filter-manage-presets)
(define-key org-zettel-ref-list-mode-map (kbd "/ m") #'org-zettel-ref-filter-by-multiple-conditions)



;;;----------------------------------------------------------------------------
;;; org-zettel-ref-list
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-list ()
  "Display reference list."
  (interactive)
  (let ((db (org-zettel-ref-ensure-db)))
    ;; Always scan directory but only create entries for new files
    (org-zettel-ref-scan-directory db)
    
    (let ((buffer (get-buffer-create "*Org Zettel Ref List*")))
      (with-current-buffer buffer
        (org-zettel-ref-list-mode)
        (let ((inhibit-read-only t))
          (erase-buffer))
        (let ((entries (org-zettel-ref-list--get-entries)))
          (setq tabulated-list-entries entries)
          (tabulated-list-print t))
        (goto-char (point-min)))
      (switch-to-buffer buffer)
      buffer)))

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
DB is the database object."
  (let ((files (org-zettel-ref-find-ref-files))
        (new-count 0)
        (existing-count 0)
        (added 0))
    (message "Found %d files to process" (length files))
    
    ;; Reset ID counter at start of scan
    (setq org-zettel-ref-id-counter 0)
    
    (dolist (file files)
      (let* ((file-path (expand-file-name file))
             (ref-id (org-zettel-ref-db-get-ref-id-by-path db file-path)))
        (if ref-id
            (cl-incf existing-count)
          (let* ((file-info (org-zettel-ref-parse-file file))
                 (title (plist-get file-info :title))
                 (author (plist-get file-info :author))
                 (keywords (plist-get file-info :keywords)))
            (org-zettel-ref-db-ensure-ref-entry db file-path title author keywords)
            (cl-incf new-count)
            (cl-incf added)
            
            ;; Save database every 100 entries
            (when (zerop (mod added 100))
              (message "Saving database after %d new entries..." added)
              (org-zettel-ref-db-save db))))))
    
    (message "Scan complete: %d new files, %d existing files"
             new-count existing-count)
    ;; Final save if there are any new entries
    (when (> new-count 0)
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
        (run-with-timer 
         0.5 nil
         (lambda ()
           (when (buffer-live-p (get-buffer "*Org Zettel Ref List*"))
             (with-current-buffer "*Org Zettel Ref List*"
               (when (eq major-mode 'org-zettel-ref-list-mode)
                 (org-zettel-ref-list-refresh)
                 (message "Refreshed due to file change: %s" 
                         (file-name-nondirectory file)))))))))))

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


;;;----------------------------------------------------------------------------
;;; Help and Documentation
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-list-help ()
  "Display help for org-zettel-ref-list mode."
  (interactive)
  (with-help-window "*Org-Zettel-Ref Help*"
    (princ "Org-Zettel-Ref List Mode Help\n")
    (princ "==========================\n\n")
    
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
    (princ "g - Refresh list\n\n")
    
    (princ "For more information, see the documentation in the source code.")))

(provide 'org-zettel-ref-list)
