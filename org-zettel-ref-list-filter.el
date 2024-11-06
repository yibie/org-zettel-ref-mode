;; org-zettel-ref-list-filter.el --- Filtering for org-zettel-ref list -*- lexical-binding: t; -*-

;;;----------------------------------------------------------------------------
;;; Search and Filtering 
;;;----------------------------------------------------------------------------

(defgroup org-zettel-ref-filter nil
  "Customization for org-zettel-ref filtering."
  :group 'org-zettel-ref)

(defun org-zettel-ref-filter-make-preset (name &optional conditions)
  "Create a filter preset with NAME and filter CONDITIONS."
  (list :name name
        :conditions (or conditions org-zettel-ref-filter-conditions)))

;;; Filter Conditions Structure
(define-derived-mode org-zettel-ref-preset-mode special-mode "Org-Zettel-Ref Filter Preset"
  "Major mode for managing Org-Zettel-Ref filter presets.
\\{org-zettel-ref-preset-mode-map}"
  (use-local-map org-zettel-ref-preset-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defvar-local org-zettel-ref-active-filters nil
  "Current active filters list.
Each filter is a (column . predicate) cons cell.")

;;; Filter Predicate Generation Functions
(defun org-zettel-ref-make-string-filter (column pattern)
  "Create a string matching filter.
COLUMN is the column index, PATTERN is the pattern to match."
  (cons column
        (lambda (entry)
          (let ((value (aref (cadr entry) column)))
            (and value (string-match-p pattern value))))))

;; (defun org-zettel-ref-make-status-filter (status)
;;   "Create a status filter.
;; STATUS can be 'read or 'unread."
;;   (cons 0  ; Status column index
;;         (lambda (entry)
;;           (let ((info (org-zettel-ref-db-get (car entry))))
;;             (if (eq status 'read)
;;                 (org-zettel-ref-state-read-p info)
;;               (org-zettel-ref-state-unread-p info))))))

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
         (pattern (read-string prompt)))
    (if (string-empty-p pattern)
        (org-zettel-ref-remove-filter column)
      (org-zettel-ref-add-filter
       (org-zettel-ref-make-string-filter column pattern)))
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
                     (pattern (read-string prompt)))
                (unless (string-empty-p pattern)
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
                  (let* ((col-idx (car f))
                         (col-desc (aref tabulated-list-format col-idx)))
                    (format "%s" (car col-desc))))
                conditions
                ", ")))
    
    ;; Show message if no filters were applied
    (when (null conditions)
      (message "No filters applied"))))



;; Modify filter application logic to adapt to current data structure
(defun org-zettel-ref-apply-filters (entries)
  "Apply filters to entry list."
  (if (null org-zettel-ref-active-filters)
      entries
    (cl-remove-if-not
     (lambda (entry)
       (cl-every (lambda (filter)
                  (let* ((column (car filter))
                         (pred (cdr filter))
                         (value (aref (cadr entry) column)))
                    (funcall pred value)))
                org-zettel-ref-active-filters))
     entries)))



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

(defun org-zettel-ref-clear-all-filters ()
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


;;;----------------------------------------------------------------------------
;;; Filter Presets: Save, and Load 
;;;---------------------------------------------------------------------------- 

;;; Define Options
(defcustom org-zettel-ref-filter-presets-file
  (expand-file-name "org-zettel-ref-filters.el" user-emacs-directory)
  "File path to store filter presets."
  :type 'file
  :group 'org-zettel-ref-filter)

(defcustom org-zettel-ref-filter-presets nil
  "Defined filter configurations.
Each configuration is a (name . filters) cons cell, where:
- name is the configuration name
- filters is the list of filter conditions"
  :type '(alist :key-type string
                :value-type (repeat (cons integer function)))
  :group 'org-zettel-ref-filter)

;;; Serialize and Deserialize Filters

(defun org-zettel-ref-filter-serialize (filter)
  "Serialize filter to a saveable format.
FILTER is a cons cell of (column . predicate)."
  (let ((column (car filter))
        (predicate (cdr filter)))
    (cond
     ;; Status filter
     ((and (= column 0)
           (string-match-p "Read\\|Unread"
                          (prin1-to-string predicate)))
      `(status . ,(if (string-match-p "Read"
                                     (prin1-to-string predicate))
                      'read 'unread)))
     ;; String/Regexp filter
     (t
      (let* ((pred-str (prin1-to-string predicate))
             (pattern (when (string-match "string-match-p +\"\\([^\"]+\\)\"" pred-str)
                       (match-string 1 pred-str))))
        (when pattern
          `(regexp . (,column . ,pattern))))))))

(defun org-zettel-ref-filter-deserialize (filter-spec)
  "Convert saved filter specification back to actual filter.
FILTER-SPEC is (column . pattern)."
  (when (consp filter-spec)
    (let ((column (car filter-spec))
          (pattern (cdr filter-spec)))
      (org-zettel-ref-make-string-filter column pattern))))

;;; Save and Load Filters

(defun org-zettel-ref-filter-save-current (name)
  "Save current filter configuration as a preset."
  (interactive "sName for filter preset: ")
  (if (null org-zettel-ref-active-filters)
      (message "No active filters to save")
    (let* ((serialized-filters
            (delq nil
                  (mapcar (lambda (filter)
                           (when (and (consp filter)
                                    (numberp (car filter))
                                    (functionp (cdr filter)))
                             (let* ((column (car filter))
                                   (pred-str (prin1-to-string (cdr filter)))
                                   (pattern (and (string-match "string-match-p +\"\\([^\"]+\\)\"" pred-str)
                                               (match-string 1 pred-str))))
                               (when pattern
                                 (list :column column :pattern pattern)))))
                         org-zettel-ref-active-filters)))
           (existing (assoc name org-zettel-ref-filter-presets)))
      (if (null serialized-filters)
          (message "No valid filters to save")
        (if existing
            (setcdr existing serialized-filters)
          (push (cons name serialized-filters)
                org-zettel-ref-filter-presets))
        (org-zettel-ref-filter-save-presets)
        (message "Saved filter preset: %s" name)))))

(defun org-zettel-ref-filter-save-presets ()
  "Save all filter presets to file."
  (with-temp-file org-zettel-ref-filter-presets-file
    (let ((print-length nil)
          (print-level nil))
      (insert ";; -*- lexical-binding: t; -*-\n")
      (insert ";; Org-zettel-ref filter presets\n")
      (prin1 `(setq org-zettel-ref-filter-presets
                    ',org-zettel-ref-filter-presets)
             (current-buffer)))))

(defun org-zettel-ref-filter-load-presets ()
  "Load filter presets from file."
  (when (file-exists-p org-zettel-ref-filter-presets-file)
    (load org-zettel-ref-filter-presets-file)))

;;;----------------------------------------------------------------------------
;;; Apply Filter Presets 
;;;---------------------------------------------------------------------------- 

(defun org-zettel-ref-filter-apply-preset (name)
  "Apply filter preset by name."
  (interactive
   (list (completing-read "Apply filter preset: "
                         (mapcar #'car org-zettel-ref-filter-presets)
                         nil t)))
  (when-let* ((preset (assoc name org-zettel-ref-filter-presets)))
    (setq org-zettel-ref-active-filters
          (delq nil
                (mapcar #'org-zettel-ref-filter-deserialize
                        (cdr preset))))
    (org-zettel-ref-list-refresh)
    (message "Applied filter preset: %s" name)))

(defun org-zettel-ref-filter-delete-preset (name)
  "Delete filter preset by name."
  (interactive
   (list (completing-read "Delete filter preset: "
                         org-zettel-ref-filter-presets nil t)))
  (setq org-zettel-ref-filter-presets
        (assoc-delete-all name org-zettel-ref-filter-presets))
  (org-zettel-ref-filter-save-presets)
  (message "Deleted filter preset: %s" name))

;;;----------------------------------------------------------------------------
;;; Manage Filter Presets 
;;;----------------------------------------------------------------------------  

(defun org-zettel-ref-filter-manage-presets ()
  "Open filter preset management interface."
  (interactive)
  (org-zettel-ref-preset-show)
  (message (substitute-command-keys 
            "Type \\[org-zettel-ref-preset-help] for help")))

(defun org-zettel-ref-filter--manage-presets-simple ()
  "Simple preset management interface (as fallback)."
  (let* ((choice (completing-read
                  "Filter preset action: "
                  '("Apply preset"
                    "Save current filters"
                    "Delete preset"
                    "Show all presets"))))
    (pcase choice
      ("Apply preset"
       (call-interactively #'org-zettel-ref-filter-apply-preset))
      ("Save current filters"
       (call-interactively #'org-zettel-ref-filter-save-current))
      ("Delete preset"
       (call-interactively #'org-zettel-ref-filter-delete-preset))
      ("Show all presets"
       (let ((buf (get-buffer-create "*Org-Zettel-Ref Filter Presets*")))
         (with-current-buffer buf
           (erase-buffer)
           (insert "Org-Zettel-Ref Filter Presets:\n\n")
           (dolist (preset org-zettel-ref-filter-presets)
             (insert (format "* %s\n  %S\n\n"
                              (car preset) (cdr preset))))
           (org-mode))
         (display-buffer buf))))))

;;;---------------------------------------------------------------------------- 
;;; Preset Filter List Mode 
;;;---------------------------------------------------------------------------- 

(defun org-zettel-ref-list--update-mode-line ()
  "Update mode-line to show filter status."
  (setq mode-line-format
        (list "%e" mode-line-front-space
              '(:eval (format "Fliter: %s"
                            (org-zettel-ref-list--format-filter-info)))
              mode-line-end-spaces)))

(define-derived-mode org-zettel-ref-preset-list-mode tabulated-list-mode "Org-Zettel-Ref Presets"
  "Major mode for managing org-zettel-ref filter presets."
  ;; Set format
  (setq tabulated-list-format
        [("Name" 20 t)                  ; Preset name
         ("Description" 40 t)           ; Preset description
         ("Filters" 0 t)])             ; Filter details
  (setq tabulated-list-sort-key (cons "Name" nil))
  ;; Initialize header
  (tabulated-list-init-header))



(defun org-zettel-ref-preset--format-filter-description (filter)
  "Format filter description."
  (cond
   ;; Handle plist format (:column and :pattern)
   ((and (listp filter)
         (plist-get filter :column)
         (plist-get filter :pattern))
    (let ((col-name (aref tabulated-list-format 
                         (plist-get filter :column) 0)))
      (format "%s matches \"%s\"" 
              col-name 
              (plist-get filter :pattern))))
   
   ;; Handle legacy status format if it exists
   ((and (consp filter)
         (eq (car filter) 'status))
    (format "Status: %s" (cdr filter)))
   
   ;; Handle legacy regexp format if it exists
   ((and (consp filter)
         (eq (car filter) 'regexp))
    (let* ((col-spec (cadr filter))
           (col-name (aref tabulated-list-format (car col-spec) 0)))
      (format "%s matches \"%s\"" col-name (cdr col-spec))))
   
   ;; Handle unknown formats
   (t (format "Unknown filter: %S" filter))))

(defun org-zettel-ref-filter-cleanup-presets ()
  "Clean up malformed presets and remove duplicates."
  (interactive)
  (setq org-zettel-ref-filter-presets
        (cl-remove-duplicates
         (cl-remove-if
          (lambda (preset)
            (or (not (consp preset))      ; Not a cons cell
                (null (car preset))       ; No name
                (string-empty-p (car preset)))) ; Empty name
          org-zettel-ref-filter-presets)
         :key #'car
         :test #'string=))
  (org-zettel-ref-filter-save-presets)
  (message "Presets cleaned up"))


(defun org-zettel-ref-preset--get-entries ()
  "Get preset list entries."
  (mapcar
   (lambda (preset)
     (let* ((name (car preset))
            (filters (cdr preset))
            (filter-desc
             (cond
              ((or (null filters) (equal filters '()))
               "No filters defined")
              ((listp filters)
               (mapconcat
                (lambda (filter)
                  (if (plist-get filter :column)
                      (format "%s matches \"%s\""
                             (aref tabulated-list-format 
                                  (plist-get filter :column) 0)
                             (plist-get filter :pattern))
                    "Invalid filter"))
                filters
                "; "))
              (t "Invalid filter format"))))
       (list name
             (vector
              (propertize name 'face 'font-lock-keyword-face)
              (propertize 
               (if (> (length filter-desc) 40)
                   (concat (substring filter-desc 0 37) "...")
                 filter-desc)
               'face 'font-lock-comment-face)
              (propertize (format "%S" filters)
                         'face 'font-lock-comment-face)))))
   org-zettel-ref-filter-presets))

(defun org-zettel-ref-preset-list-buffer ()
  "Create or switch to the preset list buffer."
  (let ((buffer (get-buffer-create "*Org-Zettel-Ref Presets*")))
    (with-current-buffer buffer
      (org-zettel-ref-preset-list-mode)
      (setq tabulated-list-entries (org-zettel-ref-preset--get-entries))
      (tabulated-list-print t))
    buffer))

(defun org-zettel-ref-preset-show ()
  "Display the filter preset manager."
  (interactive)
  (pop-to-buffer (org-zettel-ref-preset-list-buffer)
                 '((display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (side . right)
                   (window-width . 80))))

;;;----------------------------------------------------------------------------
;;; Key-Bindings 
;;;---------------------------------------------------------------------------- 

;; (defvar org-zettel-ref-preset-mode-map (make-sparse-keymap)
;;   "Keymap for `org-zettel-ref-preset-mode'.")
;; (define-key org-zettel-ref-preset-mode-map (kbd "RET") #'org-zettel-ref-preset-apply)    ; Apply preset
;; (define-key org-zettel-ref-preset-mode-map (kbd "a") #'org-zettel-ref-preset-add)        ; Add new preset
;; (define-key org-zettel-ref-preset-mode-map (kbd "d") #'org-zettel-ref-preset-delete)     ; Delete preset
;; (define-key org-zettel-ref-preset-mode-map (kbd "r") #'org-zettel-ref-preset-rename)     ; Rename preset
;; (define-key org-zettel-ref-preset-mode-map (kbd "g") #'org-zettel-ref-preset-refresh)    ; Refresh list
;; (define-key org-zettel-ref-preset-mode-map (kbd "s") #'org-zettel-ref-preset-save-current) ; Save current filters

;; ;; Navigation and help
;; (define-key org-zettel-ref-preset-mode-map (kbd "n") #'next-line)                        ; Next line
;; (define-key org-zettel-ref-preset-mode-map (kbd "p") #'previous-line)                    ; Previous line
;; (define-key org-zettel-ref-preset-mode-map (kbd "?") #'org-zettel-ref-preset-help)       ; Display help
;; (define-key org-zettel-ref-preset-mode-map (kbd "q") #'quit-window)                      ; Quit window

;; ;; 可选的额外快捷键
;; (define-key org-zettel-ref-preset-mode-map (kbd "C-k") #'org-zettel-ref-preset-delete)   ; Delete preset
;; (define-key org-zettel-ref-preset-mode-map (kbd "C-g") #'quit-window)                    ; Quit operation


(require 'tabulated-list)

(defvar org-zettel-ref-preset-mode-map (make-sparse-keymap)
  "Keymap for `org-zettel-ref-preset-mode'.")

(define-key org-zettel-ref-preset-mode-map (kbd "C-c C-m") 'org-zettel-ref-preset-menu)
(define-key org-zettel-ref-preset-mode-map (kbd "RET") 'org-zettel-ref-preset-apply)
(define-key org-zettel-ref-preset-mode-map (kbd "n") 'next-line)
(define-key org-zettel-ref-preset-mode-map (kbd "p") 'previous-line)
(define-key org-zettel-ref-preset-mode-map (kbd "q") 'quit-window)

(defvar org-zettel-ref-preset-actions
  '(("apply" . org-zettel-ref-preset-apply)
    ("add" . org-zettel-ref-preset-add)
    ("delete" . org-zettel-ref-preset-delete)
    ("rename" . org-zettel-ref-preset-rename)
    ("refresh" . org-zettel-ref-preset-refresh)
    ("save current" . org-zettel-ref-preset-save-current)
    ("help" . org-zettel-ref-preset-help)
    ("quit" . quit-window))
  "Available actions for preset management.")

(defun org-zettel-ref-preset-menu ()
  "Display preset management actions in minibuffer."
  (interactive)
  (let* ((choices (mapcar #'car org-zettel-ref-preset-actions))
         (choice (completing-read "选择操作: " choices nil t))
         (action (cdr (assoc choice org-zettel-ref-preset-actions))))
    (when action
      (call-interactively action))))

;; Update keyboard mapping, add menu call
;; First define keyboard mapping variable
(defvar org-zettel-ref-preset-mode-map (make-sparse-keymap)
  "Keymap for `org-zettel-ref-preset-mode'.")

;; Then define key bindings
(define-key org-zettel-ref-preset-mode-map (kbd "m") #'org-zettel-ref-preset-menu)
(define-key org-zettel-ref-preset-mode-map (kbd "RET") #'org-zettel-ref-preset-apply)
(define-key org-zettel-ref-preset-mode-map (kbd "g") #'org-zettel-ref-preset-refresh)    ; Refresh list
(define-key org-zettel-ref-preset-mode-map (kbd "n") #'next-line)
(define-key org-zettel-ref-preset-mode-map (kbd "p") #'previous-line)
(define-key org-zettel-ref-preset-mode-map (kbd "a") #'org-zettel-ref-preset-add)        ; Add new preset
(define-key org-zettel-ref-preset-mode-map (kbd "q") #'quit-window)

;; Update display function, modify help information
(defun org-zettel-ref-preset-show ()
  "Display the filter preset manager."
  (interactive)
  (let ((buffer (get-buffer-create "*Org-Zettel-Ref Presets*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Update help information, highlight menu function
        (insert "Org-Zettel-Ref Filter Presets\n")
        (insert "==========================\n\n")
        (insert "Shortcut Keys:\n")
        (insert "m: Open action menu\n")
        (insert "RET: Apply preset   n/p: Move up/down   q: Quit\n\n")
        ;; Create table header
        (insert "Name             Description          Filters\n")
        (insert "------------------------------------------------\n")
        ;; Insert preset entries
        (tabulated-list-init-header)
        (tabulated-list-print))
      (org-zettel-ref-preset-mode)
      (goto-char (point-min))
      (forward-line 7))
    (switch-to-buffer buffer)))

;; Optional: Add a function to dynamically get current preset names
(defun org-zettel-ref-preset-get-names ()
  "Get all preset names."
  (mapcar #'car org-zettel-ref-filter-presets))

;; Enhanced preset apply function
(defun org-zettel-ref-preset-apply ()
  "Apply preset by selecting from minibuffer."
  (interactive)
  (let* ((preset-names (org-zettel-ref-preset-get-names))
         (choice (completing-read "Select preset: " preset-names nil t)))
    (when choice
      (org-zettel-ref-filter-apply-preset choice))))

;; Enhanced preset delete function
(defun org-zettel-ref-preset-delete ()
  "Delete preset by selecting from minibuffer."
  (interactive)
  (let* ((preset-names (org-zettel-ref-preset-get-names))
         (choice (completing-read "Delete preset: " preset-names nil t)))
    (when choice
      (org-zettel-ref-filter-delete-preset choice))))

;; Enhanced preset rename function
(defun org-zettel-ref-preset-rename ()
  "Rename preset by selecting from minibuffer."
  (interactive)
  (let* ((preset-names (org-zettel-ref-preset-get-names))
         (old-name (completing-read "Select preset to rename: " preset-names nil t))
         (new-name (read-string (format "Rename '%s' to: " old-name))))
    (when (and old-name new-name)
      (let ((preset (assoc old-name org-zettel-ref-filter-presets)))
        (when preset
          (setq org-zettel-ref-filter-presets
                (cons (cons new-name (cdr preset))
                      (remove preset org-zettel-ref-filter-presets))))))))

;;;---------------------------------------------------------------------------- 
;;; Preset Manager Commands 
;;;---------------------------------------------------------------------------- 



(defun org-zettel-ref-preset-add ()
  "Add new preset."
  (interactive)
  (let* ((name (read-string "Preset name: "))
         (desc (read-string "Description: ")))
    (when (and name (not (string-empty-p name)))
      (push (cons name '()) org-zettel-ref-filter-presets)
      (org-zettel-ref-filter-save-presets)
      (tabulated-list-print t))))

(defun org-zettel-ref-preset-delete ()
  "Delete preset at point."
  (interactive)
  (when-let* ((name (tabulated-list-get-id)))
    (when (yes-or-no-p (format "Delete preset '%s'? " name))
      (setq org-zettel-ref-filter-presets
            (assoc-delete-all name org-zettel-ref-filter-presets))
      (org-zettel-ref-filter-save-presets)
      (tabulated-list-print t))))

(defun org-zettel-ref-preset-rename ()
  "Rename preset at point."
  (interactive)
  (when-let* ((old-name (tabulated-list-get-id))
              (new-name (read-string "New name: " old-name)))
    (unless (string= old-name new-name)
      (let ((preset (assoc old-name org-zettel-ref-filter-presets)))
        (setf (car preset) new-name)
        (org-zettel-ref-filter-save-presets)
        (tabulated-list-print t)))))

(defun org-zettel-ref-preset-refresh ()
  "Refresh preset list."
  (interactive)
  (setq tabulated-list-entries (org-zettel-ref-preset--get-entries))
  (tabulated-list-print t))

(defun org-zettel-ref-preset-save-current ()
  "Save current filters as new preset."
  (interactive)
  (let ((name (read-string "Save current filters as: ")))
    (when (and name (not (string-empty-p name)))
      (org-zettel-ref-filter-save-current name)
      (tabulated-list-print t))))

(defun org-zettel-ref-preset-help ()
  "Display preset manager help information."
  (interactive)
  (let ((help-window-select t))
    (with-help-window "*Org-Zettel-Ref Preset Help*"
      (princ org-zettel-ref-preset-help-message))))

(defun org-zettel-ref-filter-manage-presets-safely ()
  "Safely open filter preset manager, use simple interface on error."
  (interactive)
  (condition-case err
      (org-zettel-ref-filter-manage-presets)
    (error
     (message "Could not open preset manager: %s. Using simple interface."
              (error-message-string err))
     (org-zettel-ref-filter--manage-presets-simple))))



;;;----------------------------------------------------------------------------
;;; Initialization 
;;;----------------------------------------------------------------------------  
(defun org-zettel-ref-filter-initialize ()
  "Initialize the filter system."
  (org-zettel-ref-filter-load-presets)
  (org-zettel-ref-filter-cleanup-presets))


(org-zettel-ref-filter-initialize)

(provide 'org-zettel-ref-list-filter)