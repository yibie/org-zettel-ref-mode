;;; org-zettel-ref-core.el --- Core functionality for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains core functionality for org-zettel-ref.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'org-zettel-ref-utils)
(require 'org-zettel-ref-db) 
(require 'org-zettel-ref-list)
(require 'org-zettel-ref-migrate)
(require 'org-zettel-ref-highlight)

;;-----------------------
;; Customization
;;-----------------------

(defgroup org-zettel-ref nil
  "Customization group for org-zettel-ref."
  :group 'org)

(defcustom org-zettel-ref-use-single-overview-file nil
  "When non-nil, use a single file for all overview notes.
If nil (the default), each source file will have its own dedicated overview file."
  :type 'boolean
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-single-overview-file-path
  (expand-file-name "org-zettel-ref-unified-overview.org" user-emacs-directory)
  "The full path to the single overview file.
This is used when `org-zettel-ref-use-single-overview-file` is non-nil."
  :type 'file
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-overview-directory "~/org-zettel-ref-overviews/"
  "Directory to store overview files when NOT using single-file mode."
  :type 'directory
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-mode-type 'normal
  "The type of mode to use for org-zettel-ref. Can be `normal`, `denote`, or `org-roam`."
  :type '(choice (const :tag "Normal" normal)
                 (const :tag "Denote" denote)
                 (const :tag "Org-roam" org-roam))
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-overview-file-suffix "__overview.org"
  "Suffix to be added to overview files created by org-zettel-ref in Denote mode.
This suffix will be appended to the filename before the file extension."
  :type 'string
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-auto-save-place t
  "Whether to automatically enable save-place-mode when org-zettel-ref is used.
When non-nil, save-place-mode will be enabled if it isn't already.
Users who prefer to manage save-place-mode themselves can set this to nil."
  :type 'boolean
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-overview-width-ratio 0.5
  "Ratio of overview window width relative to source window width.
Should be a float between 0.0 and 1.0.
For example, 0.3 means the overview window will take 30% of the source window width."
  :type 'float
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-overview-min-width 30
  "Minimum width in characters for the overview window."
  :type 'integer
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-enable-ai-summary nil
  "Enable AI summary generation feature.
When enabled, summaries can be automatically generated for new notes
and can be manually triggered with `org-zettel-ref-ai-generate-summary'."
  :type 'boolean
  :group 'org-zettel-ref)

;;------------------------------------------------------------------  
;; Variables
;;------------------------------------------------------------------

(defvar org-zettel-ref-overview-file nil
  "The current overview file being used.")

(defvar org-zettel-ref-current-overview-buffer nil
  "The current overview buffer being used.")

(defvar org-zettel-ref-db nil
  "The persistent database instance for org-zettel-ref.")

(defvar-local org-zettel-ref-source-buffer nil
  "store the source buffer.")

(defvar org-zettel-ref-init-hook nil
  "Hook run after `org-zettel-ref-init` completes successfully.")

(defvar org-zettel-ref-db-initialized nil
  "Flag indicating whether the database has been initialized.")

(defvar org-zettel-ref--active-buffers (make-hash-table :test 'equal)
  "Hash table to store active buffers and their relations.")

(defun org-zettel-ref--activate-buffer (buffer)
  "Activate org-zettel-ref functionality for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local org-zettel-ref--active t)
      (puthash buffer t org-zettel-ref--active-buffers))))

(defun org-zettel-ref--deactivate-buffer (buffer)
  "Deactivate org-zettel-ref functionality for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local org-zettel-ref--active nil))
    (remhash buffer org-zettel-ref--active-buffers)))

(defun org-zettel-ref--is-active-buffer (buffer)
  "Check if BUFFER is an active org-zettel-ref buffer."
  (and (buffer-live-p buffer)
       (gethash buffer org-zettel-ref--active-buffers)))

;;------------------------------------------------------------------
;; Safe File Loading
;;------------------------------------------------------------------

(defun org-zettel-ref--safe-find-file-noselect (file-path &optional create-on-error)
  "Safely load FILE-PATH with comprehensive error handling.
If CREATE-ON-ERROR is non-nil, create a safe buffer if loading fails.
Returns the buffer or nil on failure."
  (let ((find-file-hook nil)
        (after-find-file-hook nil)
        (save-place-mode nil)
        (org-display-inline-images nil)
        (org-startup-with-inline-images nil)
        (org-element-use-cache nil))
    (condition-case err
        (let ((buffer (find-file-noselect file-path)))
          ;; Additional safety measures after loading
          (with-current-buffer buffer
            ;; Disable problematic org features
            (when (boundp 'org-element-use-cache)
              (setq-local org-element-use-cache nil))
            (when (fboundp 'org-element-cache-reset)
              (org-element-cache-reset))
            (setq-local org-display-inline-images nil)
            (setq-local org-startup-with-inline-images nil))
          buffer)
      (error
       (message "Error loading file %s: %s" file-path (error-message-string err))
       (when create-on-error
         (let ((safe-buffer (get-buffer-create 
                            (format "*Safe-%s*" (file-name-nondirectory file-path)))))
           (with-current-buffer safe-buffer
             (setq buffer-file-name file-path)
             ;; Enable org-mode with minimal features
             (let ((org-element-use-cache nil)
                   (org-display-inline-images nil))
               (org-mode))
                           (erase-buffer)
              (if (string-match "overview" file-path)
                  ;; For overview files, create proper structure
                  (progn
                    (insert "#+TITLE: Unified Org Zettel Ref Overview (Safe Mode)\n")
                    (insert (format "#+AUTHOR: %s\n" (user-full-name)))
                    (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d")))
                    (insert "#+STARTUP: content\n\n")
                    (insert "* Safe mode activated due to loading error\n")
                    (insert (format "** Original error: %s\n" (error-message-string err)))
                    (insert "** This is a temporary safe buffer. Please check the original file.\n"))
                ;; For other files, minimal content
                (progn
                  (insert "#+TITLE: Safe Mode\n")
                  (insert "#+STARTUP: content\n\n")
                  (insert "* File loading failed, safe mode activated\n")
                  (insert (format "** Original error: %s\n" (error-message-string err)))))
             (save-buffer))
           safe-buffer))))))

;;------------------------------------------------------------------
;; Overview File Management
;;------------------------------------------------------------------

(defun org-zettel-ref-create-overview-file (source-buffer target-file ref-entry)
  "Create overview file for SOURCE-BUFFER at TARGET-FILE using REF-ENTRY metadata."
  (unless (file-exists-p target-file)
    (pcase org-zettel-ref-mode-type
      ('normal (org-zettel-ref-get-normal-overview source-buffer target-file ref-entry))
      ('denote (org-zettel-ref-get-overview-file-denote source-buffer target-file ref-entry))
      ('org-roam (org-zettel-ref-get-overview-file-org-roam source-buffer target-file ref-entry))
      (_ (error "Unsupported org-zettel-ref-mode-type: %s" org-zettel-ref-mode-type))))
  target-file)

(defun org-zettel-ref-cleanup-overview ()
  "Close overview buffer if source buffer has changed.
But keep both source and overview buffers when user is switching between them."
  (let ((current-buffer (current-buffer)))
    ;; Check all windows
    (dolist (window (window-list))
      (let ((buffer (window-buffer window)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            ;; Check if it's an overview buffer
            (when (and (org-zettel-ref--is-active-buffer buffer)
                       (local-variable-p 'org-zettel-ref-source-buffer)
                       org-zettel-ref-source-buffer
                       (buffer-live-p org-zettel-ref-source-buffer)
                       ;; 不要关闭 overview buffer 或者 source buffer
                       ;; 只有当当前 buffer 不是 overview buffer 或 source buffer 时才关闭
                       (not (or (eq buffer current-buffer) 
                                (eq buffer org-zettel-ref-source-buffer)
                                (eq current-buffer org-zettel-ref-source-buffer)))
                       ;; 不要关闭正在被编辑的 overview buffer
                       (not (and (eq buffer org-zettel-ref-current-overview-buffer)
                                (window-dedicated-p window)))
                       org-zettel-ref-overview-file
                       (file-exists-p org-zettel-ref-overview-file))
              ;; Save if needed
              (when (buffer-modified-p)
                (save-buffer))
              ;; Delete window and buffer
              (let ((buf (current-buffer)))
                (delete-window window)  ;; 直接删除窗口，而不是切换buffer
                (kill-buffer buf)
                (org-zettel-ref--deactivate-buffer buf)))))))))

(defun org-zettel-ref-maybe-cleanup-overview ()
  "Only cleanup overview when appropriate conditions are met."
  (when (and (not (minibufferp))
             (not (window-minibuffer-p))
             (not executing-kbd-macro)
             ;; 不要在其他 mode 切换时触发清理
             (not (and (boundp 'org-zettel-ref-current-overview-buffer)
                      (eq (current-buffer) org-zettel-ref-current-overview-buffer))))
    (org-zettel-ref-cleanup-overview)))

(remove-hook 'window-configuration-change-hook #'org-zettel-ref-cleanup-overview)
(add-hook 'window-configuration-change-hook #'org-zettel-ref-maybe-cleanup-overview)

;;------------------------------------------------------------------
;; Buffer Management
;;------------------------------------------------------------------
(defun org-zettel-ref-get-overview-buffer-name (source-buffer)
  "Get the overview buffer name for the given SOURCE-BUFFER."
  (let* ((source-file-name (buffer-file-name source-buffer))
         (title (if source-file-name (file-name-base source-file-name) "Untitled")))
    ;; Buffer name does not include timestamp, ensuring consistency
    (format "*Org Zettel Ref: %s__overview*" title)))


(defun org-zettel-ref-setup-buffers (source-buffer overview-buffer)
  "Setup SOURCE-BUFFER and OVERVIEW-BUFFER for org-zettel-ref."
  (with-current-buffer overview-buffer
    (setq-local org-zettel-ref-source-buffer source-buffer))
  
  (with-current-buffer source-buffer
    (setq-local org-zettel-ref-source-buffer source-buffer)))

;;------------------------------------------------------------------
;; Synchronization
;;------------------------------------------------------------------

(defun org-zettel-ref--collect-highlights-from-source ()
  "Collect all highlight data from the current source buffer.
Returns a list of highlights, where each element is a list:
(original-hl-id type-symbol text name-string prefix-string img-path-string img-desc-string)"
  (let ((highlights '()) (org-element-use-cache nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
        (let* ((original-hl-id (match-string 1))
               (type-char (or (match-string 2) ""))
               (text (or (match-string 3) ""))
               (type-sym (org-zettel-ref-highlight-char-to-type type-char))
               (config (cdr (assoc type-sym org-zettel-ref-highlight-types))))
          (org-zettel-ref-debug-message-category 'core "Collect: Processing highlight - original_id: %s, type_char: %s, type_sym: %s" original-hl-id type-char type-sym)
          (when (and type-sym config)
            (let ((name-str (plist-get config :name)) (prefix-str (plist-get config :prefix)))
              (if (eq type-sym 'image)
                  (let* ((img-parts (split-string text "|")) (img-path-str (car img-parts)) (img-desc-str (cadr img-parts)))
                    (when (and img-path-str (not (string-empty-p img-path-str)))
                      (push (list original-hl-id type-sym text name-str prefix-str img-path-str img-desc-str) highlights)))
                (push (list original-hl-id type-sym text name-str prefix-str nil nil) highlights)))))))
    (nreverse highlights)))

(defun org-zettel-ref--cleanup-duplicate-headings (source-ref-id)
  "Clean duplicate headings, keeping only the first matching REF_ID title.
  
  Parameters:
  - SOURCE-REF-ID: Source file reference ID to clean
  
  Input: Source file reference ID string
  Output: Position of kept heading, nil if not found
  
  This function scans the current buffer, finds all titles with the same REF_ID attribute,
  keeps the first found title, and deletes the rest of the duplicate titles."
  (let ((found-positions '())
        (kept-heading nil))
    (message "DEBUG: Start cleaning duplicate headings, REF_ID: %s" source-ref-id)
    
    ;; Collect all matching title positions
    (condition-case collect-err
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^\\* .*$" nil t)
            (let ((heading-start (line-beginning-position)))
              (save-excursion
                (condition-case prop-err
                    (let ((props (org-entry-properties)))
                      (when (string= (cdr (assoc "REF_ID" props)) source-ref-id)
                        (push heading-start found-positions)))
                  (error
                   (message "DEBUG: Error getting properties, position: %s, error: %s" heading-start (error-message-string prop-err))))))))
      (error
       (message "DEBUG: Error collecting title positions: %s" (error-message-string collect-err))))
    
    (when (> (length found-positions) 1)
      (message "DEBUG: Found %d duplicate headings, REF_ID: %s" (length found-positions) source-ref-id)
      (setq found-positions (nreverse found-positions)) ; Sort by position
      (setq kept-heading (car found-positions)) ; Keep the first one
      
      ;; Delete subsequent duplicate headings
      (dolist (pos (cdr found-positions))
        (condition-case del-err
            (save-excursion
              (goto-char pos)
              (condition-case heading-err
                  (org-back-to-heading t)
                (error
                 (message "DEBUG: org-back-to-heading failed, position: %s, error: %s" pos (error-message-string heading-err))
                 ;; If org-back-to-heading fails, try manual positioning
                 (goto-char pos)
                 (beginning-of-line)))
              (let ((subtree-end (condition-case end-err
                                     (save-excursion (org-end-of-subtree t t))
                                   (error
                                    (message "DEBUG: org-end-of-subtree failed, using simple method: %s" (error-message-string end-err))
                                    ;; If org-end-of-subtree fails, find the next sibling or higher heading
                                    (save-excursion
                                      (forward-line 1)
                                      (if (re-search-forward "^\\* " nil t)
                                          (line-beginning-position)
                                        (point-max)))))))
                (message "DEBUG: Delete duplicate heading, position: %s" pos)
                (delete-region (point) subtree-end)))
          (error
           (message "DEBUG: Error deleting heading, position: %s, error: %s" pos (error-message-string del-err)))))
      
      (message "DEBUG: Cleanup complete, kept heading position: %s" kept-heading)
      kept-heading)
    
    ;; If there is only one or none, return the first found position
    (car found-positions)))

(defun org-zettel-ref--sync-highlights-single-file (highlights overview-file-path source-ref-id source-file-path source-title overview-buffer-hint)
  "Sync highlights to the specified source file entry in the single overview file.
  
    Parameters:
  - HIGHLIGHTS: List of highlights, each element contains (original-hl-id, type-sym, text, name, prefix, img-path, img-desc)
  - OVERVIEW-FILE-PATH: Overview file path
  - SOURCE-REF-ID: Source file reference ID
  - SOURCE-FILE-PATH: Source file path
  - SOURCE-TITLE: Source file title
  - OVERVIEW-BUFFER-HINT: Overview buffer hint
  
  Input: List of highlights and source file information
  Output: No return value, but updates the highlights in the corresponding source file in the overview file
  
  This function uses single-file mode, with an incremental synchronization strategy based on the database,
  only updating changed content, preserving user-added custom content."""
  (when (and overview-file-path
             (file-exists-p overview-file-path)
             source-ref-id source-file-path source-title)
    (org-zettel-ref-debug-message-category 'core
                                         "Single-File Sync: Processing %d highlights for source %s ('%s') into %s"
                                         (length highlights) source-ref-id source-title overview-file-path)
    
    (let* ((db (org-zettel-ref-ensure-db))
           (changes (org-zettel-ref--detect-highlight-changes highlights db source-ref-id))
           (overview-buffer (or (get-buffer overview-buffer-hint)
                               (org-zettel-ref--safe-find-file-noselect overview-file-path nil))))
      (message "DEBUG: Single-File - Using overview buffer: %s for file %s" overview-buffer overview-file-path)
      (message "DEBUG: Changes detected - Added: %d, Removed: %d, Updated: %d, Unchanged: %d"
               (length (plist-get changes :added))
               (length (plist-get changes :removed))
               (length (plist-get changes :updated))
               (length (plist-get changes :unchanged)))
      
      (with-current-buffer overview-buffer
        ;; Use basic error prevention measures
        (setq-local org-element-use-cache nil)
        (when (fboundp 'org-element-cache-reset) 
          (org-element-cache-reset))
        
        (org-with-wide-buffer
         ;; First step: Ensure source file title exists
         (let ((source-heading-point (org-zettel-ref--ensure-source-heading source-ref-id source-title source-file-path)))
           
           ;; Second step: Process deleted highlights
           (org-zettel-ref--remove-highlights source-heading-point (plist-get changes :removed))
           
           ;; Third step: Process added and updated highlights
           (org-zettel-ref--add-or-update-highlights source-heading-point 
                                                    (append (plist-get changes :added)
                                                           (plist-get changes :updated))))))
        
        (save-buffer)
        
        ;; Update highlight state in the database
        (org-zettel-ref--update-highlight-state db source-ref-id highlights)
        
        (message "DEBUG: Incremental sync complete, REF_ID: %s" source-ref-id))))

(defun org-zettel-ref--ensure-source-heading (source-ref-id source-title source-file-path)
  "Ensure the source file title exists in the overview file.
  
  Parameters:
  - SOURCE-REF-ID: Source file reference ID
  - SOURCE-TITLE: Source file title
  - SOURCE-FILE-PATH: Source file path
  
  Input: Source file information
  Output: Position of source file title marker
  
  This function finds or creates the source file title in the overview file."""
  (let ((source-heading-point nil))
    ;; Find existing source file title
    (save-excursion
      (goto-char (point-min))
      (while (and (not source-heading-point) (re-search-forward "^\\* .*$" nil t))
        (let ((props (org-entry-properties)))
          (when (string= (cdr (assoc "REF_ID" props)) source-ref-id)
            (setq source-heading-point (copy-marker (line-beginning-position)))))))
    
    ;; If not found, create new source file title
    (unless source-heading-point
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* [[ref:%s][%s]]\n" source-ref-id source-title))
      (org-entry-put nil "REF_ID" source-ref-id)
      (org-entry-put nil "SOURCE_FILE_PATH" (format "file:%s" source-file-path))
      (setq source-heading-point (copy-marker (line-beginning-position))))
    
    source-heading-point))

(defun org-zettel-ref--remove-highlights (source-heading-point removed-highlights)
  "Remove specified highlights.
  
  Parameters:
  - SOURCE-HEADING-POINT: Position of source file title
  - REMOVED-HIGHLIGHTS: List of highlights to remove
  
  Input: Title position and list of highlights to remove
  Output: No return value, but deletes the corresponding highlights
  
  This function only deletes specified highlights, preserving other user content."""
  (when removed-highlights
    (dolist (removed-hl removed-highlights)
      (let ((hl-id (nth 0 removed-hl)))
        (catch 'highlight-deleted
          (save-excursion
            (goto-char source-heading-point)
            (org-back-to-heading t)
            (org-narrow-to-subtree)
            (goto-char (point-min))
            
            ;; Find and delete corresponding highlights
            (while (re-search-forward "^\\** .*$" nil t)
              (let ((props (org-entry-properties)))
                (when (string= (cdr (assoc "ORIGINAL_HL_ID" props)) hl-id)
                  ;; Find matching highlight, delete entire entry
                  (org-back-to-heading t)
                  (let ((subtree-end (save-excursion (org-end-of-subtree t t))))
                    (delete-region (point) subtree-end))
                  (message "DEBUG: Deleted highlight entry: %s" hl-id)
                  (throw 'highlight-deleted t))))
            (widen)))))))

(defun org-zettel-ref--add-or-update-highlights (source-heading-point highlights)
  "Add or update highlights.
  
  Parameters:
  - SOURCE-HEADING-POINT: Position of source file title
  - HIGHLIGHTS: List of highlights to add or update
  
  Input: Title position and list of highlights
  Output: No return value, but creates or updates the highlights
  
  This function creates or updates the corresponding secondary title entry for each highlight."""
  (when highlights
    (dolist (highlight (sort highlights (lambda (a b) (< (string-to-number (car a)) (string-to-number (car b))))))
      (let* ((original-hl-id (nth 0 highlight))
             (hl-type-sym (nth 1 highlight))
             (hl-text-content (nth 2 highlight))
             (hl-type-name (nth 3 highlight))
             (hl-prefix (nth 4 highlight))
             (hl-img-path (nth 5 highlight))
             (hl-img-desc (nth 6 highlight))
             (display-text (if (eq hl-type-sym 'image) (or hl-img-desc hl-type-name) hl-text-content))
             (found-existing nil))
        
        ;; Find existing highlight entry
        (save-excursion
          (goto-char source-heading-point)
          (org-back-to-heading t)
          (org-narrow-to-subtree)
          (goto-char (point-min))
          
          (while (and (not found-existing) (re-search-forward "^\\** .*$" nil t))
            (let ((props (org-entry-properties)))
              (when (string= (cdr (assoc "ORIGINAL_HL_ID" props)) original-hl-id)
                ;; Found existing entry, update content
                (setq found-existing t)
                (let* ((heading-start (line-beginning-position))
                       (heading-end (line-end-position))
                       (expected-heading (format "** %s %s" hl-prefix display-text)))
                  (delete-region heading-start heading-end)
                  (insert expected-heading)
                  ;; Process image content
                  (when (and (eq hl-type-sym 'image) hl-img-path)
                    (org-end-of-meta-data t)
                    (unless (looking-at "\\(#\\+ATTR_ORG:.*\n\\)?\\[\\[file:")
                      (insert (format "\n#+ATTR_ORG: :width 300\n[[file:%s]]\n" hl-img-path))))
                  (message "DEBUG: Updated highlight entry: %s" original-hl-id)))))
          (widen))
        
        ;; If not found, create new
        (unless found-existing
          (save-excursion
            (goto-char source-heading-point)
            (org-back-to-heading t)
            (org-end-of-subtree)
            (unless (bolp) (insert "\n"))
            (insert (format "** %s %s\n" hl-prefix display-text))
            (org-entry-put nil "SOURCE_REF_ID" (org-entry-get source-heading-point "REF_ID"))
            (org-entry-put nil "ORIGINAL_HL_ID" original-hl-id)
            
            ;; Process image type
            (when (and (eq hl-type-sym 'image) hl-img-path)
              (insert (format "\n#+ATTR_ORG: :width 300\n[[file:%s]]\n" hl-img-path)))
            (message "DEBUG: Created new highlight entry: %s" original-hl-id)))))))

(defun org-zettel-ref--remove-all-ref-id-content (source-ref-id)
  "Remove all content related to the specified REF_ID from the overview file.
  
  DEPRECATED: This function will delete user-added content, not recommended.
  Please use incremental synchronization strategy.
  
  Parameters:
  - SOURCE-REF-ID: Source file reference ID to remove
  
  Input: Source file reference ID
  Output: No return value, but deletes the corresponding content
  
  This function removes all content related to the specified REF_ID from the overview file."""
  (message "WARNING: org-zettel-ref--remove-all-ref-id-content is deprecated and should not be used")
  (let ((removed-count 0)
        (continue t))
    (message "DEBUG: Start removing all REF_ID related content: %s" source-ref-id)
    
    ;; Scan repeatedly until no more matches
    (while continue
      (setq continue nil)
      (goto-char (point-min))
      (while (and (not continue) (re-search-forward "^\\* .*$" nil t))
        (let ((heading-start (line-beginning-position)))
          (save-excursion
            (condition-case err
                (let ((props (org-entry-properties)))
                  (when (string= (cdr (assoc "REF_ID" props)) source-ref-id)
                    ;; Found
                    (goto-char heading-start)
                    (org-back-to-heading t)
                    (let ((subtree-end (save-excursion 
                                         (condition-case end-err
                                             (org-end-of-subtree t t)
                                           (error
                                            ;; If org-end-of-subtree fails, find next sibling or higher heading
                                            (forward-line 1)
                                            (if (re-search-forward "^\\* " nil t)
                                                (line-beginning-position)
                                              (point-max)))))))
                      (delete-region (point) subtree-end)
                      (cl-incf removed-count)
                      (message "DEBUG: Deleted REF_ID title and content: %s" source-ref-id)
                      ;; Mark to restart search
                      (setq continue t))))
              (error
               (message "DEBUG: Error getting properties, position: %s, error: %s" heading-start (error-message-string err))))))))
    
    (message "DEBUG: Deletion complete, removed %d REF_ID entries: %s" removed-count source-ref-id)))

(defun org-zettel-ref-sync-highlights ()
  "Synchronize all highlights to the overview file.
Supports both multi-file and single-file overview modes."
  (interactive)
  (message "DEBUG: Starting sync in buffer: %s" (buffer-name))
  (message "DEBUG: Current overview file (buffer-local): %s" org-zettel-ref-overview-file)
  (message "DEBUG: Single-file mode active: %s" org-zettel-ref-use-single-overview-file)

  (let ((highlights (org-zettel-ref--collect-highlights-from-source)))
    (message "DEBUG: Collected %d highlights from source buffer." (length highlights))

    (if org-zettel-ref-use-single-overview-file
        ;; --- Single-File Mode ---
        (when (and org-zettel-ref-overview-file
                   (file-exists-p org-zettel-ref-overview-file)
                   (boundp 'org-zettel-ref-current-ref-entry)
                   org-zettel-ref-current-ref-entry)
          (let* ((source-ref-entry org-zettel-ref-current-ref-entry)
                 (source-ref-id (org-zettel-ref-ref-entry-id source-ref-entry))
                 (source-file-path (org-zettel-ref-ref-entry-file-path source-ref-entry))
                 (source-title (or (org-zettel-ref-ref-entry-title source-ref-entry) (file-name-base source-file-path)))
                 ;; Highlights list is now passed from the helper
                 (org-element-use-cache nil))

            (org-zettel-ref-debug-message-category 'core
                                                   "Single-File Sync: Processing %d highlights for source %s ('%s')"
                                                   (length highlights) source-ref-id source-title)

            ;; 2. Update the single overview file
            (org-zettel-ref--sync-highlights-single-file highlights
                                                       org-zettel-ref-overview-file
                                                       source-ref-id
                                                       source-file-path
                                                       source-title
                                                       org-zettel-ref-overview-buffer)))

        ;; --- Multi-File Mode ---
        (org-zettel-ref--sync-highlights-multi-file highlights
                                                    org-zettel-ref-overview-file
                                                    org-zettel-ref-overview-buffer))))


;;----------------------------------------------------------------
;; File namming
;;----------------------------------------------------------------

(defun org-zettel-ref-generate-filename (title &optional ref-id)
  "Generate overview file name based on stable REF_ID, without using time-based suffix.
  
  Parameters:
  - TITLE: File title
  - REF-ID: Optional reference ID, used as stable identifier if provided
  
  Input: Title string and optional reference ID
  Output: Stable overview file name
  
  This function avoids using timestamps, instead generating stable file names based on REF_ID,
  ensuring that the same source file does not produce multiple overview files."""
  (if ref-id
      ;; Use REF_ID as stable identifier
      (format "%s--%s%s" 
              ref-id
              title
              org-zettel-ref-overview-file-suffix)
    ;; Fall back to timestamp (only used when no REF_ID)
    (let ((timestamp (format-time-string "%Y%m%dT%H%M%S")))
      (format "%s--%s%s" 
              timestamp
              title
              org-zettel-ref-overview-file-suffix))))

(defun org-zettel-ref-normalize-filename (file)
  "Normalize FILE name according to the standard format.
Returns nil if no changes needed, or new filepath if changes required."
  (let* ((dir (file-name-directory file))
         (filename (file-name-nondirectory file))
         (components (org-zettel-ref-parse-filename filename))
         (author (nth 0 components))
         (title (nth 1 components))
         (keywords (nth 2 components))
         (new-filename (org-zettel-ref-format-filename author title keywords))
         (new-filepath (expand-file-name new-filename dir)))
    (unless (equal filename new-filename)
      new-filepath)))

(defun org-zettel-ref-maybe-normalize-file (file)
  "Check and normalize FILE name if needed."
  (when-let* ((db (org-zettel-ref-ensure-db))
              (new-filepath (org-zettel-ref-normalize-filename file)))
    
    (when (and (not (equal file new-filepath))
               (y-or-n-p (format "Normalize filename from %s to %s? "
                                (file-name-nondirectory file)
                                (file-name-nondirectory new-filepath))))
      (org-zettel-ref-unwatch-directory)
      (condition-case err
          (let* ((ref-id (org-zettel-ref-db-get-ref-id-by-path db file))
                 (ref-entry (when ref-id (org-zettel-ref-db-get-ref-entry db ref-id))))
            (rename-file file new-filepath t)
            (when ref-entry
              (org-zettel-ref-db-update-ref-path db file new-filepath)
              (setf (org-zettel-ref-ref-entry-file-path ref-entry) new-filepath)
              (org-zettel-ref-db-update-ref-entry db ref-entry)
              (org-zettel-ref-db-save db)
              (set-visited-file-name new-filepath)
              (set-buffer-modified-p nil))
        (error
         (message "Error during rename: %s" (error-message-string err))))
      (run-with-timer 0.5 nil #'org-zettel-ref-watch-directory)))))

(defun org-zettel-ref-rename-source-file ()
  "Rename the current source file according to the standard format."
  (interactive)
  (let* ((db (org-zettel-ref-ensure-db))
         (old-file (buffer-file-name)))
    (if (not old-file)
        (message "Current buffer is not associated with a file")
      (let* ((dir (file-name-directory old-file))
             (ref-id (org-zettel-ref-db-get-ref-id-by-path db old-file)))
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
              (org-zettel-ref-unwatch-directory)
              (condition-case err
                  (progn
                    (rename-file old-file new-file-path t)
                    (org-zettel-ref-db-update-ref-path db old-file new-file-path)
                    (setf (org-zettel-ref-ref-entry-file-path ref-entry) new-file-path
                          (org-zettel-ref-ref-entry-title ref-entry) new-title
                          (org-zettel-ref-ref-entry-author ref-entry) new-author
                          (org-zettel-ref-ref-entry-keywords ref-entry) new-keywords)
                    (org-zettel-ref-db-update-ref-entry db ref-entry)
                    (org-zettel-ref-db-save db)
                    (set-visited-file-name new-file-path)
                    (set-buffer-modified-p nil)
                    (message "File renamed from %s to %s"
                             (file-name-nondirectory old-file)
                             (file-name-nondirectory new-file-path)))
                (error
                 (message "Error during rename: %s" (error-message-string err))))
              (run-with-timer 0.5 nil #'org-zettel-ref-watch-directory))))))))

;;------------------------------------------------------------------
;; Initialization
;;------------------------------------------------------------------

(defun org-zettel-ref-init ()
  "Initialize org-zettel-ref for current buffer."
  (interactive)
  (message "DEBUG: Starting initialization for buffer: %s" (buffer-name))
  (when (buffer-file-name)
    (let* ((source-buffer (current-buffer))
           (entry-pair (org-zettel-ref-ensure-entry source-buffer)))
      (message "DEBUG: Got entry pair: %S" entry-pair)
      (when entry-pair
        (let* ((overview-file (cdr entry-pair))
               (overview-buffer-name (format "*Overview: %s*" 
                                          (file-name-base (buffer-file-name))))
               (overview-buffer (org-zettel-ref-setup-overview-window 
                               overview-file 
                               overview-buffer-name)))
          (message "DEBUG: Created overview buffer: %s" overview-buffer)
          
          ;; Activate buffers
          (org-zettel-ref--activate-buffer source-buffer)
          (org-zettel-ref--activate-buffer overview-buffer)
          
          ;; Set buffer relationship
          (with-current-buffer overview-buffer
            (setq-local org-zettel-ref-source-buffer source-buffer)
            (setq-local org-zettel-ref-overview-file overview-file)
            (message "DEBUG: Set overview file in overview buffer: %s" org-zettel-ref-overview-file)
            (setq org-zettel-ref-current-overview-buffer overview-buffer))
          
          (with-current-buffer source-buffer
            (setq-local org-zettel-ref-current-ref-entry (car entry-pair))
            (setq-local org-zettel-ref-overview-file overview-file)
            (setq-local org-zettel-ref-overview-buffer overview-buffer)
            (message "DEBUG: Set overview file in source buffer: %s" org-zettel-ref-overview-file))

          ;; If in single-file mode, navigate to the relevant section in the overview
          (when (and org-zettel-ref-use-single-overview-file
                     (buffer-live-p overview-buffer)
                     (buffer-live-p source-buffer))
            (let* ((current-source-ref-entry (car entry-pair))
                   (current-source-ref-id (org-zettel-ref-ref-entry-id current-source-ref-entry)))
              (when current-source-ref-id
                (with-current-buffer overview-buffer
                  (let ((target-heading-point nil))
                    (org-map-entries
                     (lambda ()
                       (setq target-heading-point (point-marker))
                       t) ; Stop after first match
                     (format "REF_ID=\"%s\"" current-source-ref-id)
                     'file) ; Search scope
                    (when target-heading-point
                      (goto-char target-heading-point)
                      (org-reveal) ; Or (recenter (point))
                      (message "Navigated to section for %s in single overview."
                               (org-zettel-ref-ref-entry-title current-source-ref-entry))))))))
          
          ;; Set highlights
          (with-current-buffer source-buffer
            (org-zettel-ref-highlight-setup)
            (add-hook 'after-save-hook #'org-zettel-ref-sync-highlights nil t)
            (add-hook 'after-change-functions #'org-zettel-ref-highlight-after-change nil t))
          
          (with-current-buffer overview-buffer
            (org-zettel-ref-highlight-setup))
          
          ;; Run initialization hook
          (run-hooks 'org-zettel-ref-init-hook)
          
          (message "Initialized org-zettel-ref for %s" (buffer-name))
          overview-file)))))

(defun org-zettel-ref-ensure-entry (source-buffer)
  "Ensure database entries exist for the source buffer.
Return (ref-entry . overview-file) pair."
  (message "DEBUG: Ensuring entry for buffer: %s" (buffer-name source-buffer))
  (let* ((source-file (buffer-file-name source-buffer))
         (abs-source-file (expand-file-name source-file))
         (db (org-zettel-ref-ensure-db))
         (file-name (file-name-nondirectory source-file))
         (parsed-info (org-zettel-ref-parse-filename file-name))
         (ref-entry (org-zettel-ref-db-ensure-ref-entry db abs-source-file))
         (ref-id (org-zettel-ref-ref-entry-id ref-entry))
         overview-id ; Will be determined differently based on mode
         overview-file)

    (message "DEBUG: Source file: %s" source-file)
    (message "DEBUG: Ref ID: %s" ref-id)

    (if org-zettel-ref-use-single-overview-file
        (progn
          (unless (and org-zettel-ref-single-overview-file-path
                       (stringp org-zettel-ref-single-overview-file-path)
                       (not (string-empty-p org-zettel-ref-single-overview-file-path)))
            (message "Single overview file path is not configured. Please set `org-zettel-ref-single-overview-file-path`."))
          (setq overview-file (expand-file-name org-zettel-ref-single-overview-file-path))
          (message "DEBUG: Single-file mode. Using overview file: %s" overview-file)
          
          ;; Create overview file if it doesn't exist
          (unless (file-exists-p overview-file)
            (make-directory (file-name-directory overview-file) t)
            (with-temp-file overview-file
              (insert "#+TITLE: Unified Org Zettel Ref Overview\n")
              (insert (format "#+AUTHOR: %s\n" (user-full-name)))
              (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d")))
              (insert "#+STARTUP: content\n\n"))
            (message "DEBUG: Created new single overview file: %s" overview-file))

          ;; Ensure the source heading exists in the overview file
          (let ((overview-buffer (org-zettel-ref--safe-find-file-noselect overview-file t)))
            (with-current-buffer overview-buffer
              (setq-local org-element-use-cache nil)
              (when (fboundp 'org-element-cache-reset) 
                (org-element-cache-reset))
              (org-with-wide-buffer
               ;; First ensure we're in org-mode
               (unless (eq major-mode 'org-mode)
                 (org-mode))
               
               ;; Reset org-element cache to ensure fresh parsing
               (when (boundp 'org-element-use-cache)
                 (setq-local org-element-use-cache nil))
               (when (fboundp 'org-element-cache-reset)
                 (org-element-cache-reset))

               ;; Clean duplicate headings and check if they exist
               (let ((cleaned-heading-pos (org-zettel-ref--cleanup-duplicate-headings ref-id)))
                 (unless cleaned-heading-pos
                   ;; If no headings after cleanup, create new headings
                   (message "DEBUG: Initialization creates new source file heading, REF_ID: %s" ref-id)
                   (goto-char (point-max))
                   (unless (bolp) (insert "\n"))
                   (insert (format "* [[ref:%s][%s]]\n" 
                                 ref-id
                                 (or (org-zettel-ref-ref-entry-title ref-entry)
                                     (file-name-base abs-source-file))))
                   (org-entry-put nil "REF_ID" ref-id)
                   (org-entry-put nil "SOURCE_FILE_PATH" (format "file:%s" abs-source-file))
                   (save-buffer))
                 (message "DEBUG: Initialization complete, heading status confirmed, REF_ID: %s" ref-id)))
              (message "DEBUG: Ensured source heading exists in overview file")))

          ;; Get overview ID from DB map
          (setq overview-id (org-zettel-ref-db-get-maps db ref-id))
          (message "DEBUG: Single-file mode - Overview ID from DB map for this ref: %s" overview-id)
          t)
      
      ;; Multi-file mode logic (unchanged)
      (progn
        (message "DEBUG: Multi-file mode.")
        (setq overview-id (org-zettel-ref-db-get-maps db ref-id))
        (message "DEBUG: Multi-file mode - Overview ID from DB map: %s" overview-id)
        (unless (file-exists-p org-zettel-ref-overview-directory)
          (make-directory org-zettel-ref-overview-directory t))

        (setq overview-file
              (if overview-id
                  (when-let* ((existing-overview (org-zettel-ref-db-get-overview-by-ref-id db ref-id))
                             (file-path (org-zettel-ref-overview-entry-file-path existing-overview)))
                    (message "DEBUG: Found existing overview file (multi-mode): %s" file-path)
                    (if (file-exists-p file-path)
                        file-path
                      (progn
                        (message "DEBUG: Mapped overview file (multi-mode) '%s' doesn't exist. Clearing old DB map and overview entry." file-path)
                        (when existing-overview (remhash (org-zettel-ref-overview-entry-id existing-overview) (org-zettel-ref-db-overviews db)))
                        (when existing-overview (remhash file-path (org-zettel-ref-db-overview-paths db)))
                        (remhash ref-id (org-zettel-ref-db-map db))
                        nil)))
                ;; If no existing mapping, try to find possible overview files
                ;; This prevents losing overview file associations after renaming
                (let* ((title (org-zettel-ref-ref-entry-title ref-entry))
                       (overview-filename (org-zettel-ref-generate-filename title ref-id))
                       (target-file (expand-file-name overview-filename org-zettel-ref-overview-directory))
                       ;; Try to find overview files based on original ref-id pattern
                       (ref-id-pattern (format "*%s*" ref-id))
                       (possible-overview-files (when (file-exists-p org-zettel-ref-overview-directory)
                                                 (directory-files org-zettel-ref-overview-directory 
                                                                t 
                                                                (concat ".*" (regexp-quote "__overview\\.org") "$")))))
                  (message "DEBUG: Creating new overview file (multi-mode): %s" target-file)
                  
                  ;; Check if there are any orphaned overview files that might belong to this ref-id
                  (let ((orphaned-overview nil))
                    (dolist (possible-file possible-overview-files)
                      ;; Check if the file content contains a reference to this ref-id
                      (when (file-exists-p possible-file)
                        (with-temp-buffer
                          (insert-file-contents possible-file)
                          (when (re-search-forward (format "SOURCE_FILE:.*%s" (regexp-quote (file-name-nondirectory abs-source-file))) nil t)
                            (setq orphaned-overview possible-file)
                            (message "DEBUG: Found orphaned overview file that matches source: %s" possible-file)))))
                    
                    (if orphaned-overview
                        ;; Re-establish association with orphaned overview file
                        (progn
                          (message "DEBUG: Re-establishing link to orphaned overview file: %s" orphaned-overview)
                          (let* ((overview-entry (org-zettel-ref-db-create-overview-entry
                                                 db
                                                 ref-id
                                                 orphaned-overview
                                                 title)))
                            (org-zettel-ref-db-add-overview-entry db overview-entry)
                            (org-zettel-ref-db-add-map db ref-id (org-zettel-ref-overview-entry-id overview-entry))
                            (org-zettel-ref-db-save db))
                          orphaned-overview)
                      ;; Create new overview file
                      (progn
                        (unless (file-exists-p target-file)
                          (let* ((new-file-content (org-zettel-ref-create-overview-file source-buffer target-file ref-entry))
                                 (new-overview-entry (org-zettel-ref-db-create-overview-entry
                                                    db
                                                    ref-id
                                                    target-file
                                                    title)))
                            (org-zettel-ref-db-add-overview-entry db new-overview-entry)
                            (org-zettel-ref-db-add-map db ref-id (org-zettel-ref-overview-entry-id new-overview-entry))
                            (org-zettel-ref-db-save db)))
                        target-file))))))
        (message "DEBUG: Multi-file mode logic complete. Overview file determined as: %s" overview-file)
        t))

    (let ((final-overview-file overview-file)
          (final-ref-entry ref-entry))
      (message "DEBUG: Pre-cons: Final overview file: %s" final-overview-file)
      (message "DEBUG: Pre-cons: Final ref-entry ID: %s" 
               (if final-ref-entry 
                   (org-zettel-ref-ref-entry-id final-ref-entry) 
                 "NIL ref-entry"))
      (if (and final-ref-entry final-overview-file)
          (cons final-ref-entry final-overview-file)
        (progn
          (message "ERROR: org-zettel-ref-ensure-entry is returning NIL because final-ref-entry or final-overview-file is nil.")
          nil)))))

(defun org-zettel-ref-setup-overview-window (overview-file buffer-name)
  "Setup a window for OVERVIEW-FILE with BUFFER-NAME.
Returns the overview buffer."
  (let* ((source-window (selected-window))
         (source-width (window-width source-window))
         ;; calculate target windows width
         (target-width (max org-zettel-ref-overview-min-width
                           (round (* source-width org-zettel-ref-overview-width-ratio))))
         (overview-window (split-window-right))
         ;; display overview file in new window with robust error handling
         (overview-buffer (org-zettel-ref--safe-find-file-noselect overview-file t)))
    ;; switch to overview window and display buffer
    (select-window overview-window)
    (switch-to-buffer overview-buffer)
    ;; adjust window size
    (let ((width-delta (- target-width (window-width))))
      (when (not (zerop width-delta))
        (window-resize overview-window width-delta t)))
    ;; switch back to source window
    (select-window source-window)
    (set-window-dedicated-p overview-window t)
    overview-buffer))

;;------------------------------------------------------------------
;; Overview File Creation
;;------------------------------------------------------------------
(defun org-zettel-ref-get-normal-overview (source-buffer overview-file ref-entry)
  "Create an overview file for SOURCE-BUFFER in normal mode using REF-ENTRY metadata."
  (let* ((title (org-zettel-ref-ref-entry-title ref-entry))
         (source-file (org-zettel-ref-ref-entry-file-path ref-entry))
         (author (org-zettel-ref-ref-entry-author ref-entry)))
    (unless (file-exists-p overview-file)
      (with-temp-file overview-file
        (insert (format "#+TITLE: Overview - %s\n" title))
        (when author
          (insert (format "#+AUTHOR: %s\n" author)))
        (insert (format "#+SOURCE_FILE: %s\n" source-file))
        (insert "#+filetags: :overview:\n")
        (insert "#+startup: showall\n\n")))
    overview-file))

(defun org-zettel-ref-get-overview-file-org-roam (source-buffer overview-file ref-entry)
  "Use org-roam mode to get or create an overview file for SOURCE-BUFFER using REF-ENTRY metadata."
  (let* ((title (org-zettel-ref-ref-entry-title ref-entry))
         (source-file (org-zettel-ref-ref-entry-file-path ref-entry))
         (author (org-zettel-ref-ref-entry-author ref-entry))
         (org-id (org-id-new)))
    (unless (file-exists-p overview-file)
      (with-temp-file overview-file
        (insert (format ":PROPERTIES:\n:ID:       %s\n:END:\n" org-id))
        (insert (format "#+TITLE: Overview - %s\n" title))
        (when author
          (insert (format "#+AUTHOR: %s\n" author)))
        (insert (format "#+SOURCE_FILE: %s\n" source-file))
        (insert "#+filetags: :overview:\n")
        (insert "#+startup: showall\n\n")))
    (when (and (featurep 'org-roam)
               (fboundp 'org-roam-db-update-file))
      (org-roam-db-update-file overview-file))
    overview-file))

(defun org-zettel-ref-get-overview-file-denote (source-buffer overview-file ref-entry)
  "Get or create an overview file for SOURCE-BUFFER using Denote mode and REF-ENTRY metadata."
  (let* ((title (org-zettel-ref-ref-entry-title ref-entry))
         (source-file (org-zettel-ref-ref-entry-file-path ref-entry))
         (author (org-zettel-ref-ref-entry-author ref-entry)))
    (unless (file-exists-p overview-file)
      (with-temp-file overview-file
        (insert (format "#+TITLE: Overview - %s\n" title))
        (when author
          (insert (format "#+AUTHOR: %s\n" author)))
        (insert (format "#+SOURCE_FILE: %s\n" source-file))
        (insert "#+filetags: :overview:\n")
        (insert "#+startup: showall\n\n")))
    overview-file))

(defun org-zettel-ref-cleanup-all-duplicates ()
  "Clean all duplicate titles in the overview file.
This function scans the entire overview file, cleaning all duplicate titles with REF_ID."
  (interactive)
  (if (not org-zettel-ref-use-single-overview-file)
      (message "This feature is only available in single file mode")
    (let* ((overview-file (expand-file-name org-zettel-ref-single-overview-file-path))
           (overview-buffer (when (file-exists-p overview-file)
                             (org-zettel-ref--safe-find-file-noselect overview-file nil))))
      (if (not overview-buffer)
          (message "Overview file not found: %s" overview-file)
        (with-current-buffer overview-buffer
          (org-with-wide-buffer
           (let ((all-ref-ids '())
                 (cleaned-count 0))
             ;; Collect all REF_IDs
             (save-excursion
               (goto-char (point-min))
               (while (re-search-forward "^\\* .*$" nil t)
                 (let ((props (org-entry-properties)))
                   (when-let ((ref-id (cdr (assoc "REF_ID" props))))
                     (push ref-id all-ref-ids)))))
             
            ;; Remove duplicates
             (setq all-ref-ids (delete-dups all-ref-ids))
             (message "Found %d different REF_IDs" (length all-ref-ids))
             
             ;; Clean each one by one
             (dolist (ref-id all-ref-ids)
               (let ((cleaned-pos (org-zettel-ref--cleanup-duplicate-headings ref-id)))
                 (when cleaned-pos
                   (cl-incf cleaned-count))))
             
             (when (> cleaned-count 0)
               (save-buffer)
               (message "Cleanup complete! Cleaned %d duplicate titles with REF_ID" cleaned-count))
             (when (= cleaned-count 0)
               (message "No duplicate titles found")))))))))

;;------------------------------------------------------------------
;; REF_ID 链接支持
;;------------------------------------------------------------------

(defun org-zettel-ref-open-ref-link (ref-id)
  "Open the corresponding source file via REF_ID.
REF_ID is the unique identifier for the reference entry."
  (let* ((db (org-zettel-ref-ensure-db))
         (ref-entry (org-zettel-ref-db-get-ref-entry db ref-id)))
    (if (not ref-entry)
        (message "No entry found for REF_ID: %s" ref-id)
      (let ((file-path (org-zettel-ref-ref-entry-file-path ref-entry)))
        (if (file-exists-p file-path)
            (find-file file-path)
          (message "File not found: %s" file-path))))))

(defun org-zettel-ref-export-ref-link (ref-id desc backend)
  "Export ref: link.
REF_ID is the reference ID, DESC is the description, BACKEND is the export backend."
  (let* ((db (org-zettel-ref-ensure-db))
         (ref-entry (org-zettel-ref-db-get-ref-entry db ref-id))
         (file-path (when ref-entry (org-zettel-ref-ref-entry-file-path ref-entry)))
         (title (or desc 
                   (when ref-entry (org-zettel-ref-ref-entry-title ref-entry))
                   ref-id)))
    (pcase backend
      ('html (format "<a href=\"file:%s\">%s</a>" 
                    (or file-path "#") title))
      ('latex (format "\\href{file:%s}{%s}" 
                     (or file-path "") title))
      (_ (format "[%s]" title)))))

(defun org-zettel-ref-complete-ref-link (&optional arg)
  "Provide completion for ref: link.
ARG is an optional argument."
  (let* ((db (org-zettel-ref-ensure-db))
         (candidates '()))
    ;; Collect all REF_ID and titles
    (maphash
     (lambda (ref-id ref-entry)
       (let ((title (org-zettel-ref-ref-entry-title ref-entry)))
         (push (format "%s %s" ref-id (or title "No title")) candidates)))
     (org-zettel-ref-db-refs db))
    ;; Provide completion
    (let ((choice (completing-read "Select reference: " candidates nil t)))
      (when choice
        (car (split-string choice " " t))))))

;; Register custom link types
(with-eval-after-load 'org
  (org-link-set-parameters
   "ref"
   :follow #'org-zettel-ref-open-ref-link
   :export #'org-zettel-ref-export-ref-link
   :complete #'org-zettel-ref-complete-ref-link))

;;------------------------------------------------------------------
;; Link migration tool
;;------------------------------------------------------------------

(defun org-zettel-ref-migrate-file-links-to-ref-links ()
  "Migrate file: links in single overview file to ref: links.
This function is used to solve the problem of link failure caused by file name changes."
  (interactive)
  (when (not org-zettel-ref-use-single-overview-file)
    (user-error "This feature is only available in single file mode"))
  
  (let* ((overview-file (expand-file-name org-zettel-ref-single-overview-file-path))
         (db (org-zettel-ref-ensure-db))
         (changed-count 0))
    
    (unless (file-exists-p overview-file)
      (user-error "Overview file not found: %s" overview-file))
    
    (with-current-buffer (find-file-noselect overview-file)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\* \\[\\[file:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" nil t)
          (let* ((file-path (match-string 1))
                 (title (match-string 2))
                 (ref-id (org-entry-get nil "REF_ID")))
            
            (when ref-id
              ;; Verify if REF_ID exists in the database
              (let ((ref-entry (org-zettel-ref-db-get-ref-entry db ref-id)))
                (when ref-entry
                  ;; Replace link
                  (replace-match (format "* [[ref:%s][%s]]" ref-id title))
                  (setq changed-count (1+ changed-count))
                  (message "Migrated link: %s -> ref:%s" (file-name-nondirectory file-path) ref-id))))))
        
        (when (> changed-count 0)
          (save-buffer)
          (message "Successfully migrated %d file links to REF_ID links" changed-count))
        (when (= changed-count 0)
          (message "No links to migrate found"))))))

(defun org-zettel-ref-cleanup-duplicate-overview-files ()
  "Clean duplicate overview files caused by timestamp naming strategy.
  
  This function will:
  1. Scan all overview files in the overview directory
  2. Identify multiple overview files belonging to the same source file
  3. Keep the latest one, delete the others
  4. Update the database mapping relationship
  
  Only available in multi-file mode."
  (interactive)
  (when org-zettel-ref-use-single-overview-file
    (user-error "This feature is only available in multi-file mode"))
  
  (unless (file-exists-p org-zettel-ref-overview-directory)
    (user-error "Overview directory not found: %s" org-zettel-ref-overview-directory))
  
  (let* ((db (org-zettel-ref-ensure-db))
         (overview-files (directory-files org-zettel-ref-overview-directory 
                                        t 
                                        (concat ".*" (regexp-quote "__overview\\.org") "$")))
         (file-groups (make-hash-table :test 'equal))
         (cleaned-count 0))
    
    (message "Start cleaning duplicate overview files...")
    (message "Found %d overview files" (length overview-files))
    
    ;; Group overview files by source file
    (dolist (overview-file overview-files)
      (let* ((filename (file-name-nondirectory overview-file))
             ;; Extract title part (remove timestamp and suffix)
             (title-part (when (string-match "--\\(.+\\)__overview\\.org$" filename)
                          (match-string 1 filename))))
        (when title-part
          (let ((existing-files (gethash title-part file-groups)))
            (puthash title-part (cons overview-file existing-files) file-groups)))))
    
    ;; Process each group
    (maphash
     (lambda (title-part files)
       (when (> (length files) 1)
         (message "Found %d duplicate overview files, title: %s" (length files) title-part)
         
         ;; Sort by modification time, keep the latest
         (let* ((sorted-files (sort files 
                                  (lambda (a b)
                                    (time-less-p 
                                     (file-attribute-modification-time (file-attributes a))
                                     (file-attribute-modification-time (file-attributes b))))))
                (keep-file (car (last sorted-files)))  ; The latest file
                (delete-files (butlast sorted-files))) ; Other files
           
           (message "Keep file: %s" (file-name-nondirectory keep-file))
           
           ;; Delete duplicate files and update database
           (dolist (delete-file delete-files)
             (message "Delete duplicate file: %s" (file-name-nondirectory delete-file))
             
             ;; Clean related entries from the database
             (let ((overview-id-to-remove nil))
               (maphash
                (lambda (overview-id overview-entry)
                  (when (string= (org-zettel-ref-overview-entry-file-path overview-entry) delete-file)
                    (setq overview-id-to-remove overview-id)))
                (org-zettel-ref-db-overviews db))
               
               (when overview-id-to-remove
                 (remhash overview-id-to-remove (org-zettel-ref-db-overviews db))
                 (remhash delete-file (org-zettel-ref-db-overview-paths db))
                 (message "清理数据库条目: %s" overview-id-to-remove)))
             
             ;; Delete
             (when (file-exists-p delete-file)
               (delete-file delete-file)
               (cl-incf cleaned-count))))))
     file-groups)
    
    ;; Save database  
    (when (> cleaned-count 0)
      (org-zettel-ref-db-save db)
      (message "Cleanup complete! Deleted %d duplicate overview files" cleaned-count))
    (when (= cleaned-count 0)
      (message "No duplicate overview files found"))))

(defun org-zettel-ref-test-cleanup-ref-id (ref-id)
  "Manually test cleanup of all content for a specific REF_ID.
  REF_ID is the unique identifier for the reference entry."
  (interactive "sEnter REF_ID to cleanup: ")
  (unless org-zettel-ref-use-single-overview-file
    (user-error "This feature is only available in single file mode"))
  
  (let* ((overview-file (expand-file-name org-zettel-ref-single-overview-file-path))
         (overview-buffer (when (file-exists-p overview-file)
                           (org-zettel-ref--safe-find-file-noselect overview-file nil))))
    (if (not overview-buffer)
        (message "Overview file not found: %s" overview-file)
      (with-current-buffer overview-buffer
        (org-with-wide-buffer
         (org-zettel-ref--remove-all-ref-id-content ref-id)
         (save-buffer))
        (message "Test cleanup complete: REF_ID %s" ref-id)))))

(defun org-zettel-ref-test-incremental-sync ()
  "Test if the incremental sync feature is working properly.
  
  This function will display the current source file's highlight status and change detection results."
  (interactive)
  (unless org-zettel-ref-use-single-overview-file
    (user-error "This feature is only available in single file mode"))
  
  (when (and (buffer-file-name)
             (boundp 'org-zettel-ref-current-ref-entry)
             org-zettel-ref-current-ref-entry)
    (let* ((db (org-zettel-ref-ensure-db))
           (source-ref-id (org-zettel-ref-ref-entry-id org-zettel-ref-current-ref-entry))
           (current-highlights (org-zettel-ref--collect-highlights-from-source))
           (changes (org-zettel-ref--detect-highlight-changes current-highlights db source-ref-id)))
      
      (message "=== Incremental sync test results ===")
      (message "Source file: %s" (buffer-file-name))
      (message "REF_ID: %s" source-ref-id)
      (message "Current highlight count: %d" (length current-highlights))
      (message "Change statistics:")
      (message "  - Added: %d" (length (plist-get changes :added)))
      (message "  - Removed: %d" (length (plist-get changes :removed)))
      (message "  - Updated: %d" (length (plist-get changes :updated)))
      (message "  - Unchanged: %d" (length (plist-get changes :unchanged)))
      
      (when (plist-get changes :added)
        (message "New highlights:")
        (dolist (hl (plist-get changes :added))
          (message "  + %s: %s" (nth 0 hl) (nth 2 hl))))
      
      (when (plist-get changes :removed)
        (message "Deleted highlights:")
        (dolist (hl (plist-get changes :removed))
          (message "  - %s: %s" (nth 0 hl) (nth 2 hl))))
      
      (when (plist-get changes :updated)
        (message "Updated highlights:")
        (dolist (hl (plist-get changes :updated))
          (message "  * %s: %s" (nth 0 hl) (nth 2 hl))))
      
      (message "========================"))))

;;------------------------------------------------------------------
;; Highlight tracking data structure
;;------------------------------------------------------------------

(cl-defstruct org-zettel-ref-highlight-state
  "Highlight tracking structure"
  (highlights (make-hash-table :test 'equal))  ; Highlight ID -> Highlight information mapping
  (last-sync-time nil)                         ; Last sync time
  (checksum nil))                              ; Content checksum

(defun org-zettel-ref-calculate-highlights-checksum (highlights)
  "Calculate the checksum of the highlight list, used to detect content changes.
  
  Parameters:
  - HIGHLIGHTS: Highlight content list
  
  Input: Highlight content list
  Output: Checksum string
  
  This function calculates the MD5 checksum based on the highlight ID, type, and content,
  used to quickly detect if the highlight content has changed."
  (let ((content-string 
         (mapconcat 
          (lambda (hl) 
            (format "%s:%s:%s" (nth 0 hl) (nth 1 hl) (nth 2 hl)))
          (sort highlights (lambda (a b) (string< (car a) (car b))))
          "|")))
    (md5 content-string)))

(defun org-zettel-ref-get-highlight-state (db source-ref-id)
  "Get the highlight state of the specified source file from the database.
  
  Parameters:
  - DB: Database instance
  - SOURCE-REF-ID: Source file reference ID
  
  Input: Database instance and source file reference ID
  Output: Highlight state structure, nil if not found
  
  This function retrieves highlight state information from the user data area of the database."
  (when-let* ((ref-entry (org-zettel-ref-db-get-ref-entry db source-ref-id)))
    (org-zettel-ref-ref-entry-get-user-data ref-entry 'highlight-state)))

(defun org-zettel-ref-set-highlight-state (db source-ref-id state)
  "Set the highlight state of the specified source file in the database.
  
  Parameters:
  - DB: Database instance
  - SOURCE-REF-ID: Source file reference ID
  - STATE: Highlight state structure
  
  Input: Database instance, source file reference ID, and state structure
  Output: No return value, but updates the database
  
  This function saves highlight state information to the user data area of the database."
  (when-let* ((ref-entry (org-zettel-ref-db-get-ref-entry db source-ref-id)))
    (org-zettel-ref-ref-entry-set-user-data ref-entry 'highlight-state state)
    (org-zettel-ref-db-save db)))

(defun org-zettel-ref--detect-highlight-changes (current-highlights db source-ref-id)
  "Detect highlight changes and return the required operations.
  
  Parameters:
  - CURRENT-HIGHLIGHTS: Current highlight list in the source file
  - DB: Database instance
  - SOURCE-REF-ID: Source file reference ID
  
  Input: Current highlight list, database instance, and source file reference ID
  Output: plist containing operation types (:added :removed :updated :unchanged)
  
  This function compares the current highlights with the stored state in the database,
  identifying added, deleted, modified, and unchanged highlights."
  (let* ((state (org-zettel-ref-get-highlight-state db source-ref-id))
         (stored-highlights (if state (org-zettel-ref-highlight-state-highlights state) 
                              (make-hash-table :test 'equal)))
         (current-checksum (org-zettel-ref-calculate-highlights-checksum current-highlights))
         (stored-checksum (if state (org-zettel-ref-highlight-state-checksum state) nil))
         (added '())
         (removed '())
         (updated '())
         (unchanged '()))
    
    ;; If the checksum is the same, there is no change
    (if (and stored-checksum (string= current-checksum stored-checksum))
        (list :added '() :removed '() :updated '() :unchanged current-highlights)
      
      ;; If the checksum is different, need to compare in detail
      (progn
        ;; Check current highlights
        (dolist (current-hl current-highlights)
          (let* ((hl-id (nth 0 current-hl))
                 (stored-hl (gethash hl-id stored-highlights)))
            (if stored-hl
                ;; Exists in the database, check if there is a change
                (if (equal current-hl stored-hl)
                    (push current-hl unchanged)
                  (push current-hl updated))
              ;; Not in the database, it is new
              (push current-hl added))))
        
        ;; Check deleted highlights
        (maphash 
         (lambda (hl-id stored-hl)
           (unless (cl-find hl-id current-highlights :key #'car :test #'string=)
             (push stored-hl removed)))
         stored-highlights)
        
        (list :added added :removed removed :updated updated :unchanged unchanged)))))

(defun org-zettel-ref--update-highlight-state (db source-ref-id current-highlights)
  "Update the highlight state in the database.
  
  Parameters:
  - DB: Database instance
  - SOURCE-REF-ID: Source file reference ID
  - CURRENT-HIGHLIGHTS: Current highlight list
  
  Input: Database instance, source file reference ID, and current highlight list
  Output: No return value, but updates the database state
  
  This function saves the current highlight state to the database for next comparison."
  (let* ((state (make-org-zettel-ref-highlight-state))
         (highlights-hash (make-hash-table :test 'equal)))
    
    ;; Build highlight mapping
    (dolist (hl current-highlights)
      (puthash (nth 0 hl) hl highlights-hash))
    
    (setf (org-zettel-ref-highlight-state-highlights state) highlights-hash
          (org-zettel-ref-highlight-state-last-sync-time state) (current-time)
          (org-zettel-ref-highlight-state-checksum state) 
          (org-zettel-ref-calculate-highlights-checksum current-highlights))
    
    (org-zettel-ref-set-highlight-state db source-ref-id state)))

(defun org-zettel-ref--sync-highlights-multi-file (highlights overview-file-path overview-buffer-hint)
  "Sync highlight content to a dedicated overview file.
  
  Parameters:
  - HIGHLIGHTS: Highlight content list, each element contains (ref, type-sym, text, name, prefix, img-path, img-desc)
  - OVERVIEW-FILE-PATH: Overview file path
  - OVERVIEW-BUFFER-HINT: Overview file buffer hint
  
  Input: Highlight content list and target file information
  Output: No return value, but updates the overview file content
  
  This function uses multi-file mode, creating separate org-mode headings for each highlight."
  (when (and overview-file-path
             (file-exists-p overview-file-path))
    (org-zettel-ref-debug-message-category 'core
                                           "Multi-File Sync: Processing %d highlights into %s"
                                           (length highlights) overview-file-path)
    (let ((notes '()) ; Keep original structure, may be used later
          (org-element-use-cache nil)
          (overview-buffer (or (get-buffer overview-buffer-hint) 
                               (org-zettel-ref--safe-find-file-noselect overview-file-path nil))))
      (message "DEBUG: Multi-File - Using overview buffer: %s for file %s" overview-buffer overview-file-path)
      (with-current-buffer overview-buffer
        ;; Use basic error prevention measures, rather than complex condition-case
        (setq-local org-element-use-cache nil)
        (when (fboundp 'org-element-cache-reset) (org-element-cache-reset))
        (org-with-wide-buffer
         (dolist (highlight (sort highlights (lambda (a b) (< (string-to-number (car a)) (string-to-number (car b))))))
           (let* ((ref (nth 0 highlight)) ; Original
                  (type-sym (nth 1 highlight))
                  (text (nth 2 highlight))
                  (name (nth 3 highlight))
                  (prefix (nth 4 highlight))
                  (img-path (nth 5 highlight))
                  (img-desc (nth 6 highlight))
                  (heading-regexp (format "^\\* .* \\[\\[hl:%s\\]" ref))
                  (property-regexp (format ":HI_ID: \\[\\[hl:%s\\]" ref)))
             (message "DEBUG: Multi-File - Processing entry - ref: %s, type: %s" ref type-sym)
             (goto-char (point-min))
             (if (or (re-search-forward heading-regexp nil t)
                     (re-search-forward property-regexp nil t))
                 (progn
                   (org-back-to-heading t)
                   (let* ((heading-start (point))
                          (heading-end (line-end-position))
                          (current-heading (buffer-substring-no-properties heading-start heading-end))
                          (display-text (if (eq type-sym 'image) (or img-desc name) text))
                          (expected-heading (format "* %s %s" prefix display-text)))
                     (unless (string-match-p (regexp-quote expected-heading) current-heading)
                       (delete-region heading-start heading-end)
                       (insert expected-heading))
                     (when (and (eq type-sym 'image) img-path)
                       (org-end-of-meta-data t)
                       (let ((has-image (looking-at "\\(#\\+ATTR_ORG:.*\n\\)?\\[\\[file:")))
                         (unless has-image
                           (insert "\n#+ATTR_ORG: :width 300\n")
                           (insert (format "[[file:%s]]\n" img-path)))))))
               (progn
                 (goto-char (point-max))
                 (insert (format "\n* %s %s\n:PROPERTIES:\n:HI_ID: [[hl:%s][hl-%s]]\n:END:\n"
                               prefix
                               (if (eq type-sym 'image) (or img-desc name) text)
                               ref ref))
                 (when (and (eq type-sym 'image) img-path)
                   (insert "\n#+ATTR_ORG: :width 300\n")
                   (insert (format "[[file:%s]]\n" img-path))))))))
        (save-buffer))))) 

(defun org-zettel-ref-reset-highlight-tracking (ref-id)
  "Reset the highlight tracking state for the specified source file.
  
  Parameters:
  - REF-ID: Source file reference ID to reset
  
  This function clears the highlight state record for the specified source file in the database,
  so that all highlights will be treated as new during the next sync."
  (interactive 
   (list 
    (if (and (boundp 'org-zettel-ref-current-ref-entry)
             org-zettel-ref-current-ref-entry)
        (org-zettel-ref-ref-entry-id org-zettel-ref-current-ref-entry)
      (read-string "Enter REF_ID to reset: "))))
  
  (let ((db (org-zettel-ref-ensure-db)))
    (when-let* ((ref-entry (org-zettel-ref-db-get-ref-entry db ref-id)))
      ;; Clear highlight state
      (org-zettel-ref-ref-entry-set-user-data ref-entry 'highlight-state nil)
      (org-zettel-ref-db-save db)
      (message "Reset highlight tracking state for REF_ID %s" ref-id))))

(defun org-zettel-ref-reset-all-highlight-tracking ()
  "Reset the highlight tracking state for all source files.
  
  This function clears the highlight state record for all source files in the database,
  used to migrate from the old system to the new incremental sync system."
  (interactive)
  (when (y-or-n-p "Are you sure you want to reset the highlight tracking state for all files? This will cause all highlights to be treated as new during the next sync.")
    (let ((db (org-zettel-ref-ensure-db))
          (reset-count 0))
      (maphash 
       (lambda (ref-id ref-entry)
         (when (org-zettel-ref-ref-entry-get-user-data ref-entry 'highlight-state)
           (org-zettel-ref-ref-entry-set-user-data ref-entry 'highlight-state nil)
           (cl-incf reset-count)))
       (org-zettel-ref-db-refs db))
      (org-zettel-ref-db-save db)
      (message "Reset %d files' highlight tracking state" reset-count))))

(provide 'org-zettel-ref-core)

;;; org-zettel-ref-core.el ends here
