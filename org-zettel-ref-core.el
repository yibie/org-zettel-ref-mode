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

(defcustom org-zettel-ref-overview-directory "~/org-zettel-ref-overviews/"
  "Directory to store overview files."
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

(defun org-zettel-ref-sync-highlights ()
  "Synchronize all highlights to the overview file, using incremental update strategy."
  (interactive)
  (message "DEBUG: Starting sync in buffer: %s" (buffer-name))
  (message "DEBUG: Current overview file: %s" org-zettel-ref-overview-file)
  (message "DEBUG: Current overview buffer: %s" org-zettel-ref-overview-buffer)
  
  (when (and org-zettel-ref-overview-file
             (file-exists-p org-zettel-ref-overview-file))
    (let ((highlights '())
          (notes '())
          (org-element-use-cache nil))  ; 全局禁用 cache
      
      ;; collect all the highlights
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
          (let* ((ref (or (match-string 1) ""))
                 (type-char (or (match-string 2) ""))
                 (text (or (match-string 3) ""))
                 (type (org-zettel-ref-highlight-char-to-type type-char))
                 (config (cdr (assoc type org-zettel-ref-highlight-types))))
            (org-zettel-ref-debug-message-category 'core 
              "Processing highlight - ref: %s, type: %s" 
              ref type)
            (when (and type-char (not (string-empty-p type-char)) config)
              (let ((name (plist-get config :name))
                    (prefix (plist-get config :prefix)))
                (if (string= type-char "i")
                    (let* ((img-parts (split-string text "|"))
                          (img-path (car img-parts))
                          (img-desc (cadr img-parts)))
                      (when (and img-path (not (string-empty-p img-path)))
                        (push (list ref type text name prefix img-path img-desc)
                              highlights)))
                  (push (list ref type text name prefix nil nil)
                        highlights)))))))
      
      (org-zettel-ref-debug-message-category 'core 
        "Sync complete - processed %d highlights" 
        (length highlights))
      
      ;; Update the overview file with cache handling
      (let ((overview-buffer (or org-zettel-ref-overview-buffer
                                (find-file-noselect org-zettel-ref-overview-file))))
        (message "DEBUG: Using overview buffer: %s" overview-buffer)
        (with-current-buffer overview-buffer
          (condition-case err
              (progn
                ;; 确保禁用 cache
                (setq-local org-element-use-cache nil)
                (when (fboundp 'org-element-cache-reset)
                  (org-element-cache-reset))
                
                (org-with-wide-buffer
                 ;; Update or add each highlight
                 (dolist (highlight (sort highlights
                                        (lambda (a b)
                                          (< (string-to-number (car a))
                                             (string-to-number (car b))))))
                   (let* ((ref (nth 0 highlight))
                          (type (nth 1 highlight))
                          (text (nth 2 highlight))
                          (name (nth 3 highlight))
                          (prefix (nth 4 highlight))
                          (img-path (nth 5 highlight))
                          (img-desc (nth 6 highlight))
                          (heading-regexp (format "^\\* .* \\[\\[hl:%s\\]" ref))
                          (property-regexp (format ":HI_ID: \\[\\[hl:%s\\]" ref)))
                     
                     (message "DEBUG: Processing entry - ref: %s, type: %s" ref type)
                     
                     ;; Check if the corresponding entry exists by searching for property
                     (goto-char (point-min))
                     (if (or (re-search-forward heading-regexp nil t)
                             (re-search-forward property-regexp nil t))
                         ;; Entry exists - update only if needed
                         (progn
                           ;; Find the heading
                           (org-back-to-heading t)
                           ;; Get current heading text
                           (let* ((heading-start (point))
                                  (heading-end (line-end-position))
                                  (current-heading (buffer-substring-no-properties heading-start heading-end))
                                  (display-text (if (string= type "image") 
                                                   (or img-desc name)
                                                 text))
                                  (expected-heading (format "* %s %s"
                                                          prefix
                                                          display-text)))
                             
                             ;; Only update heading if it's different
                             (unless (string-match-p (regexp-quote expected-heading) current-heading)
                               (delete-region heading-start heading-end)
                               (insert expected-heading))
                             
                             ;; For images, check if we need to update the image
                             (when (and (string= type "image") img-path)
                               ;; Move past properties drawer
                               (org-end-of-meta-data t)
                               ;; Check if image already exists
                               (let ((has-image (looking-at "\\(#\\+ATTR_ORG:.*\n\\)?\\[\\[file:")))
                                 (unless has-image
                                   ;; Add image if not present
                                   (insert "\n#+ATTR_ORG: :width 300\n")
                                   (insert (format "[[file:%s]]\n" img-path)))))))
                       
                       ;; Add new entry
                       (goto-char (point-max))
                       (insert (format "\n* %s %s\n:PROPERTIES:\n:HI_ID: [[hl:%s][hl-%s]]\n:END:\n"
                                     prefix
                                     (if (string= type "image")
                                         (or img-desc name)
                                       text)
                                     ref
                                     ref))
                       
                       ;; Handle image specific content for new entries
                       (when (and (string= type "image") img-path)
                         (insert "\n#+ATTR_ORG: :width 300\n")
                         (insert (format "[[file:%s]]\n" img-path)))))))
                
                ;; Save the updated file
                (save-buffer))
            (error
             (message "Error during sync: %s" (error-message-string err))
             ;; 尝试恢复 cache 状态
             (when (fboundp 'org-element-cache-reset)
               (org-element-cache-reset)))))))))



;;----------------------------------------------------------------
;; File namming
;;----------------------------------------------------------------

(defun org-zettel-ref-generate-filename (title)
  "Generate overview file name based on TITLE."
  (let ((timestamp (format-time-string "%Y%m%dT%H%M%S")))
    (format "%s--%s%s" 
            timestamp
            title  ; Use original title directly
            org-zettel-ref-overview-file-suffix)))

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
          
          ;; 激活 buffers
          (org-zettel-ref--activate-buffer source-buffer)
          (org-zettel-ref--activate-buffer overview-buffer)
          
          ;; 设置 buffer 关系
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
          
          ;; 设置高亮
          (with-current-buffer source-buffer
            (org-zettel-ref-highlight-setup)
            (add-hook 'after-save-hook #'org-zettel-ref-sync-highlights nil t)
            (add-hook 'after-change-functions #'org-zettel-ref-highlight-after-change nil t))
          
          (with-current-buffer overview-buffer
            (org-zettel-ref-highlight-setup))
          
          ;; 运行初始化 hook
          (run-hooks 'org-zettel-ref-init-hook)
          
          (message "Initialized org-zettel-ref for %s" (buffer-name)))))))

(defun org-zettel-ref-ensure-entry (source-buffer)
  "Ensure source-buffer has corresponding reference and overview entries.
Return (ref-entry . overview-file) pair."
  (message "DEBUG: Ensuring entry for buffer: %s" (buffer-name source-buffer))
  (let* ((source-file (buffer-file-name source-buffer))
         (abs-source-file (expand-file-name source-file))
         (db (org-zettel-ref-ensure-db))
         (ref-entry (org-zettel-ref-db-ensure-ref-entry db abs-source-file))
         (ref-id (org-zettel-ref-ref-entry-id ref-entry))
         (overview-id (org-zettel-ref-db-get-maps db ref-id))
         overview-file)
    
    (message "DEBUG: Source file: %s" source-file)
    (message "DEBUG: Ref ID: %s" ref-id)
    (message "DEBUG: Overview ID: %s" overview-id)
    
    ;; Ensure overview directory exists
    (unless (file-exists-p org-zettel-ref-overview-directory)
      (make-directory org-zettel-ref-overview-directory t))
    
    (setq overview-file
          (if overview-id
              ;; Try to get existing overview file
              (when-let* ((existing-overview (org-zettel-ref-db-get-overview-by-ref-id db ref-id))
                         (file-path (org-zettel-ref-overview-entry-file-path existing-overview)))
                (message "DEBUG: Found existing overview file: %s" file-path)
                (if (file-exists-p file-path)
                    file-path
                  ;; If file doesn't exist but entry does, remove the entry
                  (message "DEBUG: Overview file doesn't exist, removing entry")
                  (remhash overview-id (org-zettel-ref-db-overviews db))
                  (remhash ref-id (org-zettel-ref-db-map db))
                  nil))
            ;; Create new overview file
            (let* ((title (org-zettel-ref-ref-entry-title ref-entry))
                   (overview-filename (org-zettel-ref-generate-filename title))
                   (target-file (expand-file-name overview-filename org-zettel-ref-overview-directory)))
              (message "DEBUG: Creating new overview file: %s" target-file)
              (unless (file-exists-p target-file)
                (let ((new-file (org-zettel-ref-create-overview-file source-buffer target-file ref-entry))
                      (new-entry (org-zettel-ref-db-create-overview-entry 
                                db
                                ref-id 
                                target-file
                                title)))
                  (org-zettel-ref-db-add-overview-entry db new-entry)
                  (org-zettel-ref-db-add-map db ref-id (org-zettel-ref-overview-entry-id new-entry))
                  (org-zettel-ref-db-save db)
                  new-file))
              target-file)))
    (message "DEBUG: Final overview file: %s" overview-file)
    (cons ref-entry overview-file)))

(defun org-zettel-ref-setup-overview-window (overview-file buffer-name)
  "Setup a window for OVERVIEW-FILE with BUFFER-NAME.
Returns the overview buffer."
  (let* ((source-window (selected-window))
         (source-width (window-width source-window))
         ;; calculate target windows width
         (target-width (max org-zettel-ref-overview-min-width
                           (round (* source-width org-zettel-ref-overview-width-ratio))))
         (overview-window (split-window-right))
         ;; display overview file in new window
         (overview-buffer (find-file-noselect overview-file)))
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




(provide 'org-zettel-ref-core)

;;; org-zettel-ref-core.el ends here
