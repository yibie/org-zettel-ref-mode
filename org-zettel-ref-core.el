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
;; Minor Modey
;;-----------------------

(defvar org-zettel-ref-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Add shortcuts here
    map)
  "Keymap for `org-zettel-ref-minor-mode'.")

(define-minor-mode org-zettel-ref-minor-mode
  "Minor mode for org-zettel-ref buffers.
This mode indicates that the buffer is part of an org-zettel-ref pair."
  :init-value nil
  :lighter " Zettel"
  :keymap (let ((map (make-sparse-keymap)))
            ;; 整合高亮功能的快捷键
            (define-key map (kbd "C-c h h") #'org-zettel-ref-highlight-region)
            (define-key map (kbd "C-c h r") #'org-zettel-ref-highlight-refresh)
            (define-key map (kbd "C-c h e") #'org-zettel-ref-highlight-edit)
            (define-key map (kbd "C-c h n") #'org-zettel-ref-highlight-add-note)
            (define-key map (kbd "C-c h N") #'org-zettel-ref-highlight-edit-note)
            map)
  :group 'org-zettel-ref
  (if org-zettel-ref-minor-mode
      (progn
        ;; 启用时的操作
        (org-zettel-ref-highlight-refresh)
        (add-hook 'after-save-hook #'org-zettel-ref-sync-highlights nil t)
        (add-hook 'after-change-functions #'org-zettel-ref-highlight-after-change nil t))
    ;; 禁用时的操作
    (progn
      (remove-overlays nil nil 'org-zettel-ref-highlight t)
      (remove-hook 'after-save-hook #'org-zettel-ref-sync-highlights t)
      (remove-hook 'after-change-functions #'org-zettel-ref-highlight-after-change t))))


;;-------------------------
;; Customization
;;-------------------------

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
  "存储当前高亮源文件的buffer.")

;;------------------------------------------------------------------
;; Overview File Management
;;------------------------------------------------------------------

(defun org-zettel-ref-create-overview-file (source-buffer target-file)
  "Create overview file for SOURCE-BUFFER at TARGET-FILE."
  (unless (file-exists-p target-file)
    (pcase org-zettel-ref-mode-type
      ('normal (org-zettel-ref-get-normal-overview source-buffer target-file))
      ('denote (org-zettel-ref-get-overview-file-denote source-buffer target-file))
      ('org-roam (org-zettel-ref-get-overview-file-org-roam source-buffer target-file))
      (_ (error "Unsupported org-zettel-ref-mode-type: %s" org-zettel-ref-mode-type))))
  target-file)

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
  (with-current-buffer overview-buffer))


;;------------------------------------------------------------------
;; Synchronization
;;------------------------------------------------------------------


(defun org-zettel-ref-sync-overview ()
  "使用高亮系统同步当前 buffer 到 overview 文件."
  (interactive)
  (when (and org-zettel-ref-overview-file
             (file-exists-p org-zettel-ref-overview-file))
    (org-zettel-ref-sync-highlights)))

(defun org-zettel-ref-sync-highlights ()
  "Synchronize all highlights to the overview file, using incremental update strategy."
  (interactive)
  (when (and org-zettel-ref-overview-file
             (file-exists-p org-zettel-ref-overview-file))
    (let ((highlights '())
          (notes '()))
      
      ;; 收集所有高亮和笔记
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
      
      ;; Update the overview file
      (with-current-buffer (find-file-noselect org-zettel-ref-overview-file)
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
                  (heading-regexp (format "^\\* .* \\[\\[hl:%s\\]" ref)))
             
             (message "DEBUG: Processing entry - ref: %s, type: %s" ref type)
             
             ;; Check if the corresponding entry exists
             (goto-char (point-min))
             (if (re-search-forward heading-regexp nil t)
                 ;; Update existing entry
                 (progn
                   (beginning-of-line)
                   (delete-region (point) (line-end-position))
                   (insert (format "* %s [[hl:%s][hl-%s]] %s"
                                 prefix
                                 ref
                                 ref
                                 (if (string= type "image") 
                                     (or img-desc "")
                                     text))))
               ;; Add new entry
               (goto-char (point-max))
               (insert (format "\n* %s [[hl:%s][hl-%s]] %s"
                             prefix
                             ref
                             ref
                             (if (string= type "image")
                                 (or img-desc "")
                                 text))))
             
             ;; Handle image specific content
             (when (and (string= type "image") img-path)
               (forward-line)
               (unless (looking-at "\\(#\\+ATTR_ORG:.*\n\\)?\\[\\[file:")
                 (insert "\n#+ATTR_ORG: :width 300\n")
                 (insert (format "[[file:%s]]\n" img-path))))))
         
         ;; Save the updated file
         (save-buffer))))))

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


;;------------------------------------------------------------------
;; Initialization
;;------------------------------------------------------------------

(defun org-zettel-ref-init ()
  "Initialize org-zettel-ref-mode."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (source-file (buffer-file-name source-buffer)))
    (unless source-file
      (user-error "Current buffer is not associated with a file"))
    
    ;; 启用 minor-mode
    (org-zettel-ref-minor-mode 1)
    
    ;; 设置 source-buffer
    (setq-local org-zettel-ref-source-buffer source-buffer)
    
    ;; 其他初始化
    (unless save-place-mode
      (save-place-mode 1))
    (org-zettel-ref-debug-message-category 'core "Starting initialization: %s" source-file)

    ;; 初始化高亮功能
    (org-zettel-ref-highlight-initialize-counter)
    (org-zettel-ref-highlight-refresh)
  
    (let* ((entry-pair (org-zettel-ref-ensure-entry source-buffer))
           (ref-entry (car entry-pair))
           (overview-file (cdr entry-pair)))
      
      (when overview-file
        (let* ((overview-buffer-name (format "*Overview: %s*" 
                                           (file-name-base source-file)))
               (overview-buffer (org-zettel-ref-setup-overview-window 
                               overview-file 
                               overview-buffer-name)))
          (org-zettel-ref-setup-buffers source-buffer overview-buffer)
          (setq org-zettel-ref-current-overview-buffer overview-buffer)
          (setq org-zettel-ref-overview-file overview-file))))))

(defun org-zettel-ref-ensure-entry (source-buffer)
  "Ensure source-buffer has corresponding reference and overview entries.
Return (ref-entry . overview-file) pair."
  (let* ((source-file (buffer-file-name source-buffer))
         (db (org-zettel-ref-ensure-db))
         (ref-entry (org-zettel-ref-db-ensure-ref-entry db source-file))
         (ref-id (org-zettel-ref-ref-entry-id ref-entry))
         (overview-id (org-zettel-ref-db-get-maps db ref-id))
         overview-file)
    (setq overview-file
          (if overview-id
              (when-let* ((existing-overview (org-zettel-ref-db-get-overview-by-ref-id db ref-id))  
                         (file-path (org-zettel-ref-overview-entry-file-path existing-overview)))
                (when (file-exists-p file-path)
                  file-path))
            (let* ((title (org-zettel-ref-ref-entry-title ref-entry))
                   (overview-filename (org-zettel-ref-generate-filename title))
                   (target-file (expand-file-name overview-filename org-zettel-ref-overview-directory))
                   (new-file (org-zettel-ref-create-overview-file source-buffer target-file))
                   (new-entry (org-zettel-ref-db-create-overview-entry 
                             db
                             ref-id 
                             new-file
                             title)))
              (org-zettel-ref-db-add-overview-entry db new-entry)
              (org-zettel-ref-db-add-map db ref-id (org-zettel-ref-overview-entry-id new-entry))
              new-file)))
    (org-zettel-ref-db-save db)
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
    overview-buffer))

;;------------------------------------------------------------------
;; Overview File Creation
;;------------------------------------------------------------------
(defun org-zettel-ref-get-normal-overview (source-buffer overview-file)
  "Create an overview file for SOURCE-BUFFER in normal mode."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (file-name-base source-file))
         (id (file-name-sans-extension 
              (file-name-nondirectory 
               (org-zettel-ref-generate-filename title)))))
    (unless (file-exists-p overview-file)
      (with-temp-file overview-file
        (insert (format "#+TITLE: Overview - %s\n#+SOURCE_FILE: %s\n#+filetags: :overview:\n#+startup: showall\n\n" 
                       title source-file))))
    overview-file))

(defun org-zettel-ref-get-overview-file-org-roam (source-buffer overview-file)
  "Use org-roam mode to get or create an overview file for SOURCE-BUFFER."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (file-name-base source-file))
         (org-id (org-id-new)))
    (unless (file-exists-p overview-file)
      (with-temp-file overview-file
        (insert (format ":PROPERTIES:\n:ID:       %s\n:END:\n#+title: Overview - %s\n#+filetags: :overview:\n#+startup: showall\n\n" org-id title))))
    (when (and (featurep 'org-roam)
               (fboundp 'org-roam-db-update-file))
      (org-roam-db-update-file overview-file))
    overview-file))

(defun org-zettel-ref-get-overview-file-denote (source-buffer overview-file)
  "Get or create an overview file for SOURCE-BUFFER using Denote mode."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (file-name-base source-file)))
    (unless (file-exists-p overview-file)
      (with-temp-file overview-file
        (insert (format "#+title: Overview - %s\n#+startup: showall\n#+filetags: :overview:\n\n" title))))
    overview-file))

;;----------------------------------------------------------------------------
;; Hooks
;;----------------------------------------------------------------------------

(defmacro org-zettel-ref-with-transaction (&rest body)
  "Execute operations within a database transaction."
  `(let ((db (org-zettel-ref-ensure-db)))
     (unwind-protect
         (progn ,@body
                (org-zettel-ref-db-save db org-zettel-ref-db-file))
       (setf (org-zettel-ref-db-dirty db) t))))

(defun org-zettel-ref-db--around-dired-do-rename (orig-fun &optional arg)
  "Handle database updates during dired rename.
ORIG-FUN is the original `dired-do-rename' function.
ARG is the prefix argument."
  (let* ((marked-files (dired-get-marked-files))
         (db (org-zettel-ref-ensure-db))
         (ref-dir (expand-file-name org-zettel-ref-directory))
         (overview-dir (expand-file-name org-zettel-ref-overview-directory)))
    
    ;; Execute the original function
    (funcall orig-fun arg)
    
    ;; Get the new list of files
    (let ((new-files (dired-get-marked-files)))
      ;; Process each renamed file
      (cl-loop for old-file in marked-files
               for new-file in new-files
               do (progn
                    ;; Process only files in the ref directory
                    (when (string-prefix-p ref-dir (expand-file-name old-file))
                      (when-let* ((ref-id (gethash old-file (org-zettel-ref-db-ref-paths db))))
                        (condition-case err
                            (org-zettel-ref-with-transaction
                             (when-let* ((ref-entry (gethash ref-id (org-zettel-ref-db-refs db))))
                               ;; Update file path
                               (setf (org-zettel-ref-ref-entry-file-path ref-entry) new-file)
                               ;; Update path mapping
                               (remhash old-file (org-zettel-ref-db-ref-paths db))
                               (puthash new-file ref-id (org-zettel-ref-db-ref-paths db))
                               ;; Update entry
                               (org-zettel-ref-db-update-ref-entry db ref-entry)
                               (message "Updated database for ref file rename: %s -> %s" 
                                      old-file new-file)))
                          (error
                           (message "Failed to update database for ref file %s -> %s: %s"
                                   old-file new-file (error-message-string err))))))
                    
                    ;; Process only files in the overview directory
                    (when (string-prefix-p overview-dir (expand-file-name old-file))
                      (when-let* ((overview-id (gethash old-file (org-zettel-ref-db-overview-paths db))))
                        (condition-case err
                            (org-zettel-ref-with-transaction
                             (when-let* ((overview-entry (gethash overview-id 
                                                               (org-zettel-ref-db-overviews db))))
                               ;; Update file path
                               (setf (org-zettel-ref-overview-entry-file-path overview-entry) new-file)
                               ;; Update path mapping
                               (remhash old-file (org-zettel-ref-db-overview-paths db))
                               (puthash new-file overview-id (org-zettel-ref-db-overview-paths db))
                               ;; Save changes
                               (org-zettel-ref-db-save db org-zettel-ref-db-file)
                               (message "Updated database for overview file rename: %s -> %s" 
                                      old-file new-file)))
                          (error
                           (message "Failed to update database for overview file %s -> %s: %s"
                                   old-file new-file (error-message-string err)))))))))))

(defun org-zettel-ref-db--around-dired-do-delete (orig-fun &optional arg)
  "Handle database updates during dired delete.
ORIG-FUN is the original `dired-do-delete' function.
ARG is the prefix argument."
  (let* ((marked-files (dired-get-marked-files))
         (db (org-zettel-ref-ensure-db))
         (ref-dir (expand-file-name org-zettel-ref-directory))
         (overview-dir (expand-file-name org-zettel-ref-overview-directory)))
    
    ;; Check and process database first
    (dolist (file marked-files)
      (let ((full-path (expand-file-name file)))
        (cond
         ;; Process reference files
         ((string-prefix-p ref-dir full-path)
          (when-let* ((ref-id (gethash full-path (org-zettel-ref-db-ref-paths db))))
            (condition-case err
                (org-zettel-ref-with-transaction
                 ;; Delete reference entry
                 (remhash ref-id (org-zettel-ref-db-refs db))
                 ;; Delete path mapping
                 (remhash full-path (org-zettel-ref-db-ref-paths db))
                 ;; Delete related overview mapping
                 (when-let* ((overview-id (gethash ref-id (org-zettel-ref-db-map db))))
                   (remhash ref-id (org-zettel-ref-db-map db))
                   (message "Removed ref entry and related mappings for: %s" file))
                 ;; Set database to modified state
                 (setf (org-zettel-ref-db-modified db) (current-time)
                       (org-zettel-ref-db-dirty db) t))
              (error
               (message "Failed to update database for deleted ref file %s: %s"
                       file (error-message-string err))))))
         
         ;; Process overview files
         ((string-prefix-p overview-dir full-path)
          (when-let* ((overview-id (gethash full-path (org-zettel-ref-db-overview-paths db))))
            (condition-case err
                (org-zettel-ref-with-transaction
                 ;; Delete overview entry
                 (remhash overview-id (org-zettel-ref-db-overviews db))
                 ;; Delete path mapping
                 (remhash full-path (org-zettel-ref-db-overview-paths db))
                 ;; Delete related reference mapping
                 (maphash (lambda (ref-id mapped-overview-id)
                           (when (equal mapped-overview-id overview-id)
                             (remhash ref-id (org-zettel-ref-db-map db))))
                         (org-zettel-ref-db-map db))
                 (message "Removed overview entry and related mappings for: %s" file)
                 ;; Set database to modified state
                 (setf (org-zettel-ref-db-modified db) (current-time)
                       (org-zettel-ref-db-dirty db) t))
              (error
               (message "Failed to update database for deleted overview file %s: %s"
                       file (error-message-string err)))))))))
    
    ;; Execute the original delete function
    (funcall orig-fun arg)
    
    ;; Save database
    (org-zettel-ref-db-save db)))

;; Rename handling
(advice-remove 'dired-do-rename #'org-zettel-ref-db--around-dired-do-rename)
(advice-add 'dired-do-rename :around #'org-zettel-ref-db--around-dired-do-rename)

;; Delete handling
(advice-remove 'dired-do-delete #'org-zettel-ref-db--around-dired-do-delete)
(advice-add 'dired-do-delete :around #'org-zettel-ref-db--around-dired-do-delete)


(provide 'org-zettel-ref-core)

;;; org-zettel-ref-core.el ends here
