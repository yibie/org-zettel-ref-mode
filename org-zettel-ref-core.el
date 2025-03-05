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
            (define-key map (kbd "C-c C-r") #'org-zettel-ref-rename-source-file)
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
            (when (and (local-variable-p 'org-zettel-ref-source-buffer)
                       org-zettel-ref-source-buffer
                       (buffer-live-p org-zettel-ref-source-buffer)
                       ;; 不要关闭 overview buffer 或者 source buffer
                       ;; 只有当当前 buffer 不是 overview buffer 或 source buffer 时才关闭
                       (not (or (eq buffer current-buffer) 
                                (eq buffer org-zettel-ref-source-buffer)
                                (eq current-buffer org-zettel-ref-source-buffer)))
                       org-zettel-ref-overview-file
                       (file-exists-p org-zettel-ref-overview-file))
              ;; Save if needed
              (when (buffer-modified-p)
                (save-buffer))
              ;; Delete window and buffer
              (let ((buf (current-buffer)))
                (delete-window window)  ;; 直接删除窗口，而不是切换buffer
                (kill-buffer buf)))))))))

(defun org-zettel-ref-maybe-cleanup-overview ()
  "Only cleanup overview when appropriate conditions are met."
  (when (and (not (minibufferp))
             (not (window-minibuffer-p))
             (not executing-kbd-macro))
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
    (setq-local org-zettel-ref-source-buffer source-buffer)
    (org-zettel-ref-minor-mode 1))
  
  (with-current-buffer source-buffer
    (setq-local org-zettel-ref-source-buffer source-buffer)
    (org-zettel-ref-minor-mode 1)))

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
                  (heading-regexp (format "^\\* .* \\[\\[hl:%s\\]" ref))
                  (property-regexp (format ":HL_ID: \\[\\[hl:%s\\]" ref))
                  (found-in-property nil)
                  (entry-found nil))
             
             (message "DEBUG: Processing entry - ref: %s, type: %s" ref type)
             
             ;; 检查是否存在对应的条目
             (goto-char (point-min))
             (if (re-search-forward heading-regexp nil t)
                 ;; 在标题中找到
                 (progn
                   (setq entry-found t)
                   (beginning-of-line))
               ;; 尝试在属性中查找
               (goto-char (point-min))
               (when (re-search-forward property-regexp nil t)
                 (setq entry-found t)
                 (setq found-in-property t)
                 ;; 我们在属性抽屉中找到了匹配项，需要回到标题
                 (org-back-to-heading t)))
             
             (if entry-found
                 ;; 更新现有条目
                 (progn
                   ;; 更新标题
                   (beginning-of-line)
                   (let ((heading-end (line-end-position)))
                     (delete-region (point) heading-end)
                     (insert (format "* %s %s"
                                    prefix
                                    (if (string= type "image") 
                                        (or img-desc "")
                                        text))))
                   
                   ;; 检查属性抽屉是否存在
                   (forward-line)
                   (if (looking-at "^[ \t]*:PROPERTIES:")
                       ;; 属性抽屉存在，更新 HL_ID 属性
                       (let ((drawer-start (point))
                             (hl-id-found nil))
                         ;; 查找 HL_ID 属性或 :END: 标记
                         (while (and (not (looking-at "^[ \t]*:END:"))
                                    (not (eobp)))
                           (when (looking-at "^[ \t]*:HL_ID:")
                             (setq hl-id-found t)
                             (beginning-of-line)
                             (delete-region (point) (line-end-position))
                             (insert (format ":HL_ID: [[hl:%s][hl-%s]]" ref ref)))
                           (forward-line))
                         
                         ;; 如果没有找到 HL_ID 属性，在 :END: 之前添加
                         (when (and (not hl-id-found) (looking-at "^[ \t]*:END:"))
                           (beginning-of-line)
                           (insert (format ":HL_ID: [[hl:%s][hl-%s]]\n" ref ref))))
                     ;; 没有属性抽屉，添加一个
                     (insert (format ":PROPERTIES:\n:HL_ID: [[hl:%s][hl-%s]]\n:END:\n" ref ref))))
               
               ;; 添加新条目
               (goto-char (point-max))
               (insert (format "\n* %s %s\n:PROPERTIES:\n:HL_ID: [[hl:%s][hl-%s]]\n:END:"
                             prefix
                             (if (string= type "image")
                                 (or img-desc "")
                                 text)
                             ref
                             ref)))
             
             ;; 处理图片特定内容
             (when (and (string= type "image") img-path)
               ;; 转到条目末尾（:END: 之后）
               (while (and (not (looking-at "^[ \t]*:END:"))
                          (not (eobp)))
                 (forward-line))
               (when (looking-at "^[ \t]*:END:")
                 (forward-line))
               
               ;; 检查图片是否已存在
               (unless (looking-at "\\(#\\+ATTR_ORG:.*\n\\)?\\[\\[file:")
                 (insert "\n#+ATTR_ORG: :width 300\n")
                 (insert (format "[[file:%s]]\n" img-path))))))
         
         ;; 保存更新后的文件
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
              (org-zettel-ref-db-save db org-zettel-ref-db-file)
              (set-visited-file-name new-filepath)
              (set-buffer-modified-p nil))
            (message "File renamed from %s to %s"
                     (file-name-nondirectory file)
                     (file-name-nondirectory new-filepath)))
        (error
         (message "Error during rename: %s" (error-message-string err))))
      (run-with-timer 0.5 nil #'org-zettel-ref-watch-directory))))

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
                    (org-zettel-ref-db-save db org-zettel-ref-db-file)
                    (set-visited-file-name new-file-path)
                    (set-buffer-modified-p nil)
                    (message "File renamed from %s to %s"
                             (file-name-nondirectory old-file)
                             (file-name-nondirectory new-file-path)))
                (error
                 (message "Error during rename: %s" (error-message-string err))))
              (run-with-timer 0.5 nil #'org-zettel-ref-watch-directory)))))))

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
    (org-zettel-ref-db-save db org-zettel-ref-db-file)
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
      (cl-loop for old-file in marked-files
               for new-file in new-files
               do (progn
                    (when (string-prefix-p ref-dir (expand-file-name old-file))
                      (when-let* ((ref-id (gethash old-file (org-zettel-ref-db-ref-paths db))))
                        (condition-case err
                            (org-zettel-ref-with-transaction
                             (when-let* ((ref-entry (gethash ref-id (org-zettel-ref-db-refs db))))
                               (setf (org-zettel-ref-ref-entry-file-path ref-entry) new-file)
                               (remhash old-file (org-zettel-ref-db-ref-paths db))
                               (puthash new-file ref-id (org-zettel-ref-db-ref-paths db))
                               (org-zettel-ref-db-update-ref-entry db ref-entry)
                               (org-zettel-ref-db-save db org-zettel-ref-db-file)
                               (message "Updated database for reference file rename: %s -> %s" 
                                      old-file new-file)))
                          (error
                           (message "Failed to update database for reference file %s -> %s: %s"
                                   old-file new-file (error-message-string err))))))
                    
                    (when (string-prefix-p overview-dir (expand-file-name old-file))
                      (when-let* ((overview-id (gethash old-file (org-zettel-ref-db-overview-paths db))))
                        (condition-case err
                            (org-zettel-ref-with-transaction
                             (when-let* ((overview-entry (gethash overview-id 
                                                               (org-zettel-ref-db-overviews db))))
                               (setf (org-zettel-ref-overview-entry-file-path overview-entry) new-file)
                               (remhash old-file (org-zettel-ref-db-overview-paths db))
                               (puthash new-file overview-id (org-zettel-ref-db-overview-paths db))
                               (org-zettel-ref-db-update-overview-entry db overview-entry)
                               (org-zettel-ref-db-save db org-zettel-ref-db-file)
                               (message "Updated database for overview file rename: %s -> %s" 
                                      old-file new-file)))
                          (error
                           (message "Failed to update database for overview file %s -> %s: %s"
                                   old-file new-file (error-message-string err))))))))))
    
    ;; Save database
    (org-zettel-ref-db-save db org-zettel-ref-db-file)))

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
                 (when-let* ((ref-entry (gethash ref-id (org-zettel-ref-db-refs db))))
                   (remhash ref-id (org-zettel-ref-db-refs db))
                   (remhash full-path (org-zettel-ref-db-ref-paths db))
                   (when-let* ((overview-id (gethash ref-id (org-zettel-ref-db-map db))))
                     (remhash ref-id (org-zettel-ref-db-map db))
                     (message "Removed ref entry and related mappings for: %s" file))
                   (setf (org-zettel-ref-db-modified db) (current-time)
                         (org-zettel-ref-db-dirty db) t)))
              (error
               (message "Failed to update database for deleted ref file %s: %s"
                       file (error-message-string err))))))
         
         ;; Process overview files
         ((string-prefix-p overview-dir full-path)
          (when-let* ((overview-id (gethash full-path (org-zettel-ref-db-overview-paths db))))
            (condition-case err
                (org-zettel-ref-with-transaction
                 (when-let* ((overview-entry (gethash overview-id 
                                                     (org-zettel-ref-db-overviews db))))
                   (remhash overview-id (org-zettel-ref-db-overviews db))
                   (remhash full-path (org-zettel-ref-db-overview-paths db))
                   (maphash (lambda (ref-id mapped-overview-id)
                             (when (equal mapped-overview-id overview-id)
                               (remhash ref-id (org-zettel-ref-db-map db))))
                           (org-zettel-ref-db-map db))
                   (message "Removed overview entry and related mappings for: %s" file)
                   (setf (org-zettel-ref-db-modified db) (current-time)
                         (org-zettel-ref-db-dirty db) t)))
              (error
               (message "Failed to update database for deleted overview file %s: %s"
                       file (error-message-string err)))))))))
    
    ;; Execute the original delete function
    (funcall orig-fun arg)
    
    ;; Save database
    (org-zettel-ref-db-save db org-zettel-ref-db-file)))

;; Rename handling
(advice-remove 'dired-do-rename #'org-zettel-ref-db--around-dired-do-rename)
(advice-add 'dired-do-rename :around #'org-zettel-ref-db--around-dired-do-rename)

;; Delete handling
(advice-remove 'dired-do-delete #'org-zettel-ref-db--around-dired-do-delete)
(advice-add 'dired-do-delete :around #'org-zettel-ref-db--around-dired-do-delete)


(provide 'org-zettel-ref-core)

;;; org-zettel-ref-core.el ends here
