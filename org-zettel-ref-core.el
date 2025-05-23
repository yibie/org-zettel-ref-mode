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
  "清理重复的标题，只保留第一个匹配的REF_ID标题。"
  (let ((found-positions '())
        (kept-heading nil))
    (message "DEBUG: 开始清理重复标题，REF_ID: %s" source-ref-id)
    
    ;; 收集所有匹配的标题位置
    (condition-case err
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
                   (message "DEBUG: 获取属性时出错，位置: %s, 错误: %s" heading-start (error-message-string prop-err))))))))
      (error
       (message "DEBUG: 收集标题位置时出错: %s" (error-message-string err))))
    
    (when (> (length found-positions) 1)
      (message "DEBUG: 发现 %d 个重复标题，REF_ID: %s" (length found-positions) source-ref-id)
      (setq found-positions (nreverse found-positions)) ; 按位置排序
      (setq kept-heading (car found-positions)) ; 保留第一个
      
      ;; 删除后续的重复标题
      (dolist (pos (cdr found-positions))
        (condition-case del-err
            (save-excursion
              (goto-char pos)
              (condition-case heading-err
                  (org-back-to-heading t)
                (error
                 (message "DEBUG: org-back-to-heading 失败，位置: %s, 错误: %s" pos (error-message-string heading-err))
                 ;; 如果 org-back-to-heading 失败，尝试手动定位
                 (goto-char pos)
                 (beginning-of-line)))
              (let ((subtree-end (condition-case end-err
                                     (save-excursion (org-end-of-subtree t t))
                                   (error
                                    (message "DEBUG: org-end-of-subtree 失败，使用简单方法: %s" (error-message-string end-err))
                                    ;; 如果 org-end-of-subtree 失败，寻找下一个同级或更高级标题
                                    (save-excursion
                                      (forward-line 1)
                                      (if (re-search-forward "^\\* " nil t)
                                          (line-beginning-position)
                                        (point-max)))))))
                (message "DEBUG: 删除重复标题，位置: %s" pos)
                (delete-region (point) subtree-end)))
          (error
           (message "DEBUG: 删除标题时出错，位置: %s, 错误: %s" pos (error-message-string del-err)))))
      
      (message "DEBUG: 清理完成，保留标题位置: %s" kept-heading)
      kept-heading)
    
    ;; 如果只有一个或没有，返回第一个找到的位置
    (car found-positions)))

(defun org-zettel-ref--sync-highlights-single-file (highlights overview-file-path source-ref-id source-file-path source-title overview-buffer-hint)
  "Synchronize HIGHLIGHTS to the single OVERVIEW-FILE-PATH for a given source."
  (when (and overview-file-path
             (file-exists-p overview-file-path)
             source-ref-id source-file-path source-title) ; Ensure critical params are present
    (org-zettel-ref-debug-message-category 'core
                                         "Single-File Sync: Processing %d highlights for source %s ('%s') into %s"
                                         (length highlights) source-ref-id source-title overview-file-path)
          (let ((overview-buffer (or (get-buffer overview-buffer-hint) ; Use hint if available
                             (org-zettel-ref--safe-find-file-noselect overview-file-path nil))))
      (message "DEBUG: Single-File - Using overview buffer: %s for file %s" overview-buffer overview-file-path)
      (with-current-buffer overview-buffer
        (condition-case sync-err
            (progn
              (setq-local org-element-use-cache nil)
              (when (fboundp 'org-element-cache-reset) (org-element-cache-reset))
              (org-with-wide-buffer
               ;; 首先清理重复标题
               (let ((cleaned-heading-pos (org-zettel-ref--cleanup-duplicate-headings source-ref-id)))
                 
                 ;; 增量更新策略：检查清理后是否存在标题
                 (let ((source-heading-point cleaned-heading-pos)
                       (heading-exists (not (null cleaned-heading-pos))))
                   (message "DEBUG: 清理后检查，REF_ID: %s，存在标题: %s" source-ref-id heading-exists)

                   ;; 如果清理后没有标题，创建新的源文件标题
                   (unless heading-exists
                     (message "DEBUG: 创建新的源文件标题，REF_ID: %s" source-ref-id)
                     (goto-char (point-max))
                     (unless (bolp) (insert "\n"))
                     (insert (format "* [[ref:%s][%s]]\n" source-ref-id source-title))
                     (org-entry-put nil "REF_ID" source-ref-id)
                     (org-entry-put nil "SOURCE_FILE_PATH" (format "file:%s" source-file-path))
                     (setq source-heading-point (copy-marker (line-beginning-position)))
                     (message "DEBUG: 已创建新的源文件标题，REF_ID: %s" source-ref-id))

                   (unless source-heading-point
                     (error "无法找到或创建源文件标题，REF_ID: %s" source-ref-id))

                   ;; 增量更新高亮内容
                   (message "DEBUG: 开始增量更新 %d 个高亮，REF_ID: %s" (length highlights) source-ref-id)
                   (dolist (highlight (sort highlights (lambda (a b) (< (string-to-number (car a)) (string-to-number (car b))))))
                     (let* ((original-hl-id (nth 0 highlight))
                           (hl-type-sym (nth 1 highlight))
                           (hl-text-content (nth 2 highlight))
                           (hl-type-name (nth 3 highlight))
                           (hl-prefix (nth 4 highlight))
                           (hl-img-path (nth 5 highlight))
                           (hl-img-desc (nth 6 highlight))
                           (display-text (if (eq hl-type-sym 'image) (or hl-img-desc hl-type-name) hl-text-content))
                           found-highlight)
                       
                         ;; 在源文件标题下搜索现有高亮
                         (save-excursion
                           (goto-char source-heading-point)
                           (org-back-to-heading t)
                           (org-narrow-to-subtree)
                           (goto-char (point-min))
                           
                           ;; 搜索现有高亮条目
                           (while (and (not found-highlight)
                                     (re-search-forward "^\\** .*$" nil t))
                             (let ((props (org-entry-properties)))
                               (when (and (string= (cdr (assoc "SOURCE_REF_ID" props)) source-ref-id)
                                        (string= (cdr (assoc "ORIGINAL_HL_ID" props)) original-hl-id))
                                 (setq found-highlight t)
                                 (message "DEBUG: 找到现有高亮 %s-%s，进行更新" source-ref-id original-hl-id)
                                 ;; 更新现有高亮标题
                                 (let* ((entry-heading-start (match-beginning 0))
                                       (entry-heading-end (line-end-position))
                                       (current-heading-line (buffer-substring-no-properties entry-heading-start entry-heading-end))
                                       (expected-heading-line (format "** %s %s" hl-prefix display-text)))
                                   (unless (string= current-heading-line expected-heading-line)
                                     (delete-region entry-heading-start entry-heading-end)
                                     (insert expected-heading-line)
                                     (message "DEBUG: 已更新高亮标题: %s" expected-heading-line))
                                   ;; 清理多余的 ID 属性
                                   (when (org-entry-get nil "ID")
                                     (org-delete-property "ID"))
                                   ;; 更新图片内容
                                   (when (and (eq hl-type-sym 'image) hl-img-path)
                                     (org-end-of-meta-data t)
                                     (unless (looking-at "\\(#\\+ATTR_ORG:.*\n\\)?\\[\\[file:")
                                       (insert (format "\n#+ATTR_ORG: :width 300\n[[file:%s]]\n" hl-img-path))))))))
                         (widen))
                       
                         ;; 如果没有找到现有高亮，创建新的高亮条目
                         (unless found-highlight
                           (message "DEBUG: 创建新高亮条目 %s-%s" source-ref-id original-hl-id)
                           (save-excursion
                             (goto-char source-heading-point)
                             (org-back-to-heading t)
                             (org-narrow-to-subtree)
                             (goto-char (point-max))
                             (unless (bolp) (insert "\n"))
                             (insert (format "** %s %s\n" hl-prefix display-text))
                             (org-entry-put nil "SOURCE_REF_ID" source-ref-id)
                             (org-entry-put nil "ORIGINAL_HL_ID" original-hl-id)
                             (when (and (eq hl-type-sym 'image) hl-img-path)
                               (insert (format "\n#+ATTR_ORG: :width 300\n[[file:%s]]\n" hl-img-path)))
                             (widen)
                             (message "DEBUG: 已创建新高亮条目 %s-%s" source-ref-id original-hl-id)))))))
              
              (save-buffer)
              (message "DEBUG: 增量同步完成，REF_ID: %s" source-ref-id))
          (error
           (message "同步过程中出错: %s" (error-message-string sync-err))
           (when (fboundp 'org-element-cache-reset) (org-element-cache-reset)))))))))

(defun org-zettel-ref--sync-highlights-multi-file (highlights overview-file-path overview-buffer-hint)
  "Synchronize HIGHLIGHTS to the dedicated OVERVIEW-FILE-PATH."
  (when (and overview-file-path
             (file-exists-p overview-file-path))
    (org-zettel-ref-debug-message-category 'core
                                           "Multi-File Sync: Processing %d highlights into %s"
                                           (length highlights) overview-file-path)
    (let ((notes '()) ; Not used here but part of original structure, kept for consistency if needed later
          (org-element-use-cache nil)
          (overview-buffer (or (get-buffer overview-buffer-hint) ; Use hint if available
                               (org-zettel-ref--safe-find-file-noselect overview-file-path nil))))
      (message "DEBUG: Multi-File - Using overview buffer: %s for file %s" overview-buffer overview-file-path)
      (with-current-buffer overview-buffer
        (condition-case err ; Keep original `err` for multi-file as it was working
            (progn
              (setq-local org-element-use-cache nil)
              (when (fboundp 'org-element-cache-reset) (org-element-cache-reset))
              (org-with-wide-buffer
               (dolist (highlight (sort highlights (lambda (a b) (< (string-to-number (car a)) (string-to-number (car b))))))
                 (let* ((ref (nth 0 highlight)) ; This is original_hl_id in this context
                        (type-sym (nth 1 highlight))
                        (text (nth 2 highlight))
                        (name (nth 3 highlight))
                        (prefix (nth 4 highlight))
                        (img-path (nth 5 highlight))
                        (img-desc (nth 6 highlight))
                        (heading-regexp (format "^\\\\* .* \\\\\\[\\\\\\[hl:%s\\\\\\]" ref))
                        (property-regexp (format ":HI_ID: \\\\\\[\\\\\\[hl:%s\\\\\\]" ref)))
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
                             (let ((has-image (looking-at "\\\\(#\\\\\\+ATTR_ORG:.*\\\\n\\\\)?\\\\\\[\\\\\\[file:")))
                               (unless has-image
                                 (insert "\\\\n#+ATTR_ORG: :width 300\\\\n")
                                 (insert (format "[[file:%s]]\\\\n" img-path)))))))
                     (progn
                       (goto-char (point-max))
                       (insert (format "\\\\n* %s %s\\\\n:PROPERTIES:\\\\n:HI_ID: [[hl:%s][hl-%s]]\\\\n:END:\\\\n"
                                     prefix
                                     (if (eq type-sym 'image) (or img-desc name) text)
                                     ref ref))
                       (when (and (eq type-sym 'image) img-path)
                         (insert "\\\\n#+ATTR_ORG: :width 300\\\\n")
                         (insert (format "[[file:%s]]\\\\n" img-path)))))))))
              (save-buffer))
            (error ; Original error handler for multi-file
             (message "Error during multi-file sync: %s" (error-message-string err))
             (when (fboundp 'org-element-cache-reset) (org-element-cache-reset)))))))

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
          
          ;; 设置高亮
          (with-current-buffer source-buffer
            (org-zettel-ref-highlight-setup)
            (add-hook 'after-save-hook #'org-zettel-ref-sync-highlights nil t)
            (add-hook 'after-change-functions #'org-zettel-ref-highlight-after-change nil t))
          
          (with-current-buffer overview-buffer
            (org-zettel-ref-highlight-setup))
          
          ;; 运行初始化 hook
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
              (condition-case err
                  (org-with-wide-buffer
                   ;; First ensure we're in org-mode
                   (unless (eq major-mode 'org-mode)
                     (org-mode))
                   
                   ;; Reset org-element cache to ensure fresh parsing
                   (when (boundp 'org-element-use-cache)
                     (setq-local org-element-use-cache nil))
                   (when (fboundp 'org-element-cache-reset)
                     (org-element-cache-reset))

                   ;; 清理重复标题并检查是否存在
                   (let ((cleaned-heading-pos (org-zettel-ref--cleanup-duplicate-headings ref-id)))
                     (unless cleaned-heading-pos
                       ;; 如果清理后没有标题，创建新标题
                       (message "DEBUG: 初始化时创建新的源文件标题，REF_ID: %s" ref-id)
                       (goto-char (point-max))
                       (unless (bolp) (insert "\n"))
                       (insert (format "* [[ref:%s][%s]]\n" 
                                     ref-id
                                     (or (org-zettel-ref-ref-entry-title ref-entry)
                                         (file-name-base abs-source-file))))
                       (org-entry-put nil "REF_ID" ref-id)
                       (org-entry-put nil "SOURCE_FILE_PATH" (format "file:%s" abs-source-file))
                       (save-buffer))
                     (message "DEBUG: 初始化完成，标题状态已确认，REF_ID: %s" ref-id)))
                (error
                 (message "Error processing overview file during init: %s" (error-message-string err))))
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
                (let* ((title (org-zettel-ref-ref-entry-title ref-entry))
                       (overview-filename (org-zettel-ref-generate-filename title))
                       (target-file (expand-file-name overview-filename org-zettel-ref-overview-directory)))
                  (message "DEBUG: Creating new overview file (multi-mode): %s" target-file)
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
                  target-file)))
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
  "手动清理overview文件中的所有重复标题。
这个函数会扫描整个overview文件，清理所有REF_ID重复的标题。"
  (interactive)
  (if (not org-zettel-ref-use-single-overview-file)
      (message "此功能仅在单一文件模式下可用")
    (let* ((overview-file (expand-file-name org-zettel-ref-single-overview-file-path))
           (overview-buffer (when (file-exists-p overview-file)
                             (org-zettel-ref--safe-find-file-noselect overview-file nil))))
      (if (not overview-buffer)
          (message "未找到overview文件: %s" overview-file)
        (with-current-buffer overview-buffer
          (org-with-wide-buffer
           (let ((all-ref-ids '())
                 (cleaned-count 0))
             ;; 收集所有REF_ID
             (save-excursion
               (goto-char (point-min))
               (while (re-search-forward "^\\* .*$" nil t)
                 (let ((props (org-entry-properties)))
                   (when-let ((ref-id (cdr (assoc "REF_ID" props))))
                     (push ref-id all-ref-ids)))))
             
             ;; 去重
             (setq all-ref-ids (delete-dups all-ref-ids))
             (message "发现 %d 个不同的REF_ID" (length all-ref-ids))
             
             ;; 逐个清理
             (dolist (ref-id all-ref-ids)
               (let ((cleaned-pos (org-zettel-ref--cleanup-duplicate-headings ref-id)))
                 (when cleaned-pos
                   (cl-incf cleaned-count))))
             
             (when (> cleaned-count 0)
               (save-buffer)
               (message "清理完成！清理了 %d 个REF_ID的重复标题" cleaned-count))
             (when (= cleaned-count 0)
               (message "未发现重复标题")))))))))

;;------------------------------------------------------------------
;; REF_ID 链接支持
;;------------------------------------------------------------------

(defun org-zettel-ref-open-ref-link (ref-id)
  "通过 REF_ID 打开对应的源文件。
REF_ID 是引用条目的唯一标识符。"
  (let* ((db (org-zettel-ref-ensure-db))
         (ref-entry (org-zettel-ref-db-get-ref-entry db ref-id)))
    (if (not ref-entry)
        (message "找不到 REF_ID: %s 对应的条目" ref-id)
      (let ((file-path (org-zettel-ref-ref-entry-file-path ref-entry)))
        (if (file-exists-p file-path)
            (find-file file-path)
          (message "文件不存在: %s" file-path))))))

(defun org-zettel-ref-export-ref-link (ref-id desc backend)
  "导出 ref: 链接的函数。
REF_ID 是引用 ID，DESC 是描述，BACKEND 是导出后端。"
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
  "为 ref: 链接提供补全功能。
ARG 是可选参数。"
  (let* ((db (org-zettel-ref-ensure-db))
         (candidates '()))
    ;; 收集所有 REF_ID 和标题
    (maphash
     (lambda (ref-id ref-entry)
       (let ((title (org-zettel-ref-ref-entry-title ref-entry)))
         (push (format "%s %s" ref-id (or title "无标题")) candidates)))
     (org-zettel-ref-db-refs db))
    ;; 提供补全
    (let ((choice (completing-read "选择引用: " candidates nil t)))
      (when choice
        (car (split-string choice " " t))))))

;; 注册自定义链接类型
(with-eval-after-load 'org
  (org-link-set-parameters
   "ref"
   :follow #'org-zettel-ref-open-ref-link
   :export #'org-zettel-ref-export-ref-link
   :complete #'org-zettel-ref-complete-ref-link))

;;------------------------------------------------------------------
;; 链接迁移工具
;;------------------------------------------------------------------

(defun org-zettel-ref-migrate-file-links-to-ref-links ()
  "将单一概览文件中的 file: 链接迁移为 ref: 链接。
这个函数用于解决文件名变化导致链接失效的问题。"
  (interactive)
  (when (not org-zettel-ref-use-single-overview-file)
    (user-error "此功能仅适用于单一文件模式"))
  
  (let* ((overview-file (expand-file-name org-zettel-ref-single-overview-file-path))
         (db (org-zettel-ref-ensure-db))
         (changed-count 0))
    
    (unless (file-exists-p overview-file)
      (user-error "概览文件不存在: %s" overview-file))
    
    (with-current-buffer (find-file-noselect overview-file)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\* \\[\\[file:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" nil t)
          (let* ((file-path (match-string 1))
                 (title (match-string 2))
                 (ref-id (org-entry-get nil "REF_ID")))
            
            (when ref-id
              ;; 验证 REF_ID 在数据库中存在
              (let ((ref-entry (org-zettel-ref-db-get-ref-entry db ref-id)))
                (when ref-entry
                  ;; 替换链接
                  (replace-match (format "* [[ref:%s][%s]]" ref-id title))
                  (setq changed-count (1+ changed-count))
                  (message "迁移链接: %s -> ref:%s" (file-name-nondirectory file-path) ref-id))))))
        
        (when (> changed-count 0)
          (save-buffer)
          (message "成功迁移 %d 个文件链接为 REF_ID 链接" changed-count))
        (when (= changed-count 0)
          (message "没有找到需要迁移的链接"))))))

(provide 'org-zettel-ref-core)

;;; org-zettel-ref-core.el ends here
