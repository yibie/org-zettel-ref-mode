;;; org-zettel-ref-reading-manager.el --- Reading management interface for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides a reading management interface for org-zettel-ref-mode.
;; It manages reading states and integrates with the database system
;; provided by org-zettel-ref-db.el.

;;; Code:

(require 'org-zettel-ref-db)
(require 'transient)

;;;----------------------------------------------------------------------------
;;; Variables
;;;----------------------------------------------------------------------------

(defcustom org-zettel-ref-directory "~/Documents/ref/"
  "org-zettel-ref 笔记库的目录路径。")

(defvar org-zettel-ref-filter-presets-file
  (expand-file-name "org-zettel-ref-filters.el" org-zettel-ref-directory)
  "文件路径，存储过滤器预设。")

(defvar org-zettel-ref-list-mode-map (make-sparse-keymap)
  "org-zettel-ref 阅读模式下的快捷键映射。")

(defgroup org-zettel-ref-reading-manager nil
  "Customization group for org-zettel-ref reading manager."
  :group 'org-zettel-ref)

;;;----------------------------------------------------------------------------
;;; File Name Parsing and Formatting
;;;----------------------------------------------------------------------------

(defconst org-zettel-ref-author-regexp "^\\([^_]*?\\)__.*$"
  "匹配文件名中的作者部分") 

(defconst org-zettel-ref-title-regexp "__\\([^=]*?\\)==.*$"
  "匹配文件名中的标题部分")

(defconst org-zettel-ref-keywords-regexp "==\\([^.]*?\\)\\..*$"
  "匹配文件名中关键词部分")

;; Parse file name
(defun org-zettel-ref-parse-filename (filename)
  "将FILENAME解析为 (author title keywords) 的列表。
如果文件名不符合标准格式，返回基本信息。"
  (message "Debug: Parsing filename: %s" filename)
  (let (author title keywords)
    ;; 尝试标准格式解析
    (cond
     ;; 标准格式：author__title==keywords.org
     ((string-match "^\\(.+?\\)__\\(.+?\\)==\\(.+?\\)\\.org$" filename)
      (setq author (match-string 1 filename)
            title (match-string 2 filename)
            keywords (split-string (match-string 3 filename) "_")))
     
     ;; 只有作者和标题：author__title.org
     ((string-match "^\\(.+?\\)__\\(.+?\\)\\.org$" filename)
      (setq author (match-string 1 filename)
            title (match-string 2 filename)))
     
     ;; 其他情况：使用文件名作为题
     (t
      (setq title (file-name-sans-extension filename))))
    
    (message "Debug: Parsed: author=%s, title=%s, keywords=%S" 
             author title keywords)
    (list author title keywords)))

;; Format file name
(defun org-zettel-ref-format-filename (author title keywords)
  "生成标准格式的文件名: AUTHOR__TITLE==KEYWORDS.org"
    (concat 
     (when author (concat author "__"))
     title
     (when keywords (concat "==" (string-join keywords "_")))
     ".org"))

;; Get file modified time
(defun org-zettel-ref-get-modified-time (file)
  "获取FILE的修改时间"
  (format-time-string "%Y-%m-%d %H:%M:%S"
                     (nth 5 (file-attributes file))))


;;;----------------------------------------------------------------------------
;;; File Scanning and Info Table Construction, and Overview File Management
;;;----------------------------------------------------------------------------


;;;----------------------------------------------------------------------------
;;; Display Interface
;;;----------------------------------------------------------------------------

;;; Column Components

(defun org-zettel-ref-column-status ()
  "Status column definition."
  (list "Status" 10 t
        (lambda (entry)
          (let ((info (org-zettel-ref-db-get-entry 
                      (org-zettel-ref-db-get-id-by-path (car entry)))))
            (org-zettel-ref-state-string info)))))

(defun org-zettel-ref-column-author ()
  "Author column definition."
  (list "Author" 20 t
        (lambda (entry)
          (let ((info (org-zettel-ref-db-get (car entry))))
            (or (org-zettel-ref-entry-author info) "")))))

(defun org-zettel-ref-column-title ()
  "Title column definition."
  (list "Title" 60 t
        (lambda (entry)
          (let ((info (org-zettel-ref-db-get (car entry))))
            (or (org-zettel-ref-entry-title info) "")))))

(defun org-zettel-ref-column-modified ()
  "Modified time column definition."
  (list "Modified" 30 t
        (lambda (entry)
          (let ((info (org-zettel-ref-db-get (car entry))))
            (or (org-zettel-ref-entry-modified-time info) "")))))

(defun org-zettel-ref-column-keywords ()
  "Keywords column definition."
  (list "Keywords" 0 t
        (lambda (entry)
          (let ((info (org-zettel-ref-db-get (car entry))))
            (if (org-zettel-ref-entry-keywords info)
                (string-join (org-zettel-ref-entry-keywords info) ", ")
              "")))))

;;;----------------------------------------------------------------------------
;;; List Configuration
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-list-columns ()
  "Get the column configurations to display."
  (list (org-zettel-ref-column-status)
        (org-zettel-ref-column-author)
        (org-zettel-ref-column-title)
        (org-zettel-ref-column-modified)
        (org-zettel-ref-column-keywords)))

;;; Format Generators

(defun org-zettel-ref-list--get-format ()
  "Generate display format from column definitions."
  (vconcat
   (mapcar (lambda (column)
             (list (nth 0 column)    ; Title
                   (nth 1 column)    ; Width
                   (nth 2 column)))  ; Sortable
           (org-zettel-ref-list-columns))))

(defun org-zettel-ref-list--format-entry (id entry)
  "Format entry for tabulated list display.
ID is the entry identifier.
ENTRY is the org-zettel-ref-entry struct."
  (if (null entry)
      (progn 
        (message "Warning: Nil entry for id %s" id)
        (make-vector (length (org-zettel-ref-list-columns)) "N/A"))
    (vconcat
     (mapcar (lambda (column)
               (let ((value (funcall (nth 3 column) (cons id entry))))
                 (if (stringp value) value (format "%s" value))))
             (org-zettel-ref-list-columns)))))

;;; List Mode Definition

(define-derived-mode org-zettel-ref-list-mode tabulated-list-mode "Org-Zettel-Ref"
  "Major mode for displaying org-zettel-ref reading list."
  (setq tabulated-list-format
        [("Title" 60 t)
         ("Author" 20 t)
         ("Keywords" 30 t)
         ("Status" 10 t)
         ("Modified" 20 t)])  ; 添加了 Modified 列
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Title" nil))
  (tabulated-list-init-header))

;;; Entry Management

(defun org-zettel-ref-list--get-entries ()
  "获取所有条目用于显示。"
  (let (entries)
    (maphash
     (lambda (id entry)
       (when (and entry (org-zettel-ref-entry-source-file entry))
         (push (list (org-zettel-ref-entry-source-file entry)
                    (org-zettel-ref-list--format-entry id entry))
               entries)))
     (org-zettel-ref-db-entries (org-zettel-ref-db-ensure)))
    (nreverse entries)))

(defun org-zettel-ref-list-refresh (&rest _args)
  "刷新显示。"
  (interactive)
  (let ((entries (org-zettel-ref-list--get-entries)))
    (setq tabulated-list-entries entries)
    (tabulated-list-print t)))



;;;---------------------------------------------------------------------------- 
;;; File Operation
;;;----------------------------------------------------------------------------

;;; 重命名相关的辅助函数

(defun org-zettel-ref-rename--prompt-author (current-author)
  "提示输入作名，CURRENT-AUTHOR为当前作者。"
  (read-string 
   (format "Author%s: "
           (if current-author
               (format " (current: %s)" current-author)
             ""))
   current-author))

(defun org-zettel-ref-rename--prompt-title (current-title)
  "提示输入标题，CURRENT-TITLE为当前标题。"
  (read-string 
   (format "Title%s: "
           (if current-title
               (format " (current: %s)" current-title)
             ""))
   current-title))

(defun org-zettel-ref-rename--prompt-keywords (current-keywords)
  "提示输入关键词，CURRENT-KEYWORDS为当前关键词列。"
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
  "重命名当前文件。"
  (interactive)
  (when-let* ((old-file (tabulated-list-get-id))
              (entry (org-zettel-ref-db-get-entry 
                      (org-zettel-ref-db-get-id-by-path old-file)))
              ;; 解析当前文件名
              (old-info (org-zettel-ref-parse-filename 
                         (file-name-nondirectory old-file)))
              ;; 分别提示修改各部分
              (new-author (org-zettel-ref-rename--prompt-author 
                           (nth 0 old-info)))
              (new-title (org-zettel-ref-rename--prompt-title
                          (nth 1 old-info)))
              (new-keywords (org-zettel-ref-rename--prompt-keywords
                             (nth 2 old-info)))
              ;; 生成新文件名
              (new-name (org-zettel-ref-format-filename 
                         new-author new-title new-keywords)))
    (condition-case err
        (let* ((new-file (expand-file-name new-name 
                                          (file-name-directory old-file)))
               (overview-file (org-zettel-ref-entry-overview-file entry))
               (new-overview (when overview-file
                             ;; 根据源文件名更新概览文件名
                              (org-zettel-ref-make-overview-name 
                               new-file
                               (org-zettel-ref-entry-id entry)))))
          
          ;; 检查目标文件是否存在
          (when (and (file-exists-p new-file)
                    (not (string= old-file new-file)))
            (user-error "目标文件已存在: %s" new-file))
          
          ;; 执行重命名
          (rename-file old-file new-file)
          
          ;; 如果有概览文件，也重命名
          (when (and overview-file new-overview)
            (rename-file overview-file new-overview))
          
          ;; 更新数据库
          (org-zettel-ref-db-handle-rename old-file new-file)
          (when overview-file
            (org-zettel-ref-db-handle-rename overview-file new-overview))
          
          ;; 更新显示
          (org-zettel-ref-db-notify-state-change entry)
          (org-zettel-ref-list-refresh))
      
      (error 
       (org-zettel-ref-handle-error 'rename old-file err)))))

(defun org-zettel-ref-list-delete-file ()
  "删除当前文件。"
  (interactive)
  (when-let* ((file (tabulated-list-get-id))
              (entry (org-zettel-ref-db-get-entry 
                      (org-zettel-ref-db-get-id-by-path file))))
    (when (yes-or-no-p (format "Delete %s? " file))
      (condition-case err
          (progn
            (delete-file file)
            (when-let ((overview (org-zettel-ref-entry-overview-file entry)))
              (when (file-exists-p overview)
                (delete-file overview)))
            (org-zettel-ref-db-remove-entry 
             (org-zettel-ref-db-get-id-by-path file))
            (org-zettel-ref-list-refresh))
        (error (org-zettel-ref-handle-error 'delete file err))))))

(defun org-zettel-ref-list-open-file ()
  "Open the file at point."
  (interactive)
  (when-let ((file (tabulated-list-get-id)))
    (if (file-exists-p file)
        (find-file file)
      (user-error "File does not exist: %s" file))))

;;;----------------------------------------------------------------------------
;;; Experiment feature: List Editing Mode: rename filename like wdired
;;;----------------------------------------------------------------------------

(defvar-local org-zettel-ref-list-edit-mode nil
  "Whether the list is in edit mode.")

;;; 切换到编辑模式
(defun org-zettel-ref-list-edit ()
  "Switch to edit mode."
  (interactive)
  (unless org-zettel-ref-list-edit-mode
    ;; 启用编辑模式
    (setq org-zettel-ref-list-edit-mode t)
    ;; 移除只属性
    (setq buffer-read-only nil)
    ;; 使列表可编辑
    (let ((inhibit-read-only t))
      ;; 添加编辑模式的视觉提示
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((entry (tabulated-list-get-entry))
                 (file (tabulated-list-get-id))
                 (info (when file (org-zettel-ref-db-get file))))
            (when info
              ;; 为每个可编辑的单元格添加属性和视觉提示
              (dolist (col '(1 2 4))  ; Author, Title, Keywords 列
                (let* ((text (aref entry col))
                       (cell-start (tabulated-list-get-entry-column col))
                       (cell-end (+ cell-start (length text))))
                  ;; 添加可编辑属性和视觉提示
                  (add-text-properties 
                   cell-start cell-end
                   `(org-zettel-ref-editable t
                     face (:background "#f0f0f0" :underline t)
                     help-echo "Click to edit this field"))
                  ;; 添加鼠标点击事件
                  (add-text-properties
                   cell-start cell-end
                   '(mouse-face highlight
                     keymap ,(let ((map (make-sparse-keymap)))
                              (define-key map [mouse-1]
                                        #'org-zettel-ref-list-edit-cell)
                              map)))))))
          (forward-line 1))))
    
    ;; 添加编辑模式的键位绑定
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c C-c") #'org-zettel-ref-list-edit-finish)
      (define-key map (kbd "C-c C-k") #'org-zettel-ref-list-edit-abort)
      (define-key map [tab] #'org-zettel-ref-list-next-field)
      (define-key map [backtab] #'org-zettel-ref-list-prev-field)
      (define-key map (kbd "RET") #'org-zettel-ref-list-edit-cell)
      (use-local-map (make-composed-keymap map (current-local-map))))
    
    ;; 更新模式显示
    (setq mode-name "Org-Zettel-Ref-Edit")
    (force-mode-line-update)
    (message "Editing mode enabled. Press RET to edit field, TAB to move between fields. C-c C-c to finish, C-c C-k to abort")))

;;; 编辑单元格
(defun org-zettel-ref-list-edit-cell ()
  "Edit the cell at point."
  (interactive)
  (when-let* ((pos (point))
              (col (get-text-property pos 'tabulated-list-column))
              (editable (get-text-property pos 'org-zettel-ref-editable))
              (file (tabulated-list-get-id))
              (entry (tabulated-list-get-entry))
              (old-value (aref entry col)))
    (let* ((prompt (format "Edit %s: "
                          (nth col '("Status" "Author" "Title" "Modified" "Keywords"))))
           (new-value (read-string prompt old-value)))
      (unless (string= old-value new-value)
        (aset entry col new-value)
        (let ((inhibit-read-only t))
          (tabulated-list-put-field col new-value))))))

;;; 字段导航
(defun org-zettel-ref-list-next-field ()
  "Move to next editable field."
  (interactive)
  (let ((pos (point))
        found)
    (save-excursion
      (while (and (not found)
                  (< pos (point-max)))
        (setq pos (next-single-property-change pos 'org-zettel-ref-editable))
        (when (and pos
                   (get-text-property pos 'org-zettel-ref-editable))
          (setq found pos))))
    (when found
      (goto-char found))))

(defun org-zettel-ref-list-prev-field ()
  "Move to previous editable field."
  (interactive)
  (let ((pos (point))
        found)
    (save-excursion
      (while (and (not found)
                  (> pos (point-min)))
        (setq pos (previous-single-property-change pos 'org-zettel-ref-editable))
        (when (and pos
                   (get-text-property pos 'org-zettel-ref-editable))
          (setq found pos))))))

;;; 完成编辑
(defun org-zettel-ref-list-edit-finish ()
  "Finish editing and apply changes."
  (interactive)
  (when org-zettel-ref-list-edit-mode
    (condition-case err
        (let ((changes nil))
          ;; 收集所有变更
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when-let* ((file (tabulated-list-get-id))
                          (entry (tabulated-list-get-entry))
                          (info (org-zettel-ref-db-get file))
                          (new-author (aref entry 1))
                          (new-title (aref entry 2))
                          (new-keywords (split-string (aref entry 4) "[,\s]+" t)))
                ;; 检查是否有变化
                (when (or (not (string= new-author (or (org-zettel-ref-entry-author info) "")))
                         (not (string= new-title (or (org-zettel-ref-entry-title info) "")))
                         (not (equal new-keywords (or (org-zettel-ref-entry-keywords info) nil))))
                  (push (list file new-author new-title new-keywords) changes)))
              (forward-line 1)))
          ;; 应用变更
          (when changes
            (if (yes-or-no-p (format "Apply %d file changes? " (length changes)))
                (dolist (change changes)
                  (pcase-let ((`(,old-path ,new-author ,new-title ,new-keywords) change))
                    (org-zettel-ref-rename-file-internal 
                     old-path
                     (expand-file-name 
                      (org-zettel-ref-format-filename new-author new-title new-keywords)
                      (file-name-directory old-path))
                     new-author new-title new-keywords)))
              (message "Changes aborted"))))
      
      (error
       (org-zettel-ref-handle-error 'edit-finish nil err)))
    
    ;; 退出编辑模式
    (setq org-zettel-ref-list-edit-mode nil)
    (setq buffer-read-only t)
    (setq mode-name "Org-Zettel-Ref")
    (force-mode-line-update)
    (org-zettel-ref-list-refresh)))

;;; 取消编辑
(defun org-zettel-ref-list-edit-abort ()
  "Abort editing without applying changes."
  (interactive)
  (when org-zettel-ref-list-edit-mode
    (setq org-zettel-ref-list-edit-mode nil)
    (setq buffer-read-only t)
    (setq mode-name "Org-Zettel-Ref")
    (force-mode-line-update)
    (org-zettel-ref-list-refresh)
    (message "Edit aborted")))

;;; 内部命名函数
(defun org-zettel-ref-rename-file-internal (old-path new-path author title keywords)
  "Internal function to rename file and update its information."
  (unless (string= old-path new-path)
    (when (and (file-exists-p new-path)
               (not (string= old-path new-path)))
      (user-error "File already exists: %s" new-path))
    
    (let ((info (org-zettel-ref-db-get old-path)))
      ;; 更新文件信息
      (setf (org-zettel-ref-entry-author info) author
            (org-zettel-ref-entry-title info) title
            (org-zettel-ref-entry-keywords info) keywords
            (org-zettel-ref-entry-modified-time info)
            (format-time-string "%Y-%m-%d %H:%M:%S"))
      
      ;; 重命名文件
      (rename-file old-path new-path t)
      
      ;; 更新哈希表
      (org-zettel-ref-db-remove old-path)
      (org-zettel-ref-db-put new-path info)
      
      ;; 更新访问的buffer
      (when-let ((buffer (get-file-buffer old-path)))
        (with-current-buffer buffer
          (set-visited-file-name new-path))))))


;;;----------------------------------------------------------------------------
;;; Search and Filtering 
;;;----------------------------------------------------------------------------

(defgroup org-zettel-ref-filter nil
  "Customization for org-zettel-ref filtering."
  :group 'org-zettel-ref)

;;; 过滤条件结构

(defvar-local org-zettel-ref-active-filters nil
  "当前激活的过滤条件列表。
每个过滤条件是一个 (column . predicate) 的 cons cell。")

;;; 过滤谓词生成函数

(defun org-zettel-ref-make-string-filter (column pattern)
  "创建基于字符串匹配的过滤条件。
COLUMN 是列序号，PATTERN 是要匹配的模式。"
  (cons column
        (lambda (entry)
          (let ((value (aref (cadr entry) column)))
            (string-match-p pattern value)))))

(defun org-zettel-ref-make-status-filter (status)
  "创建基于阅读状态的过滤条件。
STATUS 可以是 'read 或 'unread。"
  (cons 0  ; Status 列的序号
        (lambda (entry)
          (let ((info (org-zettel-ref-db-get (car entry))))
            (if (eq status 'read)
                (org-zettel-ref-state-read-p info)
              (org-zettel-ref-state-unread-p info))))))

;;; 过滤命令

(defun org-zettel-ref-filter-by-regexp (column)
  "按正则表达式过滤指定列。"
  (interactive
   (list
    (let* ((columns '(("Status" . 0)
                      ("Author" . 1)
                      ("Title" . 2)
                      ("Keywords" . 4)))
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

(defun org-zettel-ref-filter-by-status (status)
  "按阅读状态过滤。"
  (interactive
   (list (intern (completing-read "Filter by status: "
                                 '("read" "unread")
                                 nil t))))
  (org-zettel-ref-add-filter
   (org-zettel-ref-make-status-filter status))
  (org-zettel-ref-list-refresh))

;;; 过滤器管理

(defun org-zettel-ref-add-filter (filter)
  "添加过滤条件。"
  (let ((column (car filter)))
    ;; 移除同一列的旧过滤条件
    (setq org-zettel-ref-active-filters
          (cl-remove-if (lambda (f) (= (car f) column))
                       org-zettel-ref-active-filters))
    ;; 添加新的过滤条件
    (push filter org-zettel-ref-active-filters)))

(defun org-zettel-ref-remove-filter (column)
  "移除指定列的过滤条件。"
  (setq org-zettel-ref-active-filters
        (cl-remove-if (lambda (f) (= (car f) column))
                     org-zettel-ref-active-filters))
  (org-zettel-ref-list-refresh))

(defun org-zettel-ref-clear-all-filters ()
  "清除所有过滤条件。"
  (interactive)
  (setq org-zettel-ref-active-filters nil)
  (org-zettel-ref-list-refresh))

;;; 应用过滤器

(defun org-zettel-ref-apply-filters (entries)
  "应用��有过滤条件到条目列。"
  (if (null org-zettel-ref-active-filters)
      entries
    (cl-remove-if-not
     (lambda (entry)
       (cl-every (lambda (filter)
                   (funcall (cdr filter) entry))
                org-zettel-ref-active-filters))
     entries)))



;;; 显示过滤状态

(defun org-zettel-ref-list--format-filter-info ()
  "格式化当前过滤条件信息。"
  (if org-zettel-ref-active-filters
      (format " [Filters: %s]"
              (mapconcat
               (lambda (filter)
                 (let ((col (aref tabulated-list-format (car filter))))
                   (format "%s" (car col))))
               org-zettel-ref-active-filters
               ","))
    ""))



  ;;;; 滤器配置的保存和加载功能

;;; 定义选项
(defcustom org-zettel-ref-filter-presets-file
  (expand-file-name "org-zettel-ref-filters.el" user-emacs-directory)
  "存储过滤器预设的文件路径。"
  :type 'file
  :group 'org-zettel-ref-filter)

(defcustom org-zettel-ref-filter-presets nil
  "预定义的过滤器配置。
每个配置是一个 (name . filters) 的 cons cell，其中：
- name 是配置名称
- filters 是过滤条件的列表"
  :type '(alist :key-type string
                :value-type (repeat (cons integer function)))
  :group 'org-zettel-ref-filter)

;;; 序列化和反序列化

(defun org-zettel-ref-filter-serialize (filter)
  "将过滤器转换为可保存的形式。"
  (let ((column (car filter))
        (predicate (cdr filter)))
    (cond
     ;; 状态过滤器
     ((and (= column 0)
           (string-match-p "Read\\|Unread"
                          (prin1-to-string predicate)))
      `(status . ,(if (string-match-p "Read"
                                     (prin1-to-string predicate))
                      'read 'unread)))
     ;; 正则表达式过滤器
     (t
      `(regexp . (,column . ,(nth 2 (car (read-from-string
                                          (prin1-to-string predicate))))))))))

(defun org-zettel-ref-filter-deserialize (spec)
  "将保存的过滤器规格转换回实际的过滤器。"
  (pcase spec
    (`(status . read)
     (org-zettel-ref-make-status-filter 'read))
    (`(status . unread)
     (org-zettel-ref-make-status-filter 'unread))
    (`(regexp . (,column . ,pattern))
     (org-zettel-ref-make-string-filter column pattern))))

;;; 保存和加载过滤器

(defun org-zettel-ref-filter-save-current (name)
  "将当前的过滤器配置保存为预设。"
  (interactive "sName for filter preset: ")
  (when org-zettel-ref-active-filters
    (let* ((serialized-filters
            (mapcar #'org-zettel-ref-filter-serialize
                   org-zettel-ref-active-filters))
           (existing (assoc name org-zettel-ref-filter-presets)))
      (if existing
          (setcdr existing serialized-filters)
        (push (cons name serialized-filters)
              org-zettel-ref-filter-presets))
      (org-zettel-ref-filter-save-presets)
      (message "Saved filter preset: %s" name))))

(defun org-zettel-ref-filter-save-presets ()
  "保存所有过滤器预设到文件。"
  (with-temp-file org-zettel-ref-filter-presets-file
    (let ((print-length nil)
          (print-level nil))
      (insert ";; -*- lexical-binding: t; -*-\n")
      (insert ";; Org-zettel-ref filter presets\n")
      (prin1 `(setq org-zettel-ref-filter-presets
                    ',org-zettel-ref-filter-presets)
             (current-buffer)))))

(defun org-zettel-ref-filter-load-presets ()
  "从文件加载过滤器预设。"
  (when (file-exists-p org-zettel-ref-filter-presets-file)
    (load org-zettel-ref-filter-presets-file)))

;;; 应用过滤器预设

(defun org-zettel-ref-filter-apply-preset (name)
  "应用指定名称的过滤器预设。"
  (interactive
   (list (completing-read "Apply filter preset: "
                         org-zettel-ref-filter-presets nil t)))
  (when-let ((preset (assoc name org-zettel-ref-filter-presets)))
    (setq org-zettel-ref-active-filters
          (mapcar #'org-zettel-ref-filter-deserialize
                  (cdr preset)))
    (org-zettel-ref-list-refresh)
    (message "Applied filter preset: %s" name)))

(defun org-zettel-ref-filter-delete-preset (name)
  "删除指定名称的过滤器预设。"
  (interactive
   (list (completing-read "Delete filter preset: "
                         org-zettel-ref-filter-presets nil t)))
  (setq org-zettel-ref-filter-presets
        (assoc-delete-all name org-zettel-ref-filter-presets))
  (org-zettel-ref-filter-save-presets)
  (message "Deleted filter preset: %s" name))

;;; 管理界面
(defun org-zettel-ref-filter-manage-presets ()
  "打开过滤器预设管理界面。"
  (interactive)
  (org-zettel-ref-preset-show)
  (message (substitute-command-keys 
            "Type \\[org-zettel-ref-preset-help] for help")))

(defun org-zettel-ref-filter--manage-presets-simple ()
  "简单的预设理界面（作为后备）。"
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

;;;; 过滤器预设管理界面

(define-derived-mode org-zettel-ref-preset-list-mode tabulated-list-mode "Org-Zettel-Ref Presets"
  "Major mode for managing org-zettel-ref filter presets."
  ;; 设置表格格式
  (setq tabulated-list-format
        [("Name" 20 t)                  ; 预设名称
         ("Description" 40 t)           ; 预设描述
         ("Filters" 0 t)])             ; 过滤器详情
  (setq tabulated-list-sort-key (cons "Name" nil))
  ;; 初始化表头
  (tabulated-list-init-header))

(defun org-zettel-ref-preset--format-filter-description (filter)
  "格式化过滤器的描述。"
  (pcase filter
    (`(status . ,status)
     (format "Status: %s" status))
    (`(regexp . (,column . ,pattern))
     (let ((col-name (car (aref tabulated-list-format column))))
       (format "%s matches \"%s\"" col-name pattern)))
    (_ (format "%S" filter))))

(defun org-zettel-ref-preset--get-entries ()
  "获取预设列表条。"
  (mapcar
   (lambda (preset)
     (let* ((name (car preset))
            (filters (cdr preset))
       ;; 通过第一个过滤器生成简短描述
       (let ((short-desc (if filters
                            (org-zettel-ref-preset--format-filter-description (car filters))
                          "No filters")))
         ;; 返回表格行
         (list name
               (vector
                (propertize name 'face 'font-lock-keyword-face)
                (propertize short-desc 'face 'font-lock-comment-face)
                filter-desc))))
   org-zettel-ref-filter-presets))))



(defun org-zettel-ref-preset-list-buffer ()
  "创建或切换到预设列表缓冲区。"
  (let ((buffer (get-buffer-create "*Org-Zettel-Ref Presets*")))
    (with-current-buffer buffer
      (org-zettel-ref-preset-list-mode)
      (setq tabulated-list-entries (org-zettel-ref-preset--get-entries))
      (tabulated-list-print t))
    buffer))

(defun org-zettel-ref-preset-show ()
  "示过滤器预设管理界面。"
  (interactive)
  (pop-to-buffer (org-zettel-ref-preset-list-buffer)
                 '((display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (side . right)
                   (window-width . 80))))

;;; 预设管理命令

(defun org-zettel-ref-preset-apply ()
  "应用当前行的预设。"
  (interactive)
  (when-let ((name (tabulated-list-get-id)))
    (quit-window)
    (org-zettel-ref-filter-apply-preset name)))

(defun org-zettel-ref-preset-add ()
  "添加新的预设。"
  (interactive)
  (let* ((name (read-string "Preset name: "))
         (desc (read-string "Description: ")))
    (when (and name (not (string-empty-p name)))
      (push (cons name '()) org-zettel-ref-filter-presets)
      (org-zettel-ref-filter-save-presets)
      (tabulated-list-print t))))

(defun org-zettel-ref-preset-delete ()
  "删除当前行的预设。"
  (interactive)
  (when-let ((name (tabulated-list-get-id)))
    (when (yes-or-no-p (format "Delete preset '%s'? " name))
      (setq org-zettel-ref-filter-presets
            (assoc-delete-all name org-zettel-ref-filter-presets))
      (org-zettel-ref-filter-save-presets)
      (tabulated-list-print t))))

(defun org-zettel-ref-preset-rename ()
  "重命名当前行的预设。"
  (interactive)
  (when-let* ((old-name (tabulated-list-get-id))
              (new-name (read-string "New name: " old-name)))
    (unless (string= old-name new-name)
      (let ((preset (assoc old-name org-zettel-ref-filter-presets)))
        (setf (car preset) new-name)
        (org-zettel-ref-filter-save-presets)
        (tabulated-list-print t)))))

(defun org-zettel-ref-preset-refresh ()
  "刷新预设列表。"
  (interactive)
  (setq tabulated-list-entries (org-zettel-ref-preset--get-entries))
  (tabulated-list-print t))

(defun org-zettel-ref-preset-save-current ()
  "保存当前过滤器为新预设。"
  (interactive)
  (let ((name (read-string "Save current filters as: ")))
    (when (and name (not (string-empty-p name)))
      (org-zettel-ref-filter-save-current name)
      (tabulated-list-print t))))

;;; 帮助信息

(defvar org-zettel-ref-preset-help-message
  "Filter Preset Commands:
  \\[org-zettel-ref-preset-apply]   - Apply preset at point
  \\[org-zettel-ref-preset-add]   - Add new preset
  \\[org-zettel-ref-preset-delete]   - Delete preset at point
  \\[org-zettel-ref-preset-rename]   - Rename preset at point
  \\[org-zettel-ref-preset-save-current]   - Save current filters as preset
  \\[org-zettel-ref-preset-refresh]   - Refresh preset list
  \\[quit-window]   - Exit preset manager"
  "Help message for the preset manager.")

(defun org-zettel-ref-preset-help ()
  "显示预设管理器的帮助信息。"
  (interactive)
  (let ((help-window-select t))
    (with-help-window "*Org-Zettel-Ref Preset Help*"
      (princ org-zettel-ref-preset-help-message))))

  ;; 添加错误处理和后备方案
(defun org-zettel-ref-filter-manage-presets-safely ()
  "安全地打开过滤器预设管理界面，出错时使用简单界面。"
  (interactive)
  (condition-case err
      (org-zettel-ref-filter-manage-presets)
    (error
     (message "Could not open preset manager: %s. Using simple interface."
              (error-message-string err))
     (org-zettel-ref-filter--manage-presets-simple))))



;;; 初始化
(org-zettel-ref-filter-load-presets)

;;;----------------------------------------------------------------------------
;;; Reading State Management
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-state-read-p (entry)
  "判断是否为已读状态。"
  (and entry (org-zettel-ref-db-entry-read-p entry)))

(defun org-zettel-ref-state-unread-p (entry)
  "判断是否为未读状态。"
  (and entry (org-zettel-ref-db-entry-unread-p entry)))

;;;----------------------------------------------------------------------------
;;; State String Functions
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-state-string (entry)
  "返回条目的状态显示字符串。"
  (if (org-zettel-ref-db-entry-read-p entry)
      (propertize "Read" 'face 'success)
    (propertize "Unread" 'face 'warning)))

;;;----------------------------------------------------------------------------
;;; State Display Functions
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-list-show-state ()
  "显示当前文件的阅读状态。"
  (interactive)
  (condition-case err
      (if-let* ((file (tabulated-list-get-id))
          (entry (org-zettel-ref-db-get-entry 
                 (org-zettel-ref-db-get-id-by-path file))))
          (message "Reading state: %s" 
                  (if (org-zettel-ref-db-entry-read-p entry)
                      "Read (Has overview file)"
                    "Unread (No overview file)"))
        (message "No entry selected"))
    (error (message "Error getting state: %s" err))))

;;;----------------------------------------------------------------------------
;;; Filtering Functions
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-filter-by-state (state)
  "Filter entries by state."
  (let ((pred (pcase state
               ('read #'org-zettel-ref-db-entry-read-p)
               ('unread #'org-zettel-ref-db-entry-unread-p))))
    (org-zettel-ref-add-filter 
     (cons 'state (lambda (entry)
                   (when-let ((info (org-zettel-ref-db-get-entry (car entry))))
                     (funcall pred info)))))))

;;;----------------------------------------------------------------------------
;;; File Path and Existence Functions
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-get-file-path (file)
  "获取文件的完整路径。"
  (when file
    (expand-file-name file org-zettel-ref-directory)))

(defun org-zettel-ref-file-exists-p (file)
  "检查文件是否存在。"
  (when file
    (file-exists-p (org-zettel-ref-get-file-path file))))


;;;----------------------------------------------------------------------------
;;; Handle Errors Operation
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-log-error (error-type error-context)
  "记录错误到日志文件。
ERROR-TYPE 是错误类型的符号，ERROR-CONTEXT 是错误的详细信息。"
  (let ((log-file (expand-file-name "org-zettel-ref-error.log" org-zettel-ref-directory))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] %s: %s\n" timestamp error-type error-context))
      (append-to-file (point-min) (point-max) log-file))))

(defun org-zettel-ref-handle-error (operation source err)
  "处理操作过程中的错误。
OPERATION 是操作类型，SOURCE 是错误来源，ERR 是错误详情。"
  (let ((err-msg (format "Error in %s: %s" operation err)))
    (message "%s" err-msg)
    (when (called-interactively-p 'any)
      (with-current-buffer (get-buffer-create "*Org-Zettel-Ref Error*")
        (goto-char (point-max))
        (insert (format-time-string "\n[%Y-%m-%d %H:%M:%S]\n"))
        (insert (format "Operation: %s\n" operation))
        (insert (format "Source: %s\n" source))
        (insert (format "Error: %s\n" err))
        (insert "Backtrace:\n")
        (insert (with-temp-buffer
                 (backtrace)
                 (buffer-string)))))))

;;;----------------------------------------------------------------------------
;;; Define Key Bindings
;;;----------------------------------------------------------------------------

(defvar org-zettel-ref-list-mode-map
  (let ((map (make-sparse-keymap)))
    ;; 件操作
    (define-key map (kbd "o") #'org-zettel-ref-list-open-file)
    (define-key map (kbd "r") #'org-zettel-ref-list-rename-file)
    (define-key map (kbd "d") #'org-zettel-ref-list-delete-file)
    
    ;; 过滤操作
    (define-key map (kbd "f") #'org-zettel-ref-filter)
    (define-key map (kbd "c") #'org-zettel-ref-clear-all-filters)
    
    ;; 状态查看
    (define-key map (kbd "s") #'org-zettel-ref-list-show-state)
    
    ;; 其他操作
    (define-key map (kbd "g") #'org-zettel-ref-list-refresh)
    (define-key map (kbd "?") #'org-zettel-ref-help)
    
    map)
  "Keymap for `org-zettel-ref-list-mode'.")

;;;----------------------------------------------------------------------------
;;; Help and Documentation
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-help ()
  "显示帮助信息。"
  (interactive)
  (with-help-window "*Org-Zettel-Ref Help*"
    (princ "Org-Zettel-Ref Help\n\n")
    (princ "Key bindings:\n")
    (princ "  o - Open file at point\n")
    (princ "  r - Rename file at point\n")
    (princ "  d - Delete file at point\n")
    (princ "  f - Apply filter (by regexp or status)\n")
    (princ "  c - Clear all filters\n")
    (princ "  s - Show reading state of current file\n")
    (princ "  g - Refresh file list\n")
    (princ "  ? - Show this help\n\n")
    (princ "Reading States:\n")
    (princ "  Read    - File has an associated overview file\n")
    (princ "  Unread  - File has no associated overview file\n\n")
    (princ "Notes:\n")
    (princ "- Reading state is determined by the existence of overview file mappings\n")
    (princ "- Use the filter system to show only read or unread files\n")
    (princ "- Overview files are managed automatically by the system\n")))



;;;----------------------------------------------------------------------------
;;; Main Entry Point
;;;----------------------------------------------------------------------------

(defun org-zettel-ref-list ()
  "显示阅读列表界面。"
  (interactive)
  (let ((buffer (get-buffer-create "*Org-Zettel-Ref-List*")))
    (with-current-buffer buffer
      (org-zettel-ref-list-mode)
      (add-hook 'org-zettel-ref-db-state-change-hook
                #'org-zettel-ref-list-refresh)
      (org-zettel-ref-list-refresh))
    (switch-to-buffer buffer)
    (message "Type ? for help")))


(provide 'org-zettel-ref-reading-manager)
