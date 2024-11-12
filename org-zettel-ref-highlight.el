;;; org-zettel-ref-highlight-simple.el --- Simple highlighting with target links -*- lexical-binding: t; -*-

;;; Commentary:
;; 使用格式：
;; <<hl-1>> §q{高亮的文本}  <- 在源文件中
;; ❓ 高亮的文本 <- 在概览文件中显示的格式

;;----------------------------------------------------------------------------  
;; Variables
;;----------------------------------------------------------------------------

(defcustom org-zettel-ref-highlight-types
  '(("question" . (:face (:background "#FFE0B2" :foreground "#000000" :extend t)
            :name "问题"
            :prefix "❓"))
    ("fact" . (:face (:background "#B2DFDB" :foreground "#000000" :extend t)
            :name "事实"
            :prefix "📝"))
    ("method" . (:face (:background "#BBDEFB" :foreground "#000000" :extend t)
            :name "方法"
            :prefix "🔧"))
    ("process" . (:face (:background "#E1BEE7" :foreground "#000000" :extend t)
            :name "过程"
            :prefix "⚙️"))
    ("definition" . (:face (:background "#F8BBD0" :foreground "#000000" :extend t)
            :name "定义"
            :prefix "📖"))
    ("note" . (:face (:background "#E8EAF6" :foreground "#000000" :extend t)
            :name "笔记"
            :prefix "📌"))
    ("image" . (:face (:background "#FFECB3" :foreground "#000000" :extend t)
            :name "图片"
            :prefix "🖼️")))
  "配置高亮类型."
  :type '(alist :key-type string
                :value-type (plist :key-type symbol :value-type sexp))
  :group 'org-zettel-ref)


(defvar-local org-zettel-ref-highlight-counter 0
  "高亮标记的全局计数器.")

;; 正则表达式
(defcustom org-zettel-ref-highlight-regexp
  "<<hl-\\([0-9]+\\)>> §\\([a-z]\\){\\([^}]+\\)}"
  "用于匹配高亮标记的正则表达式.
组1: 引用ID
组2: 类型
组3: 内容（对于图片包括路径和描述）"
  :type 'string
  :group 'org-zettel-ref)

;;----------------------------------------------------------------------------
;; Highlight Display
;;----------------------------------------------------------------------------

;; 生成高亮 ID
(defun org-zettel-ref-highlight-generate-id ()
  "生成下一个高亮 ID."
  (setq-local org-zettel-ref-highlight-counter 
              (1+ org-zettel-ref-highlight-counter))
  (number-to-string org-zettel-ref-highlight-counter))

;; 高亮区域函数
(defun org-zettel-ref-highlight-region (type)
  "用指定类型TYPE高亮当前区域."
  (interactive
   (list (completing-read "高亮类型: "
                         (mapcar #'car org-zettel-ref-highlight-types)
                         nil t)))
  (when (use-region-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           ;; 确保 end 在光标后面
           (end (if (= end (point))
                   (min (point-max) (1+ end))
                 end))
           (text (buffer-substring-no-properties beg end))
           (highlight-id (org-zettel-ref-highlight-generate-id)))
      
      ;; 删除选区并插入高亮标记
      (delete-region beg end)
      (goto-char beg)
      (insert (format "%s <<hl-%s>> §%s{%s}"
                     text  ; 先插入原文本
                     highlight-id
                     type
                     text))  ; 然后是高亮标记
      
      ;; 刷新显示
      (org-zettel-ref-highlight-refresh))))

;; 刷新显示效果
(defun org-zettel-ref-highlight-refresh ()
  "刷新当前buffer中所有高亮的显示."
  (interactive)
  ;; 清除现有的 overlays
  (remove-overlays (point-min) (point-max) 'org-zettel-ref-highlight t)
  
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
      (let* ((type (match-string 2))
             (config (cdr (assoc type org-zettel-ref-highlight-types)))
             (beg (match-beginning 0))
             (end (match-end 0)))
        (when config
          ;; 创建新的 overlay
          (let ((ov (make-overlay beg end)))
            (overlay-put ov 'org-zettel-ref-highlight t)
            (overlay-put ov 'face (plist-get config :face))
            ;; 对于图片类型，只高亮描述部分
            (when (string= type "i")
              (let* ((text (match-string 3))
                     (parts (split-string text "|"))
                     (desc (cadr parts)))
                (when desc
                  ;; 可以添加特殊的图片标记
                  (overlay-put ov 'after-string "🖼️"))))))))))

(defun org-zettel-ref-toggle-target-display ()
  "切换是否显示 target 标记."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((showing nil))
      (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
        (let* ((target-start (match-beginning 0))
               (target-end (+ (match-end 1) 2))
               (overlays (overlays-in target-start target-end)))
          (dolist (ov overlays)
            (when (overlay-get ov 'org-zettel-ref-highlight)
              (setq showing (not (equal (overlay-get ov 'display) "")))
              (overlay-put ov 'display (if showing "" nil))))))
      (message "Target marks are now %s" (if showing "hidden" "visible")))))     

;;----------------------------------------------------------------------------
;; Synchronization
;;----------------------------------------------------------------------------  

(defun org-zettel-ref-get-source-from-overview ()
  "从当前 overview buffer 获取对应的 source 文件路径."
  (let* ((db (org-zettel-ref-ensure-db))
         (overview-file (buffer-file-name))
         (overview-id (gethash overview-file (org-zettel-ref-db-overview-paths db))))
    (when overview-id
      (let* ((overview-entry (gethash overview-id (org-zettel-ref-db-overviews db)))
             (ref-id (org-zettel-ref-overview-entry-ref-id overview-entry))
             (ref-entry (org-zettel-ref-db-get-ref-entry db ref-id)))
        (org-zettel-ref-ref-entry-file-path ref-entry)))))

(defun org-zettel-ref-sync-highlights ()
  "同步所有高亮到概览文件，使用增量更新策略."
  (interactive)
  (when (and org-zettel-ref-overview-file
             (file-exists-p org-zettel-ref-overview-file))
    (let ((highlights nil))
      ;; 收集高亮内容
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
          (let* ((ref (or (match-string 1) ""))
                 (type (or (match-string 2) ""))
                 (text (or (match-string 3) ""))
                 (config (cdr (assoc type org-zettel-ref-highlight-types))))
            (when (and type (not (string-empty-p type)) config)
              (let ((name (plist-get config :name))
                    (prefix (plist-get config :prefix)))
                (if (string= type "i")
                    (let* ((img-parts (split-string text "|"))
                          (img-path (car img-parts))
                          (img-desc (cadr img-parts)))
                      (when (and img-path (not (string-empty-p img-path)))
                        (push (list ref type text name prefix img-path img-desc)
                              highlights)))
                  (push (list ref type text name prefix nil nil)
                        highlights)))))))
      
      ;; 更新概览文件
      (with-current-buffer (find-file-noselect org-zettel-ref-overview-file)
        (org-with-wide-buffer
         
         ;; 更新或添加每个高亮
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
             
             ;; 查找是否存在对应的条目
             (goto-char (point-min))
             (if (re-search-forward heading-regexp nil t)
                 ;; 更新已存在的条目的标题行
                 (progn
                   (beginning-of-line)
                   (delete-region (point) (line-end-position))
                   (insert (format "* %s [[hl:%s][hl-%s]] %s"
                                 prefix
                                 ref
                                 ref
                                 (if (string= type "i") 
                                     (or img-desc "")
                                     text)))
                   ;; 图片处理部分
                   (when (and (string= type "i") img-path)
                     (forward-line)
                     (unless (looking-at "\\(#\\+ATTR_ORG:.*\n\\)?\\[\\[file:")
                       (insert "\n#+ATTR_ORG: :width 300\n")
                       (insert (format "[[file:%s]]\n" img-path)))))
               
               ;; 添加新条目
               (goto-char (point-max))
               (insert (format "\n* %s [[hl:%s][hl-%s]] %s"
                             prefix
                             ref
                             ref
                             (if (string= type "i")
                                 (or img-desc "")
                                 text)))
               ;; 图片处理部分
               (when (and (string= type "i") img-path)
                 (insert "\n#+ATTR_ORG: :width 300\n")
                 (insert (format "[[file:%s]]\n" img-path))))))
         
         ;; 保存更新后的文件
         (save-buffer))))))


;;----------------------------------------------------------------------------
;; Image Handling
;;----------------------------------------------------------------------------  

(defun org-zettel-ref-highlight--check-init ()
  "检查是否已经初始化."
  (unless (and org-zettel-ref-overview-file
               (stringp org-zettel-ref-overview-file)
               (file-exists-p org-zettel-ref-overview-file))
    (user-error "请先运行 M-x org-zettel-ref-init 初始化系统")))

(defun org-zettel-ref-highlight--ensure-image-dir ()
  "确保 overview 文件夹中的 Images 目录存在."
  (org-zettel-ref-highlight--check-init)  ; 先检查初始化
  (let* ((overview-dir (file-name-directory 
                       (expand-file-name org-zettel-ref-overview-file)))
         (image-dir (expand-file-name "Images" overview-dir)))
    (unless (file-exists-p image-dir)
      (make-directory image-dir t))
    image-dir))

(defun org-zettel-ref-highlight--copy-image (source-path)
  "将图片复制到 Images 目录，返回新的相对路径."
  (let* ((image-dir (org-zettel-ref-highlight--ensure-image-dir))
         (file-name (file-name-nondirectory source-path))
         ;; 生成唯一的文件名（使用时间戳）
         (new-name (format "%s-%s" 
                          (format-time-string "%Y%m%d-%H%M%S")
                          file-name))
         (dest-path (expand-file-name new-name image-dir)))
    (copy-file source-path dest-path t)
    ;; 返回相对于 overview 文件的路径
    (concat "Images/" new-name)))

;; 2. 修改图片高亮函数
(defun org-zettel-ref-highlight-image ()
  "为当前位置的图片添加高亮标记，并复制图片到 Images 目录."
  (interactive)
  (org-zettel-ref-highlight--check-init)
  (save-excursion
    (let ((context (org-element-context)))
      (when (and (eq (org-element-type context) 'link)
                 (string= (org-element-property :type context) "file"))
        (let* ((path (org-element-property :path context))
               (abs-path (expand-file-name path (file-name-directory (buffer-file-name))))
               (link-end (org-element-property :end context))
               (description (read-string "图片描述 (可选): ")))
          (when (and (string-match-p "\\.\\(jpg\\|jpeg\\|png\\|gif\\|svg\\|webp\\)$" path)
                    (file-exists-p abs-path))
            ;; 复制图片到 Images 目录
            (let ((new-path (org-zettel-ref-highlight--copy-image abs-path)))
              ;; 移动到链接所在行的末尾并插入换行
              (goto-char link-end)
              (end-of-line)
              (insert "\n")
              ;; 在新行添加高亮标记
              (let ((highlight-id (org-zettel-ref-highlight-generate-id)))
                (insert (format "<<hl-%s>> §i{%s|%s}"
                              highlight-id
                              new-path
                              (or description "")))
                (org-zettel-ref-highlight-refresh)))))))))

;;----------------------------------------------------------------------------
;; Highlight Editing
;;----------------------------------------------------------------------------

(defun org-zettel-ref-remove-highlight-at-point ()
  "移除光标处的高亮标记，保留原文本."
  (interactive)
  (save-excursion
    (let* ((pos (point))
           (found nil))
      ;; 寻找当前行的高亮标记
      (beginning-of-line)
      (when (re-search-forward org-zettel-ref-hig1hlight-regexp (line-end-position) t)
        (let* ((target-start (match-beginning 0))
               (target-end (match-end 0))
               (text (match-string 3)))  ; 提取原文本
          (setq found t)
          ;; 确认删除
          (when (y-or-n-p "Remove highlight mark? ")
            ;; 删除标记并保留原文本
            (delete-region target-start target-end)
            (goto-char target-start)
            (insert text)
            ;; 同步概览文件
            (org-zettel-ref-sync-highlights)
            (message "Highlight mark removed"))))
      (unless found
        (message "No highlight mark found at point")))))

;; 编辑高亮内容
(defun org-zettel-ref-highlight-edit ()
  "编辑当前光标下的高亮文本."
  (interactive)
  (save-excursion
    (when (org-zettel-ref-highlight-at-point)
      (let* ((bounds (org-zettel-ref-highlight-get-bounds))
             (old-text (org-zettel-ref-highlight-get-text bounds))
             (type (org-zettel-ref-highlight-get-type bounds))
             (ref (org-zettel-ref-highlight-get-ref bounds))
             (new-text (read-string "编辑高亮内: " old-text)))
        (unless (string= old-text new-text)
          (save-excursion
            (goto-char (car bounds))
            (delete-region (car bounds) (cdr bounds))
            (insert (format "<<hl-%s>> §%s{%s}"
                          ref type new-text))
            (org-zettel-ref-highlight-refresh)
            (org-zettel-ref-sync-highlights)))))))

(defun org-zettel-ref-highlight-edit-note ()
  "编辑当前笔记的内容."
  (interactive)
  (when (org-zettel-ref-highlight-at-point)
    (let* ((bounds (org-zettel-ref-highlight-get-bounds))
           (ref (org-zettel-ref-highlight-get-ref bounds))
           (type (org-zettel-ref-highlight-get-type bounds))
           (old-text (org-zettel-ref-highlight-get-text bounds)))
      (when (string= type "n")  ; 确保是笔记类型
        (let ((new-text (read-string "编辑笔记: " old-text)))
          (unless (string= old-text new-text)
            (save-excursion
              (goto-char (car bounds))
              (delete-region (car bounds) (cdr bounds))
              (insert (format "<<hl-%s>> §n{%s}"
                            ref new-text))
              (org-zettel-ref-highlight-refresh)
              (org-zettel-ref-sync-highlights))))))))

;;----------------------------------------------------------------------------
;; Helper Functions
;;----------------------------------------------------------------------------

;; 获取高亮位置和内容的辅助函数
(defun org-zettel-ref-highlight-at-point ()
  "断光标是否在高亮区域内."
  (save-excursion
    (let ((pos (point)))
      (beginning-of-line)
      (when (re-search-forward org-zettel-ref-highlight-regexp (line-end-position) t)
        (and (>= pos (match-beginning 0))
             (<= pos (match-end 0)))))))

(defun org-zettel-ref-highlight-get-bounds ()
  "获取当前高亮的起始和结束位置."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward org-zettel-ref-highlight-regexp (line-end-position) t)
      (cons (match-beginning 0) (match-end 0)))))

(defun org-zettel-ref-highlight-get-text (bounds)
  "获取指定范围内的高亮文本."
  (save-excursion
    (goto-char (car bounds))
    (when (re-search-forward org-zettel-ref-highlight-regexp (cdr bounds) t)
      (match-string 3))))

(defun org-zettel-ref-highlight-get-type (bounds)
  "获取指定范围内的高亮类型."
  (save-excursion
    (goto-char (car bounds))
    (when (re-search-forward org-zettel-ref-highlight-regexp (cdr bounds) t)
      (match-string 2))))

(defun org-zettel-ref-highlight-get-ref (bounds)
  "获取指定范围内的高亮引用编号."
  (save-excursion
    (goto-char (car bounds))
    (when (re-search-forward org-zettel-ref-highlight-regexp (cdr bounds) t)
      (match-string 1))))

;; 初始化高亮计数器
(defun org-zettel-ref-highlight-initialize-counter ()
  "扫描当前buffer中的所有高亮标记，初始化计数器为最大值."
  (save-excursion
    (goto-char (point-min))
    (let ((max-id 0))
      ;; 扫描所有高亮标记
      (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
        (when-let* ((id-str (match-string 1))
                    (id-num (string-to-number id-str)))
          (setq max-id (max max-id id-num))))
      ;; 设置计数器为找到的最大值
      (setq-local org-zettel-ref-highlight-counter max-id))))

(defun org-zettel-ref-follow-link-and-highlight ()
  "跳转到链接目标并高亮显示."
  (let* ((link-prop (org-element-context))
         (target-file (org-element-property :path link-prop))
         (target-id (org-element-property :search-option link-prop)))
    (when (and target-file target-id)
      (find-file target-file)
      (goto-char (point-min))
      (when (re-search-forward (concat "<<" target-id ">>") nil t)
        (org-show-context)
        (recenter)))))

;; 1. 定义 hl 链接类型
(org-link-set-parameters 
 "hl"
 :follow (lambda (path)
           (let* ((db (org-zettel-ref-ensure-db))
                  (overview-file (buffer-file-name))
                  (overview-id (gethash overview-file (org-zettel-ref-db-overview-paths db)))
                  (overview-entry (gethash overview-id (org-zettel-ref-db-overviews db)))
                  (ref-id (org-zettel-ref-overview-entry-ref-id overview-entry))
                  (ref-entry (gethash ref-id (org-zettel-ref-db-refs db)))
                  (source-file (org-zettel-ref-ref-entry-file-path ref-entry))
                  (target-mark (concat "<<hl-" path ">>"))
                  (source-buffer (find-file-noselect source-file)))
             
             (unless source-file
               (user-error "Cannot find source file for this overview"))
             
             ;; 在源文件 buffer 中搜索
             (with-current-buffer source-buffer
               (widen)
               (goto-char (point-min))
               (message "DEBUG: Buffer size: %d" (buffer-size))
               (message "DEBUG: Current point: %d" (point))
               
               (let ((case-fold-search nil))  ; 区分大小写
                 (if (re-search-forward target-mark nil t)
                     (let ((target-pos (match-beginning 0)))
                       ;; 先切换到源文件 buffer
                       (pop-to-buffer source-buffer)
                       ;; 然后移动到目标位置
                       (goto-char target-pos)
                       (org-reveal)
                       (recenter))
                   (message "DEBUG: Search failed. Buffer content sample:")
                   (message "DEBUG: %s" 
                           (buffer-substring-no-properties 
                            (point-min)
                            (min (point-max) 500)))
                   (user-error "Target not found: %s" target-mark)))))))

(defun org-zettel-ref-highlight-enable ()
  "启用高亮模式并初始化计数器."
  ;; 确保设置了 buffer-local 变量
  (make-local-variable 'org-zettel-ref-highlight-counter)
  ;; 初始化计数器
  (org-zettel-ref-highlight-initialize-counter)
  ;; 刷新显示
  (org-zettel-ref-highlight-refresh))


(defun org-zettel-ref-highlight-debug-counter ()
  "显示当前buffer的高亮计数器状态."
  (interactive)
  (let ((current-counter org-zettel-ref-highlight-counter)
        (max-found (org-zettel-ref-highlight-initialize-counter)))
    (message "Current counter: %d, Maximum found in buffer: %d"
             current-counter max-found)))

(defun org-zettel-ref-highlight-debug-info ()
  "显示当前buffer的高亮调试信息."
  (interactive)
  (message "当前计数器值: %s" org-zettel-ref-highlight-counter)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
        (cl-incf count)
        (message "找到高亮 #%d: %s" count (match-string 0)))
      (message "共找到 %d 个高亮标记" count))))

(defun org-zettel-ref-highlight-add-note ()
  "添加一则独立笔记，使用高亮系统的ID计数."
  (interactive)
  (let* ((note-text (read-string "输入笔记内容: "))
         (highlight-id (org-zettel-ref-highlight-generate-id)))
    (insert (format "<<hl-%s>> §n{%s}"
                   highlight-id
                   note-text))
    (org-zettel-ref-highlight-refresh)))

;; 修改 after-change 处理函数
(defun org-zettel-ref-highlight-after-change (beg end _len)
  "处理文本变化后的高亮更新."
  (save-excursion
    (goto-char beg)
    (let ((line-beg (line-beginning-position))
          (line-end (line-end-position)))
      (when (and (>= end line-beg)
                 (<= beg line-end)
                 (string-match org-zettel-ref-highlight-regexp
                             (buffer-substring-no-properties line-beg line-end)))
        ;; 刷新显示
        (org-zettel-ref-highlight-refresh)
        ;; 同步到 overview
        (when (and (boundp 'org-zettel-ref-overview-file)
                  org-zettel-ref-overview-file)
          (org-zettel-ref-sync-highlights))))))


(provide 'org-zettel-ref-highlight)
