;;; org-zettel-ref-highlight-simple.el --- Simple highlighting with target links -*- lexical-binding: t; -*-

;;; Commentary:
;; 使用格式：
;; <<hl-1>> §q{高亮的文本}  <- 在源文件中
;; ❓ 高亮的文本 <- 在概览文件中显示的格式



;; 高亮类型定义
(defcustom org-zettel-ref-highlight-types
  '(("q" . (:face (:background "#FFE0B2" :foreground "#000000" :extend t)
            :name "问题"
            :prefix "❓"))
    ("f" . (:face (:background "#B2DFDB" :foreground "#000000" :extend t)
            :name "事实"
            :prefix "📝"))
    ("m" . (:face (:background "#BBDEFB" :foreground "#000000" :extend t)
            :name "方法"
            :prefix "🔧"))
    ("p" . (:face (:background "#E1BEE7" :foreground "#000000" :extend t)
            :name "过程"
            :prefix "⚙️"))
    ("d" . (:face (:background "#F8BBD0" :foreground "#000000" :extend t)
            :name "定义"
            :prefix "📖")))
  "配置高亮类型."
  :type '(alist :key-type string
                :value-type (plist :key-type symbol :value-type sexp))
  :group 'org-zettel-ref)

;; 高亮计数器
(defvar-local org-zettel-ref-highlight-counter 0
  "高亮标记的全局计数器.")

;; 正则表达式
(defconst org-zettel-ref-highlight-regexp
  "<<hl-\\([0-9]+\\)>>[ \t]*§\\([qfmpd]\\){\\([^}]+\\)}"
  "匹配高亮标记的正则表达式.")

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
  "刷新所有高亮的显示效果."
  (interactive)
  (remove-overlays nil nil 'org-zettel-ref-highlight t)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
      (let* ((type (match-string 2))
             (config (cdr (assoc type org-zettel-ref-highlight-types)))
             (face (plist-get config :face))
             ;; 获取各个部分的位置
             (target-start (match-beginning 0))
             (target-end (match-end 1))
             (highlight-start (1+ target-end))  ; 跳过闭合的 ">>"
             (highlight-end (match-end 0))
             ;; 创建 overlays
             (target-ov (make-overlay target-start (+ target-end 2))) ; 包括 ">>"
             (highlight-ov (make-overlay highlight-start highlight-end)))
        
        ;; 设置 target overlay
        (overlay-put target-ov 'display "")  ; 不是隐藏，而是显示为空字符串
        (overlay-put target-ov 'org-zettel-ref-highlight t)
        
        ;; 设置高亮 overlay
        (overlay-put highlight-ov 'face face)
        (overlay-put highlight-ov 'org-zettel-ref-highlight t)))))

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

(defun org-zettel-ref-sync-highlights ()
  "同步所有高亮到概览文件，保留手动添加的笔记."
  (interactive)
  (when (and org-zettel-ref-overview-file
             (file-exists-p org-zettel-ref-overview-file))
    (let ((highlights '())
          (source-file (buffer-file-name)))
      ;; 收集高亮内容
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
          (let* ((ref (match-string 1))
                 (type (match-string 2))
                 (text (match-string 3))
                 (config (cdr (assoc type org-zettel-ref-highlight-types)))
                 (name (plist-get config :name))
                 (prefix (plist-get config :prefix)))
            (push (list ref type text name prefix) highlights))))
      
      ;; 更新概览文件
      (with-current-buffer (find-file-noselect org-zettel-ref-overview-file)
        (org-with-wide-buffer
         ;; 如果文件为空，添加标题
         (goto-char (point-min))
         (unless (re-search-forward "^\\* Highlights Overview" nil t)
           (insert "* Highlights Overview\n\n"))
         
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
                  (heading-regexp (format "^\\* .* \\[\\[file:.*::hl-%s\\]" ref)))
             
             ;; 查找是否存在对应的条目
             (goto-char (point-min))
             (if (re-search-forward heading-regexp nil t)
                 ;; 更新已存在的条目
                 (progn
                   (beginning-of-line)
                   (let ((element (org-element-at-point)))
                     ;; 保存当前标题下的内容
                     (let* ((begin (org-element-property :begin element))
                            (end (org-element-property :end element))
                            (content (buffer-substring begin end)))
                       ;; 只替换标题行，保留其他内容
                       (delete-region begin end)
                       (insert (format "* %s [[file:%s::hl-%s][hl-%s]] %s\n"
                                     prefix
                                     source-file
                                     ref
                                     ref
                                     text))
                       ;; 恢复标题下的内容（跳过第一行）
                       (when (string-match "\n\\(.*\\)\\'" content)
                         (insert (match-string 1 content))))))
               ;; 添加新条目
               (goto-char (point-max))
               (insert (format "\n* %s [[file:%s::hl-%s][hl-%s]] %s\n"
                             prefix
                             source-file
                             ref
                             ref
                             text))))))
        (save-buffer)))))

(defun org-zettel-ref-remove-highlight-at-point ()
  "移除光标处的高亮标记，保留原文本."
  (interactive)
  (save-excursion
    (let* ((pos (point))
           (found nil))
      ;; 寻找当前行的高亮标记
      (beginning-of-line)
      (when (re-search-forward org-zettel-ref-highlight-regexp (line-end-position) t)
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
             (new-text (read-string "编辑高亮内容: " old-text)))
        (unless (string= old-text new-text)
          (save-excursion
            (goto-char (car bounds))
            (delete-region (car bounds) (cdr bounds))
            (insert (format "<<hl-%s>> §%s{%s}"
                          ref type new-text))
            (org-zettel-ref-highlight-refresh)
            (org-zettel-ref-sync-highlights)))))))

;; 获取高亮位置和内容的辅助函数
(defun org-zettel-ref-highlight-at-point ()
  "判断光标是否在高亮区域内."
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


(org-link-set-parameters 
 "file" 
 :follow (lambda (path) 
           (if (string-match "::" path)
               (org-zettel-ref-follow-link-and-highlight)
             (org-link-open-as-file path nil))))

(defun org-zettel-ref-highlight-enable ()
  "启用高亮模式并初始化计数器."
  ;; 确保设置了 buffer-local 变量
  (make-local-variable 'org-zettel-ref-highlight-counter)
  ;; 初始化计数器
  (org-zettel-ref-highlight-initialize-counter)
  ;; 刷新显示
  (org-zettel-ref-highlight-refresh))

(define-minor-mode org-zettel-ref-highlight-mode
  "Minor mode for editable highlighting in org-zettel-ref."
  :lighter " ZR-HL"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c h h") #'org-zettel-ref-highlight-region)
            (define-key map (kbd "C-c h r") #'org-zettel-ref-highlight-refresh)
            (define-key map (kbd "C-c h e") #'org-zettel-ref-highlight-edit)
            map)
  (if org-zettel-ref-highlight-mode
      (progn
        (org-zettel-ref-highlight-initialize-counter)
        (org-zettel-ref-highlight-refresh)
        (add-hook 'after-save-hook #'org-zettel-ref-sync-highlights nil t)
        (add-hook 'after-change-functions #'org-zettel-ref-highlight-after-change nil t))
    (progn
      (remove-overlays nil nil 'org-zettel-ref-highlight t)
      (remove-hook 'after-save-hook #'org-zettel-ref-sync-highlights t)
      (remove-hook 'after-change-functions #'org-zettel-ref-highlight-after-change t))))

(defun org-zettel-ref-verify-highlight (start end)
  "验证START到END区域内的高亮标记是否正确插入."
  (let ((text (buffer-substring-no-properties start end)))
    (message "Verification - Text at region: %s" text)
    (when (string-match org-zettel-ref-highlight-regexp text)
      (message "Verification - Found target: %s" (match-string 1 text))
      (message "Verification - Found type: %s" (match-string 2 text))
      (message "Verification - Found content: %s" (match-string 3 text)))))

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

(provide 'org-zettel-ref-highlight)
