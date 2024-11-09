;;; org-zettel-ref-highlight-block.el --- Block-based highlighting for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:
;; 使用 org special block 实现高亮功能：
;; #+begin_highlight-q :id "unique-id"
;; 这是一个问题...
;; #+end_highlight-q
;;
;; 在概览文件中可以通过 ID 链接跳转回源文件对应位置


(require 'org-element)
(require 'org-id)

;; 高亮类型定义
(defcustom org-zettel-ref-highlight-types
  '(("highlight-q" . (:face (:background "#FFE0B2" :foreground "#000000" :extend t)
                     :name "问题"
                     :prefix "❓"))
    ("highlight-f" . (:face (:background "#B2DFDB" :foreground "#000000" :extend t)
                     :name "事实"
                     :prefix "📝"))
    ("highlight-m" . (:face (:background "#BBDEFB" :foreground "#000000" :extend t)
                     :name "方法"
                     :prefix "🔧"))
    ("highlight-p" . (:face (:background "#E1BEE7" :foreground "#000000" :extend t)
                     :name "过程"
                     :prefix "⚙️"))
    ("highlight-d" . (:face (:background "#F8BBD0" :foreground "#000000" :extend t)
                     :name "定义"
                     :prefix "📖")))
  "Configuration for highlight blocks."
  :type '(alist :key-type string
                :value-type (plist :key-type symbol :value-type sexp))
  :group 'org-zettel-ref)

;; 创建高亮
(defun org-zettel-ref-highlight-region (type)
  "将选中区域转换为高亮 block."
  (interactive
   (list (completing-read "高亮类型: "
                         (mapcar (lambda (x) (string-remove-prefix "highlight-" (car x)))
                               org-zettel-ref-highlight-types)
                         nil t)))
  (when (use-region-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (text (buffer-substring-no-properties beg end))
           (highlight-id (org-id-new)))
      (delete-region beg end)
      (insert (format "#+begin_highlight-%s :id \"%s\"\n%s\n#+end_highlight-%s"
                     type highlight-id text type))
      (org-zettel-ref-highlight-refresh))))

;; 移除高亮
(defun org-zettel-ref-highlight-remove ()
  "移除当前 highlight block."
  (interactive)
  (when-let* ((element (org-element-at-point))
              (is-block (eq (org-element-type element) 'special-block))
              (block-type (org-element-property :type element))
              (is-highlight (string-prefix-p "highlight-" block-type)))
    (let* ((beg (org-element-property :begin element))
           (end (org-element-property :end element))
           (contents-begin (org-element-property :contents-begin element))
           (contents-end (org-element-property :contents-end element))
           (contents (buffer-substring-no-properties contents-begin contents-end)))
      (delete-region beg end)
      (insert contents))))

;; 更改高亮类型
(defun org-zettel-ref-highlight-change-type ()
  "更改当前 highlight block 的类型."
  (interactive)
  (when-let* ((element (org-element-at-point))
              (is-block (eq (org-element-type element) 'special-block))
              (block-type (org-element-property :type element))
              (is-highlight (string-prefix-p "highlight-" block-type)))
    (let* ((beg (org-element-property :begin element))
           (end (org-element-property :end element))
           (contents-begin (org-element-property :contents-begin element))
           (contents-end (org-element-property :contents-end element))
           (contents (buffer-substring-no-properties contents-begin contents-end))
           (highlight-id (org-element-property :id (org-element-property :parameters element)))
           (new-type (completing-read "新的高亮类型: "
                                    (mapcar (lambda (x) 
                                            (string-remove-prefix "highlight-" (car x)))
                                          org-zettel-ref-highlight-types)
                                    nil t)))
      (delete-region beg end)
      (insert (format "#+begin_highlight-%s :id \"%s\"\n%s\n#+end_highlight-%s"
                     new-type highlight-id contents new-type)))))

;; 刷新显示
(defun org-zettel-ref-highlight-refresh ()
  "刷新所有高亮 block 的显示."
  (interactive)
  (remove-overlays nil nil 'org-zettel-ref-highlight t)
  (org-element-map (org-element-parse-buffer) 'special-block
    (lambda (block)
      (let* ((block-type (org-element-property :type block))
             (config (cdr (assoc block-type org-zettel-ref-highlight-types))))
        (when config
          (let* ((face (plist-get config :face))
                 (beg (org-element-property :begin block))
                 (end (org-element-property :end block))
                 (ov (make-overlay beg end)))
            (overlay-put ov 'face face)
            (overlay-put ov 'org-zettel-ref-highlight t)))))))

;; 同步到概览文件
(defun org-zettel-ref-sync-highlights ()
  "同步高亮内容到概览文件."
  (interactive)
  (when (and org-zettel-ref-overview-file
             (file-exists-p org-zettel-ref-overview-file))
    (let ((highlights '()))
      ;; 收集高亮内容
      (org-element-map (org-element-parse-buffer) 'special-block
        (lambda (block)
          (let* ((block-type (org-element-property :type block))
                 (config (cdr (assoc block-type org-zettel-ref-highlight-types)))
                 (highlight-id (org-element-property :id 
                                                   (org-element-property :parameters block))))
            (when (and config highlight-id)
              (let* ((name (plist-get config :name))
                     (prefix (plist-get config :prefix))
                     (contents-begin (org-element-property :contents-begin block))
                     (contents-end (org-element-property :contents-end block))
                     (contents (string-trim
                              (buffer-substring-no-properties contents-begin contents-end))))
                (push (list name prefix contents highlight-id) highlights))))))
      
      ;; 更新概览文件
      (with-current-buffer (find-file-noselect org-zettel-ref-overview-file)
        (save-excursion
          (goto-char (point-min))
          ;; 找到或创建高亮部分
          (unless (re-search-forward "^\\* Highlights$" nil t)
            (goto-char (point-max))
            (insert "\n* Highlights\n"))
          ;; 清除现有内容
          (let ((begin (point)))
            (if (re-search-forward "^\\*" nil t)
                (goto-char (match-beginning 0))
              (goto-char (point-max)))
            (delete-region begin (point)))
          ;; 按类型分组插入高亮
          (let ((sorted-highlights
                 (sort highlights
                       (lambda (a b) (string< (car a) (car b))))))
            (let (current-type)
              (dolist (highlight sorted-highlights)
                (let ((type (nth 0 highlight))
                      (prefix (nth 1 highlight))
                      (text (nth 2 highlight))
                      (id (nth 3 highlight)))
                  (unless (equal type current-type)
                    (insert (format "** %s\n" type))
                    (setq current-type type))
                  ;; 使用自定义 ID 链接
                  (insert (format "   - %s [[highlight:%s][%s]]\n"
                                prefix id text))))))
          (save-buffer))))))

;; 自定义链接类型处理
(org-link-set-parameters
 "highlight"
 :follow (lambda (id _)
           (let ((marker (org-id-find-id-in-file id (buffer-file-name))))
             (if marker
                 (progn
                   (pop-to-buffer (marker-buffer marker))
                   (goto-char (marker-position marker)))
               (message "Cannot find highlight with ID: %s" id)))))

;; Minor mode 定义
(define-minor-mode org-zettel-ref-highlight-mode
  "Minor mode for block-based highlighting in org-zettel-ref."
  :lighter " ZR-HL"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c h h") #'org-zettel-ref-highlight-region)
            (define-key map (kbd "C-c h d") #'org-zettel-ref-highlight-remove)
            (define-key map (kbd "C-c h c") #'org-zettel-ref-highlight-change-type)
            (define-key map (kbd "C-c h r") #'org-zettel-ref-highlight-refresh)
            map)
  (if org-zettel-ref-highlight-mode
      (progn
        (org-zettel-ref-highlight-refresh)
        (add-hook 'after-save-hook #'org-zettel-ref-sync-highlights nil t))
    (progn
      (remove-overlays nil nil 'org-zettel-ref-highlight t)
      (remove-hook 'after-save-hook #'org-zettel-ref-sync-highlights t))))

(provide 'org-zettel-ref-highlight)