;;; org-zettel-ref-utils.el --- Utility functions for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains utility functions for org-zettel-ref.

;;; Code:


;;----------------------------------------------------------------
;; org-zettel-ref-run-python-script
;;---------------------------------------------------------------- 

(defcustom org-zettel-ref-python-file "~/Documents/emacs/package/org-zettel-ref-mode/document_convert_to_org.py"
  "Python script file path."
  :type 'string
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-temp-folder "~/Documents/temp_convert/"
  "Temporary folder path."
  :type 'string
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-reference-folder "~/Documents/ref/"
  "Reference folder path."
  :type 'string
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-archive-folder "/Volumes/Collect/archives/"
  "Archive folder path."
  :type 'string
  :group 'org-zettel-ref)

;; debug message
(defun org-zettel-ref-debug-message (format-string &rest args)
  "Print debug information to *Messages* buffer."
  (apply #'message (concat "ORG-ZETTEL-REF-DEBUG: " format-string) args))

;; run python script
(defun org-zettel-ref-run-python-script ()
  "Run the configured Python script, displaying its output in the *Convert to Org* buffer."
  (interactive)
  (let* ((script-path (expand-file-name org-zettel-ref-python-file))
         (default-directory (file-name-directory script-path))
         (python-path (executable-find "python"))
         (temp-folder (expand-file-name org-zettel-ref-temp-folder))
         (reference-folder (expand-file-name org-zettel-ref-reference-folder))
         (archive-folder (expand-file-name org-zettel-ref-archive-folder)))
    (cond
     ((not python-path)
      (error "Python executable not found in PATH"))
     ((not (file-exists-p script-path))
      (error "Cannot find the specified Python script: %s" script-path))
     ((not (file-directory-p temp-folder))
      (error "Temporary folder does not exist: %s" temp-folder))
     ((not (file-directory-p reference-folder))
      (error "Reference folder does not exist: %s" reference-folder))
     ((not (file-directory-p archive-folder))
      (error "Archive folder does not exist: %s" archive-folder))
     (t
      (let ((command (format "%s %s --temp %s --reference %s --archive %s"
                             (shell-quote-argument python-path)
                             (shell-quote-argument script-path)
                             (shell-quote-argument temp-folder)
                             (shell-quote-argument reference-folder)
                             (shell-quote-argument archive-folder))))
        (org-zettel-ref-debug-message "Executing command: %s" command)
        (async-shell-command command "*Convert to Org*")
        (with-current-buffer "*Convert to Org*"
          (org-zettel-ref-debug-message "Python script output:\n%s" (buffer-string))))))))



;;----------------------------------------------------------------
;; Other components
;;----------------------------------------------------------------

(defun org-zettel-ref-mode-enable ()
  "Enable org-zettel-ref-mode."
  (org-zettel-ref-init)
  (org-zettel-ref-setup-quick-markup)
  (advice-add 'org-open-at-point :around #'org-zettel-ref-advice-open-at-point))

(defun org-zettel-ref-mode-disable ()
  "Disable org-zettel-ref-mode."
  (advice-remove 'org-open-at-point #'org-zettel-ref-advice-open-at-point)
  (local-unset-key (kbd org-zettel-ref-quick-markup-key)))

(defun org-zettel-ref-advice-open-at-point (orig-fun &rest args)
  "简化的链接处理，利用 org-zettel-ref-mode 的上下文."
  (let* ((context (org-element-context))
         (type (org-element-property :type context))
         (path (org-element-property :path context)))
    (if (and (eq (org-element-type context) 'link)
             (string= type "file")
             (string-match ".*::hl-\\([0-9]+\\)" path))
        ;; 如果是高亮链接，使用已有的关联直接跳转
        (with-current-buffer org-zettel-ref-source-buffer  ; 使用已关联的源文件缓冲区
          (widen)
          (goto-char (point-min))
          (let ((target-id (match-string 1 path)))
            (if (re-search-forward (concat "<<hl-" target-id ">>") nil t)
                (progn
                  (goto-char (match-beginning 0))
                  (org-reveal)
                  (org-show-entry)
                  (switch-to-buffer-other-window (current-buffer))
                  t)
              (message "Target hl-%s not found" target-id)
              nil)))
      ;; 其他链接使用原始函数处理
      (apply orig-fun args))))

(defun org-zettel-ref-find-target (file target)
  "Find TARGET in FILE and jump to its location.
FILE should be absolute path, TARGET should be the target identifier."
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (widen)
      (goto-char (point-min))
      (if (re-search-forward (concat "<<" (regexp-quote target) ">>") nil t)
          (progn
            (switch-to-buffer buf)
            (goto-char (match-beginning 0))
            (org-reveal)
            (org-show-entry)
            t)
        (message "Target %s not found in %s" target file)
        nil))))

;; 其他实用函数
(defun org-zettel-ref-get-overview-buffer-name (source-buffer)
  "获取SOURCE-BUFFER对应的概览缓冲区名称。"
  (format "*Org Zettel Ref: %s*" 
          (file-name-base (buffer-file-name source-buffer))))

(defun org-zettel-ref-extract-id-from-filename (filename)
  "从文件名中提取ID。"
  (when (string-match "\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)" filename)
    (match-string 1 filename)))

(provide 'org-zettel-ref-utils)

;;; org-zettel-ref-utils.el ends here
