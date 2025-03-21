;; org-zettel-ref-ai.el -*- lexical-binding: t; -*-
;;; Minimal version - Efficient and direct

(require 'cl-lib)
(require 'org-zettel-ref-core)

;; Basic configuration
(defgroup org-zettel-ref-ai nil
  "AI summary generation functionality."
  :group 'org-zettel-ref)

(define-obsolete-variable-alias 'org-zettel-ref-ai-enable
  'org-zettel-ref-enable-ai-summary
  "1.0"
  "Use `org-zettel-ref-enable-ai-summary' from core module instead.")

(defcustom org-zettel-ref-ai-backend 'gptel
  "AI backend for summary generation."
  :type '(const :tag "GPTel" gptel)
  :group 'org-zettel-ref-ai)

(defcustom org-zettel-ref-ai-max-content-length 32000
  "Maximum length for summary content."
  :type 'integer
  :group 'org-zettel-ref-ai)

(defcustom org-zettel-ref-ai-stream t
  "Enable streaming responses."
  :type 'boolean
  :group 'org-zettel-ref-ai)

(defcustom org-zettel-ref-ai-prompt
  "Generate a concise org-mode format summary of the following content, using headings and bullet points to highlight key information:\n\n%s"
  "System prompt for GPT model."
  :type 'string
  :group 'org-zettel-ref-ai)

(defvar-local org-zettel-ref-ai-summary-in-progress nil
  "Summary generation status flag. Buffer-local variable.")

;; Core functions
(defun org-zettel-ref-ai--check-backend ()
  "Verify gptel backend configuration."
  (unless (featurep 'gptel)
    (user-error "Please install gptel first"))
  (unless (bound-and-true-p gptel-api-key)
    (user-error "Please configure gptel API key")))

(defun org-zettel-ref-ai--prepare-prompt (&optional buffer)
  "Prepare summary prompt text.
If BUFFER is provided, use content from that buffer; otherwise use content from the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((content (buffer-substring-no-properties
                     (point-min)
                     (min (point-max) org-zettel-ref-ai-max-content-length)))
           (prompt (format org-zettel-ref-ai-prompt content)))
      (message "DEBUG: Preparing prompt with template: %s" org-zettel-ref-ai-prompt)
      prompt)))

(defun org-zettel-ref-ai--has-summary-p (buffer)
  "Check if BUFFER contains a summary section."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^\\* Summary" nil t))))

(defun org-zettel-ref-ai--remove-summary (buffer)
  "Remove summary from BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\* Summary" nil t)
        (let ((start (match-beginning 0)))
          (goto-char start)
          (if (re-search-forward "^\\* " nil t)
              (delete-region start (match-beginning 0))
            (delete-region start (point-max))))))))

(defun org-zettel-ref-ai--find-insert-position (buffer)
  "Find insertion position in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp)) (looking-at "^#+"))
        (forward-line 1))
      (when (looking-at "^$")
        (forward-line 1))
      (point))))

(defun org-zettel-ref-ai--generate (prompt target-buffer pos callback)
  "Generate summary using gptel.
PROMPT is prompt text, TARGET-BUFFER is target buffer, POS is insertion position.
CALLBACK is completion callback function."
  (require 'gptel)
  
  ;; 确保 target-buffer 是活跃的
  (unless (buffer-live-p target-buffer)
    (error "Target buffer is not live"))
  
  ;; 设置必要的 gptel 配置
  (unless (bound-and-true-p gptel-model)
    (setq-local gptel-model 'gpt-3.5-turbo))
  
  (let ((marker (with-current-buffer target-buffer 
                  (save-excursion
                    (goto-char pos)
                    (point-marker)))))
    
    (condition-case err
        (progn
          ;; 确保在正确的 buffer 中
          (with-current-buffer target-buffer
            (gptel-request 
             prompt
             :buffer target-buffer
             :position pos
             :stream org-zettel-ref-ai-stream
             :callback (lambda (&rest args)
                        (condition-case err
                            (let ((response (car args))
                                  (is-last (or (not org-zettel-ref-ai-stream)
                                             (and (> (length args) 1)
                                                  (eq (plist-get (cadr args) :status) 'end)))))
                              (when (and response (stringp response))
                                (with-current-buffer target-buffer
                                  (let ((inhibit-read-only t))
                                    (save-excursion
                                      (goto-char (buffer-size))
                                      (insert response)))))
                              (when is-last
                                (message "Summary generation completed")
                                (setq org-zettel-ref-ai-summary-in-progress nil)
                                (funcall callback)))
                          (error
                           (message "Error in callback: %S" err)
                           (setq org-zettel-ref-ai-summary-in-progress nil)))))))
      (error
       (message "Failed to send request: %S" err)
       (setq org-zettel-ref-ai-summary-in-progress nil)))))

;; 添加状态管理函数
(defun org-zettel-ref-ai--set-summary-status (buffer status)
  "Set summary generation status for BUFFER to STATUS."
  (with-current-buffer buffer
    (setq-local org-zettel-ref-ai-summary-in-progress status)))

(defun org-zettel-ref-ai--get-summary-status (buffer)
  "Get summary generation status for BUFFER."
  (with-current-buffer buffer
    org-zettel-ref-ai-summary-in-progress))

;;;###autoload
(defun org-zettel-ref-ai-generate-summary (&optional force)
  "Generate summary for current buffer and insert into overview buffer.
Use prefix argument FORCE to force regeneration."
  (interactive "P")
  (unless org-zettel-ref-enable-ai-summary
    (user-error "AI summary generation is disabled. Set org-zettel-ref-enable-ai-summary to t to enable"))
  
  (unless (buffer-file-name)
    (user-error "Current buffer not associated with a file"))
  
  (let* ((source-buffer (current-buffer))
         (overview-buffer (when (boundp 'org-zettel-ref-current-overview-buffer) 
                          org-zettel-ref-current-overview-buffer)))
    
    (unless overview-buffer
      (user-error "Overview buffer not found, please run org-zettel-ref-init"))
    
    (when (org-zettel-ref-ai--get-summary-status overview-buffer)
      (user-error "Summary generation in progress for this overview"))
    
    (org-zettel-ref-ai--check-backend)
    
    ;; Check if summary already exists and does not need to be forced to update
    (if (and (not force) 
             (buffer-live-p overview-buffer)
             (org-zettel-ref-ai--has-summary-p overview-buffer))
        ;; If summary already exists and does not need to be forced to regenerate, do nothing
        (message "Overview already has summary, use C-u prefix argument to force update")
      
      ;; Start summary generation process
      (org-zettel-ref-ai--set-summary-status overview-buffer t)
      (message "Generating summary...")
      
      ;; Get content from source buffer
      (let ((prompt (org-zettel-ref-ai--prepare-prompt source-buffer)))
        (with-current-buffer overview-buffer
          ;; If forced to update, first remove old summary
          (when force
            (org-zettel-ref-ai--remove-summary overview-buffer))
          
          ;; Find insertion position and insert summary title
          (let ((insert-pos (org-zettel-ref-ai--find-insert-position overview-buffer)))
            (save-excursion
              (goto-char insert-pos)
              (insert "* Summary\n\n")
              
              ;; Generate summary
              (condition-case err
                  (org-zettel-ref-ai--generate 
                   prompt overview-buffer (point)
                   (lambda (&rest _)
                     (with-current-buffer overview-buffer
                       (save-excursion
                         (goto-char (point-max))
                         (unless (looking-back "\n\n" (min (point) 2))
                           (insert "\n\n"))))))
                (error
                 (message "Error in generate-summary: %S" err)
                 (org-zettel-ref-ai--set-summary-status overview-buffer nil))))))))))

(defun org-zettel-ref-ai-reset ()
  "Reset summary generation status."
  (interactive)
  (when-let ((buf (when (boundp 'org-zettel-ref-current-overview-buffer)
                    org-zettel-ref-current-overview-buffer)))
    (when (buffer-live-p buf)
      (org-zettel-ref-ai--set-summary-status buf nil)
      (message "Summary generation status reset. "))))

;; Register auto-summary feature
(with-eval-after-load 'org-zettel-ref-core
  (when (boundp 'org-zettel-ref-init-hook)
    (add-hook 'org-zettel-ref-init-hook
              (lambda ()
                (when org-zettel-ref-enable-ai-summary  ; Check if feature is enabled
                  (let ((source-buffer (current-buffer)))
                    (message "DEBUG: Init hook triggered in buffer %s" (buffer-name))
                    (when (and (buffer-file-name)
                              (boundp 'org-zettel-ref-current-overview-buffer)
                              org-zettel-ref-current-overview-buffer
                              (buffer-live-p org-zettel-ref-current-overview-buffer))  ;; 确保 overview buffer 存在且活跃
                      (message "DEBUG: Overview buffer check passed: %s" 
                              (buffer-name org-zettel-ref-current-overview-buffer))
                      ;; Check if summary already exists
                      (let ((has-summary (org-zettel-ref-ai--has-summary-p org-zettel-ref-current-overview-buffer)))
                        (message "DEBUG: Has summary check: %s" has-summary)
                        (unless has-summary
                          ;; Check file size in source buffer
                          (with-current-buffer source-buffer
                            (let ((size (buffer-size)))
                              (message "DEBUG: Source buffer size: %d (max: %d)" 
                                      size org-zettel-ref-ai-max-content-length)
                              (when (< size org-zettel-ref-ai-max-content-length)
                                (org-zettel-ref-ai-generate-summary)))))))))))))

(provide 'org-zettel-ref-ai)
