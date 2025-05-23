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

(defun org-zettel-ref-ai--find-source-heading-in-single-overview (buffer source-ref-id)
  "In BUFFER (single overview), find the top-level heading for SOURCE-REF-ID.
Return point at the beginning of the heading, or nil if not found.
Also handles cleanup of any duplicate headings."
  (with-current-buffer buffer
    (save-excursion
      ;; 使用与同步函数相同的清理逻辑
      (let ((cleaned-heading-pos (org-zettel-ref--cleanup-duplicate-headings source-ref-id)))
        (when cleaned-heading-pos
          (message "DEBUG: AI found heading after cleanup, REF_ID: %s, position: %s" source-ref-id cleaned-heading-pos))
        cleaned-heading-pos))))

;; Core functions
(defun org-zettel-ref-ai--clean-script-tags (content)
  "Remove content between <script> and </script> tags from CONTENT."
  (replace-regexp-in-string "<script[^>]*>.*?</script>" "" content t t))

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
    (let* ((raw-content (buffer-substring-no-properties
                       (point-min)
                       (min (point-max) org-zettel-ref-ai-max-content-length)))
           (content (org-zettel-ref-ai--clean-script-tags raw-content))
           (prompt (format org-zettel-ref-ai-prompt content)))
      (message "DEBUG: Preparing prompt with template: %s" org-zettel-ref-ai-prompt)
      prompt)))

(defun org-zettel-ref-ai--has-summary-p (buffer &optional source-ref-id)
  "Check if BUFFER contains a summary section.
If in single-file mode and SOURCE-REF-ID is provided, check under that source's heading."
  (with-current-buffer buffer
    (condition-case err
        (save-excursion
          (if (and org-zettel-ref-use-single-overview-file source-ref-id)
              (when-let ((source-heading-point (org-zettel-ref-ai--find-source-heading-in-single-overview buffer source-ref-id)))
                (goto-char source-heading-point)
                (condition-case narrow-err
                    (org-narrow-to-subtree)
                  (error
                   (message "DEBUG: AI has-summary org-narrow-to-subtree failed: %s" (error-message-string narrow-err))
                   ;; Continue without narrowing
                   nil))
                (goto-char (point-min))
                (let ((found (re-search-forward "^\\*\\* Summary" nil t)))
                  (condition-case widen-err
                      (widen)
                    (error
                     (message "DEBUG: AI has-summary widen failed: %s" (error-message-string widen-err))))
                  found))
            ;; Multi-file mode or no source-ref-id for single (should not happen for has-summary-p)
            (goto-char (point-min))
            (re-search-forward "^\\* Summary" nil t)))
      (error
       (message "DEBUG: AI has-summary-p error: %s" (error-message-string err))
       nil))))

(defun org-zettel-ref-ai--remove-summary (buffer &optional source-ref-id)
  "Remove summary from BUFFER.
If in single-file mode and SOURCE-REF-ID is provided, remove `** Summary` under that source's heading."
  (with-current-buffer buffer
    (condition-case err
        (save-excursion
          (if (and org-zettel-ref-use-single-overview-file source-ref-id)
              (when-let ((source-heading-point (org-zettel-ref-ai--find-source-heading-in-single-overview buffer source-ref-id)))
                (goto-char source-heading-point)
                (condition-case narrow-err
                    (org-narrow-to-subtree)
                  (error
                   (message "DEBUG: AI remove-summary org-narrow-to-subtree failed: %s" (error-message-string narrow-err))))
                (goto-char (point-min))
                (when (re-search-forward "^\\*\\* Summary" nil t)
                  (let ((start (match-beginning 0)))
                    (goto-char start)
                    ;; Determine end of summary section (next sibling or end of narrowed region)
                    (if (re-search-forward "^\\*\\*\\* " (condition-case end-err
                                                             (save-excursion (org-end-of-subtree))
                                                           (error
                                                            (message "DEBUG: AI remove-summary org-end-of-subtree failed: %s" (error-message-string end-err))
                                                            (point-max))) t)
                        (delete-region start (match-beginning 0))
                      (delete-region start (point-max)))))
                (condition-case widen-err
                    (widen)
                  (error
                   (message "DEBUG: AI remove-summary widen failed: %s" (error-message-string widen-err)))))
            ;; Multi-file mode
            (goto-char (point-min))
            (when (re-search-forward "^\\* Summary" nil t)
              (let ((start (match-beginning 0)))
                (goto-char start)
                (if (re-search-forward "^\\* " nil t)
                    (delete-region start (match-beginning 0))
                  (delete-region start (point-max)))))))
      (error
       (message "DEBUG: AI remove-summary error: %s" (error-message-string err))))))

(defun org-zettel-ref-ai--find-insert-position (buffer &optional source-ref-id)
  "Find insertion position in BUFFER.
If in single-file mode and SOURCE-REF-ID is provided, find position under that source's heading section, after any existing summary.
Otherwise, find position after file-level properties for multi-file mode."
  (with-current-buffer buffer
    (condition-case err
        (save-excursion
          (if (and org-zettel-ref-use-single-overview-file source-ref-id)
              ;; --- Single-File Mode ---
              (if-let ((source-heading-marker (org-zettel-ref-ai--find-source-heading-in-single-overview buffer source-ref-id)))
                  (progn
                    (goto-char source-heading-marker)
                    (condition-case heading-err
                        (org-back-to-heading t) ; Ensure we are at the start of the heading
                      (error
                       (message "DEBUG: AI find-insert org-back-to-heading failed: %s" (error-message-string heading-err))
                       ;; Continue from current position
                       nil))
                    (condition-case narrow-err
                        (org-narrow-to-subtree)
                      (error
                       (message "DEBUG: AI find-insert org-narrow-to-subtree failed: %s" (error-message-string narrow-err))))
                    (let ((insert-pos-marker nil))
                      (goto-char (point-min)) ; Start search from beginning of narrowed section
                                        (if (re-search-forward "^\\*\\* Summary" (point-max) t)
                      ;; Summary exists, insert after its heading line
                      (progn
                        (goto-char (match-end 0)) ; Go to end of the "** Summary" line
                            (setq insert-pos-marker (point-marker)))
                        ;; Summary does not exist, insert at the end of the source's section
                        (progn
                          (goto-char (point-max))
                          (setq insert-pos-marker (point-marker))))
                      (condition-case widen-err
                          (widen)
                        (error
                         (message "DEBUG: AI find-insert widen failed: %s" (error-message-string widen-err))))
                      insert-pos-marker))
                ;; Fallback if source heading somehow not found (should be handled by called fn)
                (progn
                  (message "Warning: Source heading for REF_ID %s not found by org-zettel-ref-ai--find-source-heading-in-single-overview. Inserting summary at end of file." source-ref-id)
                  (goto-char (point-max))
                  (point-marker)))
            ;; --- Multi-File Mode (existing logic) ---
            (progn
              (goto-char (point-min))
              (while (and (not (eobp)) (looking-at "^#+"))
                (forward-line 1))
              (when (looking-at "^$") ; Skip one blank line after properties
                (forward-line 1))
              (point-marker))))
      (error
       (message "DEBUG: AI find-insert-position error: %s" (error-message-string err))
       ;; Fallback: return end of buffer marker
       (point-max-marker)))))

(defun org-zettel-ref-ai--generate (prompt target-buffer pos callback)
  "Generate summary using gptel.
PROMPT is prompt text, TARGET-BUFFER is target buffer, POS is insertion position.
CALLBACK is completion callback function."
  (require 'gptel)
  
  (unless (buffer-live-p target-buffer)
    (error "Target buffer is not live"))
  
  (unless (bound-and-true-p gptel-model)
    (setq-local gptel-model 'gpt-3.5-turbo))
  
  (let ((marker (with-current-buffer target-buffer 
                  (save-excursion
                    (goto-char pos)
                    (point-marker)))))
    
    (condition-case err
        (progn
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
                          org-zettel-ref-current-overview-buffer))
         (source-ref-id (when (and org-zettel-ref-use-single-overview-file
                                   (boundp 'org-zettel-ref-current-ref-entry)
                                   org-zettel-ref-current-ref-entry)
                          (org-zettel-ref-ref-entry-id org-zettel-ref-current-ref-entry))))
    
    (unless overview-buffer
      (user-error "Overview buffer not found, please run org-zettel-ref-init"))
    
    (when (org-zettel-ref-ai--get-summary-status overview-buffer)
      (user-error "Summary generation in progress for this overview"))
    
    (org-zettel-ref-ai--check-backend)
    
    (if (and org-zettel-ref-use-single-overview-file (not source-ref-id))
        (user-error "Single-file mode: Could not determine source reference ID for summary placement.")
      ;; Check if summary already exists and does not need to be forced to update
      (if (and (not force)
               (buffer-live-p overview-buffer)
               (org-zettel-ref-ai--has-summary-p overview-buffer source-ref-id)) ; Pass source-ref-id
          (message "Overview already has summary for this source, use C-u prefix argument to force update")
        
        ;; Start summary generation process
        (org-zettel-ref-ai--set-summary-status overview-buffer t)
        (message "Generating summary...")
        
        (let ((prompt (org-zettel-ref-ai--prepare-prompt source-buffer)))
          (with-current-buffer overview-buffer
            (when force
              (org-zettel-ref-ai--remove-summary overview-buffer source-ref-id)) ; Pass source-ref-id
            
            (let* ((summary-heading-level (if org-zettel-ref-use-single-overview-file 2 1))
                   (summary-title (concat (make-string summary-heading-level ?*) " Summary\n\n"))
                   (insert-pos (org-zettel-ref-ai--find-insert-position overview-buffer source-ref-id))) ; Pass source-ref-id
              (save-excursion
                (goto-char insert-pos)
                ;; Ensure a newline before inserting the summary heading, especially in single-file mode at end of section
                (unless (or (bobp) (looking-back "\n" (point)))
                  (insert "\n"))
                (insert summary-title)

                (condition-case err
                    (org-zettel-ref-ai--generate
                     prompt overview-buffer (point)
                     (lambda (&rest _)
                       (with-current-buffer overview-buffer
                         (save-excursion
                           (goto-char (point-max)) ; Go to very end of buffer to ensure trailing newlines
                           (unless (looking-back "\n\n" (min (point) 2))
                             (insert "\n\n"))))
                       (setq org-zettel-ref-ai-summary-in-progress nil)
                       (message "Summary generation completed")))
                  (error
                   (message "Error in generate-summary: %S" err)
                   (org-zettel-ref-ai--set-summary-status overview-buffer nil)))))))))))

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
                  (let ((source-buffer (current-buffer))
                        (overview-buffer org-zettel-ref-current-overview-buffer) ; Assumes this is set by core
                        (source-ref-id (when (and org-zettel-ref-use-single-overview-file
                                                    (boundp 'org-zettel-ref-current-ref-entry)
                                                    org-zettel-ref-current-ref-entry)
                                         (org-zettel-ref-ref-entry-id org-zettel-ref-current-ref-entry))))
                    (message "DEBUG: AI Init hook in %s. Overview: %s. Single-mode: %s. Source-Ref-ID: %s"
                             (buffer-name source-buffer)
                             (when overview-buffer (buffer-name overview-buffer))
                             org-zettel-ref-use-single-overview-file
                             source-ref-id)
                    (when (and (buffer-file-name source-buffer) ; Ensure source is a file
                               overview-buffer
                               (buffer-live-p overview-buffer))
                      (message "DEBUG: AI Hook - Overview buffer check passed: %s" (buffer-name overview-buffer))
                      ;; Check if summary already exists
                      (let ((has-summary (org-zettel-ref-ai--has-summary-p overview-buffer source-ref-id))) ; Pass source-ref-id
                        (message "DEBUG: AI Hook - Has summary check for %s: %s" (or source-ref-id "multi-file") has-summary)
                        (unless has-summary
                          (with-current-buffer source-buffer ; Ensure we are in source buffer for size check
                            (let ((size (buffer-size)))
                              (message "DEBUG: AI Hook - Source buffer size: %d (max: %d)"
                                       size org-zettel-ref-ai-max-content-length)
                              (when (< size org-zettel-ref-ai-max-content-length)
                                (org-zettel-ref-ai-generate-summary)))))))))))))

(provide 'org-zettel-ref-ai)
