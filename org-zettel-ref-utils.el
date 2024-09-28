;;; org-zettel-ref-utils.el --- Utility functions for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains utility functions for org-zettel-ref.

;;; Code:

;;----------------------------------------------------------------
;; START: File namming
;;----------------------------------------------------------------

(defun org-zettel-ref-slugify (title)
  "Convert TITLE to a slug for generating filenames."
  (let ((slug (downcase (replace-regexp-in-string "[^[:alnum:][:digit:]\u4e00-\u9fff]+" "-" title))))
    (string-trim slug "-+")))

(defun org-zettel-ref-generate-filename (title)
  "Generate a filename based on TITLE and current mode type."
  (let* ((sanitized-title (replace-regexp-in-string "[^a-zA-Z0-9\u4e00-\u9fff]+" "-" title))
         (truncated-title (if (> (length sanitized-title) 50)
                              (concat (substring sanitized-title 0 47) "...")
                            sanitized-title))
         (filename (pcase org-zettel-ref-mode-type
                     ('normal (concat truncated-title "-overview.org"))
                     ('denote (concat (format-time-string "%Y%m%dT%H%M%S--") truncated-title org-zettel-ref-overview-file-suffix))
                     ('org-roam (format "%s-overview.org" truncated-title)))))
    (if (string-empty-p filename)
        (error "Generated filename is empty")
      (message "Debug: Generated filename: %s" filename))
    filename))

(defun org-zettel-ref-ensure-overview-buffer ()
  "Ensure that the overview buffer exists, creating it if necessary."
  (unless (and org-zettel-ref-current-overview-buffer
               (buffer-live-p org-zettel-ref-current-overview-buffer))
    (org-zettel-ref-init)))

;;----------------------------------------------------------------
;; END:  File namming
;;----------------------------------------------------------------

;;----------------------------------------------------------------
;; START: org-zettel-ref-insert-quick-notes and marked-text
;;----------------------------------------------------------------  
(defun org-zettel-ref-insert-notes-and-text (source-buffer overview-buffer)
  "Insert quick notes and marked text from SOURCE-BUFFER into OVERVIEW-BUFFER."
  (with-current-buffer overview-buffer
    (let ((inhibit-read-only t)
          (quick-notes '())
          (existing-texts (org-zettel-ref-get-existing-marked-texts))
          (new-texts '()))
      ;; Collect quick notes
      (with-current-buffer source-buffer
        (org-element-map (org-element-parse-buffer) 'target
          (lambda (target)
            (let* ((begin (org-element-property :begin target))
                   (end (org-element-property :end target))
                   (content (buffer-substring-no-properties begin end)))
              (when (string-match "<<\\([^>]+\\)>>\\(.*\\)" content)
                (let ((note-name (match-string 1 content))
                      (note-content (string-trim (match-string 2 content))))
                  (push (cons note-name note-content) quick-notes)))))))
      
      ;; Collect marked text
      (with-current-buffer source-buffer
        (org-element-map (org-element-parse-buffer) '(bold underline)
          (lambda (element)
            (let* ((begin (org-element-property :begin element))
                   (end (org-element-property :end element))
                   (raw-text (string-trim (buffer-substring-no-properties begin end))))
              (when (and raw-text (not (string-empty-p raw-text))
                         (not (member raw-text existing-texts)))
                (push raw-text new-texts))))))
      
      ;; 插入快速笔记
      (when quick-notes
        ;; 定位到 * Quick Notes 部分
        (goto-char (point-min))
        (re-search-forward "^\\* Quick Notes\n")
        (goto-char (match-end 0))
        (dolist (note (nreverse quick-notes))
          (insert (format "- [[file:%s::%s][%s]]\n"
                          (buffer-file-name source-buffer)
                          (car note)
                          (if (string-empty-p (cdr note))
                              (car note)
                            (cdr note)))))
        (insert "\n"))
      
      ;; 插入标记文本
      (when new-texts
        ;; 定位到 * Marked Text 部分
        (goto-char (point-min))
        (re-search-forward "^\\* Marked Text\n")
        (goto-char (match-end 0))
        (dolist (text (delete-dups (nreverse new-texts)))
          (insert (format "- %s\n" text)))
        (insert "\n")))))

(defun org-zettel-ref-get-existing-marked-texts ()
  "Get existing marked texts from the current buffer."
  (let ((texts '()))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\* Marked Text\n" nil t)
        (while (re-search-forward "^- \\(.+\\)$" nil t)
          (push (match-string 1) texts))))
    texts))

(defun org-zettel-ref-get-existing-marked-texts ()
  "Get existing marked texts from the current buffer."
  (let ((texts '()))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\* Marked Text\n" nil t)
        (while (re-search-forward "^- \\(.+\\)$" nil t)
          (push (match-string 1) texts))))
    texts))

;;----------------------------------------------------------------
;; START: org-zettel-ref-run-python-script
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
         (python-path (executable-find "python3"))
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
;; END: org-zettel-ref-run-python-script
;;----------------------------------------------------------------

;;----------------------------------------------------------------
;; START: Other components
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
  "Advice function for `org-open-at-point'.
This function checks if the link at point is a quick note link,
and if so, it jumps to the corresponding quick note in the source buffer.
Otherwise, it calls the original `org-open-at-point' function."
  (let ((context (org-element-context)))
    (if (and (eq (org-element-type context) 'link)
             (string-equal (org-element-property :type context) "file")
             (string-match "::\\(.+\\)" (org-element-property :path context)))
        (let* ((target (match-string 1 (org-element-property :path context)))
               (source-file (org-element-property :path context))
               (source-buffer (find-file-noselect (substring source-file 0 (string-match "::" source-file)))))
          (switch-to-buffer source-buffer)
          (goto-char (point-min))
          (re-search-forward (concat "<<" (regexp-quote target) ">>") nil t)
          (org-reveal))
      (apply orig-fun args))))

;;----------------------------------------------------------------
;; END: Other components
;;----------------------------------------------------------------  


(provide 'org-zettel-ref-utils)

;;; org-zettel-ref-utils.el ends here