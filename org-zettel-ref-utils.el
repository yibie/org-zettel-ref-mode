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

(defun org-zettel-ref-sanitize-filename (filename)
  "Sanitize FILENAME by replacing invalid characters with underscores.
Preserves alphanumeric characters, Chinese characters, and some common punctuation."
  (let ((invalid-chars-regexp "[[:cntrl:]\\/:*?\"<>|]"))
    (replace-regexp-in-string invalid-chars-regexp "_" filename)))

(defun org-zettel-ref-generate-filename (title)
  "Generate a filename based on TITLE and current mode type."
  (let* ((sanitized-title (replace-regexp-in-string "[^a-zA-Z0-9\u4e00-\u9fff]+" "-" title))
         (truncated-title (if (> (length sanitized-title) 50)
                              (concat (substring sanitized-title 0 47) "...")
                            sanitized-title))
         (filename (pcase org-zettel-ref-mode-type
                     ('normal (concat truncated-title "-overview.org"))
                     ('denote (let ((date-time (format-time-string "%Y%m%dT%H%M%S")))
                                (format "%s--%s__overview.org" date-time truncated-title)))
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
(defun org-zettel-ref-insert-quick-notes (source-buffer overview-buffer)
  "Insert quick notes from SOURCE-BUFFER into OVERVIEW-BUFFER."
  (with-current-buffer source-buffer
    (org-element-map (org-element-parse-buffer) 'target
      (lambda (target)
        (let* ((begin (org-element-property :begin target))
               (end (org-element-property :end target))
               (content (buffer-substring-no-properties begin end)))
          (when (string-match "<<\\([^>]+\\)>>\\(.*\\)" content)
            (let ((note-name (match-string 1 content))
                  (note-content (string-trim (match-string 2 content))))
              (with-current-buffer overview-buffer
                (let ((inhibit-read-only t))
                  (insert (format "- [[file:%s::%s][%s]]\n"
                                  (buffer-file-name source-buffer)
                                  note-name
                                  (if (string-empty-p note-content)
                                      note-name
                                    note-content))))))))))))

(defun org-zettel-ref-insert-marked-text (source-buffer overview-buffer)
  "Insert marked text from SOURCE-BUFFER into OVERVIEW-BUFFER."
  (with-current-buffer source-buffer
    (org-element-map (org-element-parse-buffer) '(bold underline verbatim code)
      (lambda (element)
        (let* ((begin (org-element-property :begin element))
               (end (org-element-property :end element))
               (raw-text (string-trim (buffer-substring-no-properties begin end))))
          (when (and raw-text (not (string-empty-p raw-text)))
            (with-current-buffer overview-buffer
              (let ((inhibit-read-only t))
                (insert (format "- %s\n" raw-text))))))))))

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

(provide 'org-zettel-ref-utils)

;;; org-zettel-ref-utils.el ends here