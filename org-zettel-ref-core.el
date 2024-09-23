;;; org-zettel-ref-core.el --- Core functionality for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains core functionality for org-zettel-ref.

;;; Code:

(require 'org)
(require 'org-element)

(defgroup org-zettel-ref nil
  "Customization group for org-zettel-ref."
  :group 'org)

(defcustom org-zettel-ref-overview-directory "~/org-zettel-ref-overviews/"
  "Directory to store overview files."
  :type 'directory
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-mode-type 'normal
  "The type of mode to use for org-zettel-ref.
Can be 'normal, 'denote, or 'org-roam."
  :type '(choice (const :tag "Normal" normal)
                 (const :tag "Denote" denote)
                 (const :tag "Org-roam" org-roam))
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-overview-file-suffix "__overview.org"
  "Suffix to be added to overview files created by org-zettel-ref in Denote mode.
This suffix will be appended to the filename before the file extension."
  :type 'string
  :group 'org-zettel-ref)

(defvar org-zettel-ref-overview-file nil
  "The current overview file being used.")

(defvar org-zettel-ref-current-overview-buffer nil
  "The current overview buffer being used.")

(defvar org-zettel-ref-overview-index (make-hash-table :test 'equal)
  "Hash table storing the mapping of source files to overview files.")

(defun org-zettel-ref-load-index ()
  "Load the overview index from a file."
  (let ((index-file (expand-file-name ".overview-index.el" org-zettel-ref-overview-directory)))
    (if (file-exists-p index-file)
        (with-temp-buffer
          (insert-file-contents index-file)
          (setq org-zettel-ref-overview-index (read (current-buffer))))
      (setq org-zettel-ref-overview-index (make-hash-table :test 'equal)))))

(defun org-zettel-ref-save-index ()
  "Save the overview index to a file."
  (let ((index-file (expand-file-name ".overview-index.el" org-zettel-ref-overview-directory)))
    (with-temp-file index-file
      (prin1 org-zettel-ref-overview-index (current-buffer)))))

(defun org-zettel-ref-update-index (source-file overview-file)
  "Update the index with a new or existing SOURCE-FILE to OVERVIEW-FILE mapping."
  (puthash source-file overview-file org-zettel-ref-overview-index)
  (org-zettel-ref-save-index)
  (message "Debug: Updated index with %s -> %s" source-file overview-file))

(defun org-zettel-ref-get-overview-from-index (source-file)
  "Get the overview file for SOURCE-FILE from the index."
  (gethash source-file org-zettel-ref-overview-index))

(defun org-zettel-ref-get-overview-file (source-buffer)
  "Get or create an overview file for SOURCE-BUFFER."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (file-name-base source-file))
         (existing-overview (org-zettel-ref-get-overview-from-index source-file)))
    (if existing-overview
        (progn
          (message "Debug: Found existing overview file: %s" existing-overview)
          existing-overview)
      (let* ((sanitized-title (org-zettel-ref-sanitize-filename title))
             (file-name (org-zettel-ref-generate-filename sanitized-title))
             (file-path (expand-file-name file-name org-zettel-ref-overview-directory))
             (new-overview
              (pcase org-zettel-ref-mode-type
                ('normal (org-zettel-ref-get-normal-overview file-path (org-zettel-ref-generate-file-content source-buffer title) source-file))
                ('denote (org-zettel-ref-get-overview-file-denote source-buffer))
                ('org-roam (org-zettel-ref-get-org-roam-overview file-path source-buffer title))
                (_ (error "Unsupported org-zettel-ref-mode-type: %s" org-zettel-ref-mode-type)))))
        (org-zettel-ref-update-index source-file new-overview)
        (message "Debug: Created new overview file: %s" new-overview)
        new-overview))))

(defun org-zettel-ref-get-normal-overview (file-path content source-file)
  "Create a normal overview file at FILE-PATH with CONTENT and SOURCE-FILE property."
  (unless (file-exists-p file-path)
    (with-temp-file file-path
      (insert (format "#+SOURCE_FILE: %s\n" source-file))
      (insert (cdr content))))
  file-path)

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

(defun org-zettel-ref-get-overview-buffer-name (source-buffer)
  "Get the overview buffer name for the given SOURCE-BUFFER."
  (let* ((source-file-name (buffer-file-name source-buffer))
         (title (if source-file-name (file-name-base source-file-name) "Untitled")))
    (format "*Org Zettel Ref: %s__overview*" title)))

(defun org-zettel-ref-setup-buffers (source-buffer overview-buffer)
  "Set up the source and overview buffers with appropriate modes and configurations."
  (with-current-buffer source-buffer
    (org-mode))
  (with-current-buffer overview-buffer
    (org-mode)
    (setq buffer-read-only t))
    (other-window 1)
 )

(defun org-zettel-ref-sync-overview ()
  "Synchronize the quick notes and marked text from the source buffer to the overview file."
  (interactive)
  (condition-case err
      (let* ((source-buffer (or (buffer-base-buffer) (current-buffer)))
             (source-file (buffer-file-name source-buffer))
             (overview-file (org-zettel-ref-get-overview-file source-buffer))
             (overview-buffer (or (get-buffer org-zettel-ref-current-overview-buffer)
                                 (find-file-noselect overview-file))))
        (message "Debug: Starting sync for overview file: %s" overview-file)
        (unless (buffer-live-p source-buffer)
          (error "Source buffer is not live"))
        (unless (file-writable-p overview-file)
          (error "Overview file is not writable: %s" overview-file))
        (org-zettel-ref-load-index)
        (with-current-buffer overview-buffer
          (let ((inhibit-read-only t)
                (source-file-prop (org-zettel-ref-get-source-file-property)))
            (erase-buffer)
            (insert (format "#+TITLE: %s\n" (file-name-base source-file)))
            (insert (format "#+SOURCE_FILE: %s\n" source-file))
            (insert "#+filetags: :overview:\n#+STARTUP: showall\n\n")
            (insert "* Quick Notes\n\n")
            (condition-case inner-err
                (org-zettel-ref-insert-quick-notes source-buffer overview-buffer)
              (error
               (message "Error in org-zettel-ref-insert-quick-notes: %s" (error-message-string inner-err))))
            (insert "\n* Marked Text\n\n")
            (condition-case inner-err
                (org-zettel-ref-insert-marked-text source-buffer overview-buffer)
              (error
               (message "Error in org-zettel-ref-insert-marked-text: %s" (error-message-string inner-err))))
            (save-buffer)))
        (message "Debug: Sync completed successfully"))
    (error
     (message "Error during synchronization: %s" (error-message-string err))
     (message "Error type: %s" (car err))
     (message "Backtrace: %s" (with-output-to-string (backtrace))))))

(defun org-zettel-ref-insert-quick-notes (source-buffer overview-buffer)
  "Insert quick notes from SOURCE-BUFFER into OVERVIEW-BUFFER."
  (with-current-buffer source-buffer
    (org-element-map (org-element-parse-buffer) 'target
      (lambda (target)
        (let* ((begin (org-element-property :begin target))
               (end (org-element-property :end target))
               (name (org-element-property :value target))
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

(defun org-zettel-ref-init ()
  "Initialize the org-zettel-ref-mode, create or open the overview file and set up the layout."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (overview-buffer-name (org-zettel-ref-get-overview-buffer-name source-buffer))
         (existing-overview-buffer (get-buffer overview-buffer-name)))
    (if existing-overview-buffer
        (progn
          (message "Debug: Existing overview buffer found")
          (display-buffer existing-overview-buffer
                          '(display-buffer-use-some-window (inhibit-same-window . t))))
      (let ((overview-file (condition-case err
                               (org-zettel-ref-get-overview-file source-buffer)
                             (error
                              (message "Error getting overview file: %S" err)
                              nil))))
        (when overview-file
          (message "Debug: Creating new overview buffer")
          (split-window-right)
          (other-window 1)
          (find-file overview-file)
          (rename-buffer overview-buffer-name t))))))

(provide 'org-zettel-ref-core)

;;; org-zettel-ref-core.el ends here
