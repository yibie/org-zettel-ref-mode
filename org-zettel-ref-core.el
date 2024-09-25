;;; org-zettel-ref-core.el --- Core functionality for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains core functionality for org-zettel-ref.

;;; Code:

(require 'org)
(require 'org-element)

;;-------------------------
;; START: Customization
;;-------------------------

(defgroup org-zettel-ref nil
  "Customization group for org-zettel-ref."
  :group 'org)

(defcustom org-zettel-ref-overview-directory "~/org-zettel-ref-overviews/"
  "Directory to store overview files."
  :type 'directory
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-mode-type 'normal
  "The type of mode to use for org-zettel-ref. Can be `normal`, `denote`, or `org-roam`."
  :type '(choice (const :tag "Normal" normal)
                 (const :tag "Denote" denote)
                 (const :tag "Org-roam" org-roam))
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-overview-file-suffix "__overview.org"
  "Suffix to be added to overview files created by org-zettel-ref in Denote mode.
This suffix will be appended to the filename before the file extension."
  :type 'string
  :group 'org-zettel-ref)

;;-------------------------
;; END: Customization
;;-------------------------

;;-------------------------
;; START: Variables
;;-------------------------

(defvar org-zettel-ref-overview-file nil
  "The current overview file being used.")

(defvar org-zettel-ref-overview-index (make-hash-table :test 'equal)
  "Hash table storing the mapping of source files to overview files.")

(defvar org-zettel-ref-current-overview-buffer nil
  "The current overview buffer being used.")

;;-------------------------
;; END: Variables
;;-------------------------

;;-------------------------
;; START: Index Management
;;-------------------------

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

;;------------------------
;; END: Index Management
;;-------------------------

;;-------------------------
;; START: Overview File Management
;;-------------------------
(defun org-zettel-ref-get-overview-file (source-buffer)
  "Get or create an overview file for SOURCE-BUFFER."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (file-name-base source-file))
         (existing-overview (org-zettel-ref-get-overview-from-index source-file)))
    (if existing-overview
        (progn
          (message "Debug: Found existing overview file: %s" existing-overview)
          (setq org-zettel-ref-current-overview-buffer (find-file-noselect existing-overview))
          existing-overview)
      (let* ((sanitized-title (org-zettel-ref-sanitize-filename title))
             (file-name (org-zettel-ref-generate-filename sanitized-title))
             (new-overview
              (pcase org-zettel-ref-mode-type
                ('normal (org-zettel-ref-get-normal-overview nil source-buffer source-file))
                ('denote (org-zettel-ref-get-overview-file-denote source-buffer))
                ('org-roam (org-zettel-ref-get-overview-file-org-roam source-buffer))
                (_ (error "Unsupported org-zettel-ref-mode-type: %s" org-zettel-ref-mode-type)))))
        (org-zettel-ref-update-index source-file new-overview)
        (message "Debug: Created new overview file: %s" new-overview)
        (setq org-zettel-ref-current-overview-buffer (find-file-noselect new-overview))
        new-overview))))

(defun org-zettel-ref-get-normal-overview (file-path content source-file)
  "Create a normal overview file at FILE-PATH with CONTENT and SOURCE-FILE property."
  (unless (file-exists-p file-path)
    (with-temp-file file-path
      (insert (format "#+SOURCE_FILE: %s\n" source-file))
      (insert (cdr content))))
  file-path)

;;-------------------------
;; START: Buffer Management
;;-------------------------
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
    (other-window 1))

;;-------------------------
;; END: Buffer Management
;;-------------------------

;;-------------------------
;; START: Synchronization
;;-------------------------

 (defun org-zettel-ref-sync-overview ()
  "Synchronize quick notes and marked text from the current buffer to the overview file."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (source-file (buffer-file-name))
         overview-file
         overview-buffer)
    (if (not source-file)
        (message "Error: Current buffer is not associated with a file")
      (setq overview-file (org-zettel-ref-get-overview-file source-buffer))
      (if (not (file-writable-p overview-file))
          (message "Error: Overview file is not writable: %s" overview-file)
        (setq overview-buffer (find-file-noselect overview-file))
        (message "Debug: Starting to sync overview file: %s" overview-file)
        (with-current-buffer overview-buffer
          (let ((inhibit-read-only t))
            (org-zettel-ref-sync-section source-buffer overview-buffer)
            (save-buffer)))
        (message "Debug: Sync completed successfully")))))

(defun org-zettel-ref-sync-section (source-buffer overview-buffer)
  "Synchronize quick notes and marked text from SOURCE-BUFFER to OVERVIEW-BUFFER."
  (with-current-buffer overview-buffer
    (goto-char (point-min))
    (let ((quick-notes-start (when (re-search-forward "^\\* Quick Notes\n" nil t) (point)))
          (marked-text-start (when (re-search-forward "^\\* Marked Text\n" nil t) (point))))
      
      (when (and quick-notes-start marked-text-start)
        ;; 清理 Quick Notes 部分
        (goto-char quick-notes-start)
        (let ((end (save-excursion
                     (if (re-search-forward "^\\* " nil t)
                         (line-beginning-position)
                       (point-max)))))
          (when (< quick-notes-start end)
            (delete-region quick-notes-start end)
            (goto-char quick-notes-start)
            (insert "\n")))
        
        ;; 清理 Marked Text 部分
        (goto-char marked-text-start)
        (let ((end (save-excursion
                     (if (re-search-forward "^\\* " nil t)
                         (line-beginning-position)
                       (point-max)))))
          (when (< marked-text-start end)
            (delete-region marked-text-start end)
            (goto-char marked-text-start)
            (insert "\n")))
        
        ;; 插入新的内容
        (save-excursion
          (goto-char quick-notes-start)
          (org-zettel-ref-insert-quick-notes source-buffer overview-buffer))
        (save-excursion
          (goto-char marked-text-start)
          (org-zettel-ref-insert-marked-text source-buffer overview-buffer)))
      
      (unless (and quick-notes-start marked-text-start)
        (message "Error: Quick Notes or Marked Text section not found in overview file")))))

;;-------------------------
;; END: Synchronization
;;-------------------------

;;-------------------------
;; START: Mode Configuration
;;-------------------------

(defcustom org-zettel-ref-quick-markup-key "C-c m"
  "Key binding for quick markup function in org-zettel-ref-mode.
This should be a string that can be passed to `kbd'."
  :type 'string
  :group 'org-zettel-ref)

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

;;-------------------------
;; END: Mode Configuration
;;-------------------------

;;-------------------------
;; START: Initialization
;;-------------------------

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
          (rename-buffer overview-buffer-name t))))
    ;; Ensure the cursor returns to the source file
    (select-window (get-buffer-window source-buffer))
    (message "Debug: org-zettel-ref-init completed"))) 

;;-------------------------
;; END: Initialization
;;-------------------------

;;-------------------------
;; START: Mode-specific Functions
;;-------------------------

(defun org-zettel-ref-get-overview-file-org-roam (source-buffer)
  "Get or create an overview file for SOURCE-BUFFER using org-roam mode."
  (let* ((source-file (buffer-file-name source-buffer))
         (base-name (file-name-base source-file))
         (title (format "Overview - %s" base-name))
         (sanitized-name (replace-regexp-in-string "[^a-zA-Z0-9\u4e00-\u9fa5]" "-" base-name))
         (file-name (concat sanitized-name "__overview.org"))
         (file-path (expand-file-name file-name org-zettel-ref-overview-directory))
         (org-id (org-id-new)))
    (unless source-file
      (error "Source buffer is not associated with a file"))
    (unless (file-exists-p file-path)
      (with-temp-file file-path
        (insert (format ":PROPERTIES:\n:ID:       %s\n:END:\n#+title: %s\n#+filetags: :overview:\n\n* Quick Notes\n\n* Marked Text\n" org-id title))))
    (message "Debug: Org-roam file-path is %s" file-path)
    (when (and (featurep 'org-roam)
               (fboundp 'org-roam-db-update-file))
      (org-roam-db-update-file file-path))
    file-path))

(defun org-zettel-ref-denote-title (title)
  "Generate a title for Denote by directly using the source file name."
  (org-zettel-ref-sanitize-filename title))

(defun org-zettel-ref-denote-slug-title (title)
  "Generate a slug from TITLE for Denote."
  (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" (downcase title)))

(defun org-zettel-ref-find-denote-overview-file (slug-title)  
  "Find an existing Denote overview file matching SLUG-TITLE."
  (let ((files (directory-files org-zettel-ref-overview-directory t "__overview\\.org$")))
    (cl-find-if
     (lambda (file)
       (string-match-p (regexp-quote slug-title) (file-name-nondirectory file)))
     files)))

(defun org-zettel-ref-get-overview-file-denote (source-buffer)
  "Get or create an overview file for SOURCE-BUFFER using Denote mode."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (file-name-base source-file))
         (slug-title (org-zettel-ref-denote-slug-title title))
         (file-path (org-zettel-ref-find-denote-overview-file slug-title)))
    (unless source-file
      (error "Source buffer is not associated with a file"))
    (unless file-path
      (setq file-path (expand-file-name (org-zettel-ref-generate-filename title) org-zettel-ref-overview-directory))
      (with-temp-file file-path
        (insert (format "#+title: Overview - %s\n#+filetags: :overview:\n\n* Quick Notes\n\n* Marked Text\n" title))))
    file-path))

;;-------------------------
;; END: Mode-specific Functions
;;-------------------------

(provide 'org-zettel-ref-core)

;;; org-zettel-ref-core.el ends here
