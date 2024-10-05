;;; org-zettel-ref-core.el --- Core functionality for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains core functionality for org-zettel-ref.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'org-zettel-ref-utils)
(require 'org-zettel-ref-db)

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

(defvar org-zettel-ref-current-overview-buffer nil
  "The current overview buffer being used.")

;;-------------------------
;; START: Overview File Management
;;-------------------------

(defun org-zettel-ref-get-overview-file (source-buffer)
  "Get or create the overview file for SOURCE-BUFFER."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (file-name-base source-file))
         (slug-title (org-zettel-ref-denote-slug-title title)) 
         (overview-filename (org-zettel-ref-generate-filename title))
         (overview-file (expand-file-name overview-filename org-zettel-ref-overview-directory))
         (existing-overview (org-zettel-ref-get-overview-from-index source-file)))
    (if existing-overview
        (progn
          (message "Debug: Found existing overview file: %s" existing-overview)
          (setq org-zettel-ref-current-overview-buffer (find-file-noselect existing-overview))
          existing-overview)
      (let* ((overview-filename (org-zettel-ref-generate-filename slug-title))
             (overview-file (expand-file-name overview-filename org-zettel-ref-overview-directory)))
        (unless (file-exists-p overview-file)
          (let ((new-overview
                 (pcase org-zettel-ref-mode-type
                   ('normal (org-zettel-ref-get-normal-overview source-buffer overview-file))
                   ('denote (org-zettel-ref-get-overview-file-denote source-buffer overview-file))
                   ('org-roam (org-zettel-ref-get-overview-file-org-roam source-buffer overview-file))
                   (_ (error "Unsupported org-zettel-ref-mode-type: %s" org-zettel-ref-mode-type)))))
        (org-zettel-ref-update-index source-file new-overview)
            (message "Debug: Created new overview file: %s" new-overview)
            (setq org-zettel-ref-current-overview-buffer (find-file-noselect new-overview))
            new-overview))
        overview-file))))

;;-------------------------
;; START: Buffer Management
;;-------------------------
(defun org-zettel-ref-get-overview-buffer-name (source-buffer)
  "Get the overview buffer name for the given SOURCE-BUFFER."
  (let* ((source-file-name (buffer-file-name source-buffer))
         (title (if source-file-name (file-name-base source-file-name) "Untitled")))
    ;; 缓冲区名称不包含时间戳，确保一致性
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
  "Synchronize quick notes and marked text from current buffer to overview file."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (source-file (buffer-file-name)))
    (if (not source-file)
        (message "Error: Current buffer is not associated with a file")
      (condition-case err
          (let* ((overview-file (org-zettel-ref-get-overview-file source-buffer))
                 (overview-buffer (get-buffer (org-zettel-ref-get-overview-buffer-name source-buffer))))
            (if (not (file-writable-p overview-file))
                (message "Error: Overview file is not writable: %s" overview-file)
              (with-current-buffer (or overview-buffer (find-file-noselect overview-file))
                (let ((inhibit-read-only t))
                  ;; 清理现有内容
                  (org-zettel-ref-clean-sections (current-buffer))
                  ;; 插入新内容
                  (org-zettel-ref-insert-notes-and-text source-buffer (current-buffer)))
                (save-buffer))
              (message "Debug: Synchronization completed successfully")))
        (error (message "Error during synchronization: %s" (error-message-string err)))))))

(defun org-zettel-ref-clean-sections (buffer)
  "Clean the Quick Notes and Marked Text sections in BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        ;; 清理 Quick Notes 到 Marked Text 之间的内容
        (when (re-search-forward "^\\* Quick Notes\n" nil t)
          (let ((start (point)))
            (if (re-search-forward "^\\* Marked Text\n" nil t)
                (delete-region start (match-beginning 0))
              (delete-region start (point-max)))))
        ;; 清理 Marked Text 之后的所有内容
        (goto-char (point-min))
        (when (re-search-forward "^\\* Marked Text\n" nil t)
          (delete-region (point) (point-max)))
        ;; 确保 Quick Notes 和 Marked Text 标题存在
        (goto-char (point-max))
        (unless (save-excursion (re-search-backward "^\\* Quick Notes\n" nil t))
          (insert "\n* Quick Notes\n"))
        (goto-char (point-max))
        (unless (save-excursion (re-search-backward "^\\* Marked Text\n" nil t))
          (insert "\n* Marked Text\n"))))))

;;-------------------------
;; END: Synchronization
;;-------------------------

;;-------------------------
;; START: Initialization
;;-------------------------

(defun org-zettel-ref-setup-overview-window (overview-file overview-buffer-name)
  "Setup the overview window with OVERVIEW-FILE and BUFFER-NAME on the right side of the source file."
  (let* ((buffer (find-file-noselect overview-file))
         (source-window (selected-window))
         (overview-window (split-window-right)))
    (with-current-buffer buffer
      (unless (eq major-mode 'org-mode)
        (org-mode))
      (setq buffer-read-only t))
    (set-window-buffer overview-window buffer)
    (select-window overview-window)
    (rename-buffer overview-buffer-name t)
    (select-window source-window)
    buffer))

(defun org-zettel-ref-init ()
  "Initialize org-zettel-ref-mode, create or open the overview file and set the layout."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (source-file (buffer-file-name source-buffer))
         (overview-buffer-name (org-zettel-ref-get-overview-buffer-name source-buffer))
         (existing-overview-buffer (get-buffer overview-buffer-name))
         (slug-title (org-zettel-ref-denote-slug-title (file-name-base source-file)))
         (overview-file (org-zettel-ref-find-denote-overview-file slug-title)))
    (message "Debug: existing-overview-buffer: %s" existing-overview-buffer)
    (message "Debug: overview-file: %s" overview-file)
    (if (and existing-overview-buffer (get-buffer-window existing-overview-buffer))
        (progn
          (message "Debug: Using existing overview buffer and window")
          (select-window (get-buffer-window existing-overview-buffer)))
      (if existing-overview-buffer
          (progn
            (message "Debug: Using existing overview buffer")
            (org-zettel-ref-setup-overview-window (buffer-file-name existing-overview-buffer) overview-buffer-name))
        (if overview-file
            (progn
              (message "Debug: Opening existing overview file: %s" overview-file)
              (org-zettel-ref-setup-overview-window overview-file overview-buffer-name))
          (progn
            (setq overview-file (org-zettel-ref-get-overview-file source-buffer))
            (message "Debug: Creating new overview file: %s" overview-file)
            (org-zettel-ref-setup-overview-window overview-file overview-buffer-name)))))
    (message "Debug: org-zettel-ref-init completed")))

(defun org-zettel-ref-setup-overview-buffer (buffer)
  "Setup the overview BUFFER."
  (with-current-buffer buffer
    (org-mode)
    (setq buffer-read-only t)
    ;;(org-zettel-ref-sync-overview)
    ))

(defun org-zettel-ref-find-denote-overview-file (slug-title)
  "Find an existing Denote overview file matching SLUG-TITLE."
  (let ((files (directory-files org-zettel-ref-overview-directory t "__overview\\.org$")))
    (message "Debug: Searching for slug-title '%s' in files: %s" slug-title files)
    (cl-find-if
     (lambda (file)
       (if (string-match-p (regexp-quote slug-title) (file-name-nondirectory file))
           (progn
             (message "Debug: File '%s' matches slug-title" file)
             file) ;; 返回文件路径
         (progn
           (message "Debug: File '%s' does not match" file)
           nil)))
     files)))
     
;;-------------------------
;; END: Initialization
;;-------------------------

;;-------------------------
;; START: Overview File Creation
;;-------------------------
(defun org-zettel-ref-get-normal-overview (source-buffer overview-file)
  "Create an overview file for SOURCE-BUFFER in normal mode."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (file-name-base source-file))
         (id (org-zettel-ref-generate-id title)))
    (unless (file-exists-p overview-file)
      (with-temp-file overview-file
        (insert (format "#+TITLE: Overview - %s\n#+SOURCE_FILE: %s\n#+filetags: :overview:\n#+startup: showall\n\n* Quick Notes\n\n* Marked Text\n" id title source-file))))
    overview-file))



(defun org-zettel-ref-get-overview-file-org-roam (source-buffer overview-file)
  "Use org-roam mode to get or create an overview file for SOURCE-BUFFER."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (file-name-base source-file))
         (org-id (org-id-new)))
    (unless (file-exists-p overview-file)
      (with-temp-file overview-file
        (insert (format ":PROPERTIES:\n:ID:       %s\n:END:\n#+title: Overview - %s\n#+filetags: :overview:\n#+startup: showall\n\n* Quick Notes\n\n* Marked Text\n" org-id title))))
    (when (and (featurep 'org-roam)
               (fboundp 'org-roam-db-update-file))
      (org-roam-db-update-file overview-file))
    overview-file))

(defun org-zettel-ref-denote-slug-title (title)
  "Generate a slug from TITLE for Denote."
  (replace-regexp-in-string "[^a-zA-Z0-9\u4e00-\u9fff-]" "-" (downcase title)))



(defun org-zettel-ref-get-overview-file-denote (source-buffer overview-file)
  "Get or create an overview file for SOURCE-BUFFER using Denote mode."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (file-name-base source-file)))
    (unless (file-exists-p overview-file)
      (with-temp-file overview-file
        (insert (format "#+title: Overview - %s\n#+startup: showall\n#+filetags: :overview:\n\n* Quick Notes\n\n\n* Marked Text\n" title))))
    overview-file))

;;-------------------------
;; END: Overview File Creation
;;-------------------------

(provide 'org-zettel-ref-core)

;;; org-zettel-ref-core.el ends here
