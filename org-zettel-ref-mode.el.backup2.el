;;; org-zettel-ref-mode.el --- Zettelsken-style Reference Note in Org mode -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Yibie

;; Author: Yibie <yibie@outlook.com>
;; Version: 0.3.1
;; Package-Requires: ((emacs "29.4"))
;; Keywords: outlines
;; URL: https://github.com/yibie/org-zettel-ref-mode

;;; Commentary:

;; This package provides a mode for creating and managing a reference
;; overview file for marked text and quick notes in Org mode. When activated,
;; it will create (or open) an overview file and display it alongside the original
;; buffer. Any marked text or quick notes in the original buffer will be
;; automatically extracted and added to the overview file.

;;; Code:

(require 'org)
(require 'org-element)
(require 'pulse)
(require 'org-id)
(require 'cl-lib)

(defgroup org-zettel-ref nil
  "Customization options for org-zettel-ref-mode."
  :group 'org)

(defvar org-zettel-ref-current-overview-buffer nil
  "The name of the current overview buffer.")

(defcustom org-zettel-ref-overview-directory "~/org-zettel-ref-overviews/"
  "Directory to store overview files."
  :type 'directory
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-include-empty-notes nil
  "If non-nil, include empty quick notes in the overview."
  :type 'boolean
  :group 'org-zettel-ref)

(defvar org-zettel-ref-overview-file nil
  "The file path where the marked text overview is saved.")

(defcustom org-zettel-ref-mode-type 'normal
  "Specify the mode type for org-zettel-ref.
Possible values are:
'normal  - Default behavior
'denote  - Use Denote file naming convention
'org-roam - Use Org-roam format with ID property"
  :type '(choice (const :tag "Normal" normal)
                 (const :tag "Denote" denote)
                 (const :tag "Org-roam" org-roam))
  :group 'org-zettel-ref)


;; Define the mode type variable
(defvar org-zettel-ref-mode-type 'normal
  "Type of org-zettel-ref-mode. Can be 'org-roam, 'denote, or 'normal.")

;; Function to generate Denote title by directly using the source file name
(defun org-zettel-ref-denote-title (title)
  "Generate a title for Denote by directly using the source file name."
  title)

;; Function to create or retrieve the overview file with '__overview' keyword
(defun org-zettel-ref-create-denote-overview (title)
  "Create or retrieve a Denote overview file for TITLE."
  (let* ((slug-title (org-zettel-ref-denote-slug-title title))
         (existing-file (org-zettel-ref-find-denote-overview-file slug-title)))
    (if existing-file
        (progn
          (message "Debug: Found existing Denote overview file: %s" existing-file)
          existing-file)
      (let* ((timestamp (format-time-string "%Y%m%dT%H%M%S"))
             (base-name (concat timestamp "--" slug-title "--__overview.org"))
             (file-path (expand-file-name base-name org-zettel-ref-overview-directory)))
        (message "Debug: Creating new Denote overview file: %s" file-path)
        (unless (file-exists-p file-path)
          (with-temp-file file-path
            (insert (format "#+title: %s\n" title))
            (insert "#+filetags: :overview:\n")
            (insert (format "#+SOURCE_FILE: %s\n" title))))
        (when (and (fboundp 'denote-rename-file-using-front-matter)
                   (not (string-prefix-p timestamp (file-name-nondirectory file-path))))
          (denote-rename-file-using-front-matter file-path))
        file-path))))

;;;###autoload
(defun org-zettel-ref-sanitize-filename (filename)
  "Sanitize FILENAME by replacing invalid characters with underscores.
Preserves alphanumeric characters, Chinese characters, and some common punctuation."
  (let ((invalid-chars-regexp "[[:cntrl:]\\/:*?\"<>|]"))
    (replace-regexp-in-string invalid-chars-regexp "_" filename)))
;;;###autoload

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

(defun org-zettel-ref-ensure-org-roam-db-update (file)
  "Ensure FILE is added to the org-roam database if in org-roam mode."
  (when (and (eq org-zettel-ref-mode-type 'org-roam)
             (require 'org-roam nil t)
             (fboundp 'org-roam-db-autosync-mode)
             org-roam-db-autosync-mode)
    (let ((buffer (find-file-noselect file)))
      (if buffer
          (with-current-buffer buffer
            (org-roam-db-update-file))
        (error "Failed to open file: %s" file)))))

(defun org-zettel-ref-generate-file-content (source-buffer title)
  "Generate file content based on TITLE and current mode type."
  (pcase org-zettel-ref-mode-type
    ('normal (cons nil (format "#+title: Overview for %s\n\n" title)))
    ('denote
     (let ((date (format-time-string "[%Y-%m-%d %a %H:%M]"))
           (id (format-time-string "%Y%m%dT%H%M%S")))
       (cons nil (format "#+title:      %s\n#+date:       %s\n#+filetags:   \n#+identifier: %s\n\n"
                         title date id))))
    ('org-roam
     (if (require 'org-roam nil t)
         (let* ((id (org-id-new))  ; 使用 org-id-new 而不是 org-roam-id-new
                (creation-date (format-time-string "%Y-%m-%d %H:%M:%S"))
                (content (format ":PROPERTIES:
:ID:       %s
:END:
#+title: Overview of %s
#+date: %s
#+filetags: :overview:


* Quick Notes

* Marked Text

"
                                id
                                title
                                creation-date
                                title)))
           (cons id content))
       ;; Fallback to normal mode if org-roam is not available
       (message "Org-roam is not available. Falling back to normal mode.")
       (org-zettel-ref-generate-file-content source-buffer title 'normal)))))

(defun org-zettel-ref-get-overview-buffer-name (source-buffer)
  "Get the overview buffer name for the given SOURCE-BUFFER."
  (let* ((source-file-name (buffer-file-name source-buffer))
         (title (if source-file-name (file-name-base source-file-name) "Untitled")))
    (format "*Org Zettel Ref: %s__overview*" title)))

(defun org-zettel-ref-slugify (title)
  "Convert TITLE to a slug for generating filenames."
  (let ((slug (downcase (replace-regexp-in-string "[^[:alnum:][:digit:]\u4e00-\u9fff]+" "-" title))))
    (string-trim slug "-+")))

(defun org-zettel-ref-denote-slug-title (title)
  "Generate a slug from TITLE for Denote."
  (org-zettel-ref-slugify title))


(defun org-zettel-ref-fallback-file-path (sanitized-title)
  "Generate a fallback file path for SANITIZED-TITLE."
  (expand-file-name
   (concat sanitized-title ".org")
   org-zettel-ref-overview-directory))

(defun org-zettel-ref-update-roam-db (file)
  "Update org-roam database for FILE."
  (when (and (eq org-zettel-ref-mode-type 'org-roam)
             (require 'org-roam nil t))
    (condition-case err
        (progn
          (org-roam-db-update-file file)
          (message "Debug: org-roam database updated for %s" file)
          (let ((node (org-roam-node-from-file file)))
            (when node
              (message "Debug: Node ID: %s, Title: %s" 
                       (org-roam-node-id node) 
                       (org-roam-node-title node)))))
      (error
       (message "Error updating org-roam database: %S" err)))))


(defun org-zettel-ref-create-overview-file (file-path title)
  "Create a new overview Org file based on FILE-PATH and TITLE."
  (with-temp-buffer
    ;; Insert basic Org template content
    (insert (format "#+title: %s\n" title))
    (insert (format "#+date: %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
    ;; Add more template content as needed
    (insert "* Quick Notes\n\n* Marked Text\n")
    ;; Save the file
    (write-file file-path))
  ;; Set the file to read-only
  (with-current-buffer (find-file-noselect file-path)
    (read-only-mode 1))
  (message "Overview file created: %s" file-path))


(defun org-zettel-ref-check-overview-content (overview-file)
  "Check and fix the content of the overview file if necessary."
  (with-current-buffer (find-file-noselect overview-file)
    (goto-char (point-min))
    (unless (re-search-forward "^\\* Quick Notes" nil t)
      (goto-char (point-max))
      (insert "\n* Quick Notes\n\n* Marked Text\n")
      (save-buffer))
    (message "Overview file content checked and fixed if necessary.")))

(defun org-zettel-ref-get-org-roam-file (title)
  (if (require 'org-roam nil t)
      (condition-case err
          (let ((existing-node (org-roam-node-from-title-or-alias title)))
            (if existing-node
                (progn
                  (message "Debug: Existing node found. File path: %s" (org-roam-node-file existing-node))
                  (org-roam-node-file existing-node))
              (let* ((id (org-id-new))
                     (slug (org-roam-node-slug (org-roam-node-create :title title)))
                     (new-file (expand-file-name (concat slug ".org") org-roam-directory)))
                (message "Debug: New node created. File path: %s" new-file)
                new-file)))
        (error
         (message "Error in org-roam operations: %S" err)
         (org-zettel-ref-fallback-file-path title)))
    (message "org-roam not available, falling back to normal mode")
    (org-zettel-ref-fallback-file-path title)))

;;-----------START: org-zettel-ref-overveiw-index------------------
;; Use index file to manage overview files, 

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


(defun org-zettel-ref-fix-overview-files ()
  "Fix overview files by adding SOURCE_FILE property if missing."
  (interactive)
  (let ((overview-files (directory-files org-zettel-ref-overview-directory t ".*__overview\\.org$")))
    (dolist (file overview-files)
      (let ((source-file (file-name-nondirectory (file-name-sans-extension file))))
        (setq source-file (replace-regexp-in-string "__overview$" "" source-file))
        (org-zettel-ref-add-source-file-property file source-file)))
    (message "Overview files updated with SOURCE_FILE properties.")))

(defun org-zettel-ref-create-normal-overview (file-path content source-file)
  "Create a normal overview file at FILE-PATH with CONTENT and SOURCE-FILE property."
  (unless (file-exists-p file-path)
    (with-temp-file file-path
      (insert (format "#+SOURCE_FILE: %s\n" source-file))
      (insert (cdr content))))
  file-path)

(defun org-zettel-ref-create-org-roam-overview (file-path source-buffer title)
  "Create an Org-roam overview file at FILE-PATH for SOURCE-BUFFER with TITLE."
  (unless (file-exists-p file-path)
    (let* ((content (org-zettel-ref-generate-file-content source-buffer title))
           (id (car content))
           (body (cdr content)))
      (with-temp-file file-path
        (when id
          (insert (format ":PROPERTIES:\n:ID:       %s\n:END:\n" id)))
        (insert (format "#+SOURCE_FILE: %s\n" (buffer-file-name source-buffer)))
        (insert body))))
  (when (and (eq org-zettel-ref-mode-type 'org-roam)
             (fboundp 'org-roam-db-autosync-mode))
    (org-zettel-ref-ensure-org-roam-db-update file-path)))

(defun org-zettel-ref-get-overview-file (source-buffer)
  "Get or create the overview file for SOURCE-BUFFER based on the current mode type."
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
            ('normal (org-zettel-ref-create-normal-overview file-path (org-zettel-ref-generate-file-content source-buffer title) source-file))
            ('denote (org-zettel-ref-create-denote-overview title))
            ('org-roam (org-zettel-ref-create-org-roam-overview file-path source-buffer title))
            (_ (error "Unsupported org-zettel-ref-mode-type: %s" org-zettel-ref-mode-type)))))
        (org-zettel-ref-update-index source-file new-overview)
        (message "Debug: Created new overview file: %s" new-overview)
        new-overview))))

(defun org-zettel-ref-get-source-file-property ()
  "Get the SOURCE_FILE property from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+SOURCE_FILE:.*$" nil t)
      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

(defun org-zettel-ref-ensure-source-file-property (file source-file)
  "Ensure FILE has the SOURCE_FILE property set to SOURCE-FILE."
  (with-current-buffer (find-file-noselect file)
    (unless (org-zettel-ref-get-source-file-property)
      (goto-char (point-min))
      (forward-line)
      (insert (format "#+SOURCE_FILE: %s\n" source-file))
      (save-buffer))))

(defun org-zettel-ref-rescan-overview-files ()
  "Rescan the overview directory and update the index."
  (interactive)
  (let ((overview-files (directory-files org-zettel-ref-overview-directory t ".*__overview\\.org$")))
    (setq org-zettel-ref-overview-index (make-hash-table :test 'equal))
    (dolist (file overview-files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward "^#\\+SOURCE_FILE: \\(.+\\)$" nil t)
          (let ((source-file (match-string 1)))
            (org-zettel-ref-update-index source-file file)))))
    (message "Overview files rescanned and index updated.")))

(defun org-zettel-ref-refresh-index ()
  "Manually refresh the overview index."
  (interactive)
  (org-zettel-ref-rescan-overview-files)
  (message "Overview index has been refreshed."))

(defun org-zettel-ref-ensure-overview-file (source-file)
  "Ensure that the overview file for SOURCE-FILE exists and is properly linked."
  (let ((overview-file (org-zettel-ref-get-overview-from-index source-file)))
    (cond
     ;;Overviews directory is exsit.
     ((and overview-file (file-exists-p overview-file))
      overview-file)
     
     ;; Overview file exist, but not found. 
     ((and overview-file (not (file-exists-p overview-file)))
      (message "Warning: Overview file %s not found. Creating a new one." overview-file)
      (org-zettel-ref-get-overview-file (find-buffer-visiting source-file)))
     
     ;; Overview file is not exist.
     (t
      (message "Creating new overview file for %s" source-file)
      (org-zettel-ref-get-overview-file (find-buffer-visiting source-file))))))

(defun org-zettel-ref-check-and-repair-links ()
  "Check and repair links between source files and overview files."
  (interactive)
  (let ((repaired 0))
    (maphash (lambda (source-file overview-file)
                (unless (file-exists-p source-file)
                  (remhash source-file org-zettel-ref-overview-index)
                  (setq repaired (1+ repaired)))
                (unless (file-exists-p overview-file)
                  (org-zettel-ref-ensure-overview-file source-file)
                  (setq repaired (1+ repaired))))
              org-zettel-ref-overview-index)
    (org-zettel-ref-save-index)
    (message "Checked and repaired %d links." repaired)))

(defun org-zettel-ref-status ()
  "Display the current status of org-zettel-ref-mode."
  (interactive)
  (let ((index-size (hash-table-count org-zettel-ref-overview-index))
        (current-overview (org-zettel-ref-get-overview-from-index (buffer-file-name))))
    (message "org-zettel-ref-mode status:
- Index size: %d entries
- Current file: %s
- Associated overview: %s
- Overview directory: %s"
             index-size
             (buffer-file-name)
             (or current-overview "None")
             org-zettel-ref-overview-directory)))

(defun org-zettel-ref-maintenance-menu ()
  "Display a menu for org-zettel-ref-mode maintenance operations."
  (interactive)
  (let ((choice (read-char-choice
                 "Org-zettel-ref maintenance:
r: Refresh index
c: Check and repair links
s: Show status
q: Quit
Your choice: "
                 '(?r ?c ?s ?q))))
    (cond
     ((eq choice ?r) (org-zettel-ref-refresh-index))
     ((eq choice ?c) (org-zettel-ref-check-and-repair-links))
     ((eq choice ?s) (org-zettel-ref-status))
     ((eq choice ?q) (message "Quit")))))

(defvar org-zettel-ref-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c z m") 'org-zettel-ref-maintenance-menu)
    map)
  "Keymap for `org-zettel-ref-mode'.")

(define-minor-mode org-zettel-ref-mode
  "Minor mode for managing reference notes in Org mode."
  :init-value nil
  :lighter " ZettelRef"
  :keymap org-zettel-ref-mode-map
  (if org-zettel-ref-mode
      (progn
        (org-zettel-ref-load-index)
        (org-zettel-ref-rescan-overview-files)
        (org-zettel-ref-mode-enable))
    (org-zettel-ref-mode-disable)))



;;-----------END: org-zettel-ref-overveiw-index------------------

(defun org-zettel-ref-sync-overview ()
  "Synchronize quick notes and marked text from the source buffer to the overview file."
  (interactive)
  (condition-case err
      (let* ((source-buffer (or (buffer-base-buffer) (current-buffer)))
             (source-file (buffer-file-name source-buffer))
             (overview-file (org-zettel-ref-get-overview-file source-buffer))
             (overview-buffer (or (get-buffer org-zettel-ref-current-overview-buffer)
                                 (find-file-noselect overview-file))))
        (message "Debug: Starting to sync overview file: %s" overview-file)
        (unless (buffer-live-p source-buffer)
          (error "Source buffer does not exist"))
        (unless (file-writable-p overview-file)
          (error "Overview file is not writable: %s" overview-file))
        
        (with-current-buffer overview-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "#+TITLE: %s\n" (file-name-base source-file)))
            (insert (format "#+SOURCE_FILE: %s\n" source-file))
            (insert "#+filetags: :overview:\n#+STARTUP: showall\n\n")
            (insert "* Quick Notes\n\n")
            (condition-case inner-err
                (org-zettel-ref-insert-quick-notes source-buffer overview-buffer)
              (error
               (message "Error in org-zettel-ref-insert-quick-notes: %s" (error-message-string inner-err))))
            (goto-char (point-max))  ; Ensure we're at the end of the buffer
            (insert "\n* Marked Text\n\n")
            (let ((marked-text-start (point)))  ; Remember where "Marked Text" section starts
              (condition-case inner-err
                  (org-zettel-ref-insert-marked-text source-buffer overview-buffer)
                (error
                 (message "Error in org-zettel-ref-insert-marked-text: %s" (error-message-string inner-err))))
              ;; Check if any marked text was actually inserted
              (if (= marked-text-start (point))
                  (message "Debug: No marked text was inserted")
                (message "Debug: Marked text was successfully inserted")))
            (save-buffer)))
        
        (message "Debug: Sync completed successfully"))
    (error
     (message "Error during sync process: %s" (error-message-string err))
     (message "Error type: %s" (car err))
     (message "Backtrace: %s" (with-output-to-string (backtrace))))))

(defun org-zettel-ref-get-source-file-property ()
  "Get the SOURCE_FILE property from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+SOURCE_FILE:.*$" nil t)
      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

(defun org-zettel-ref-add-quick-note ()
  "Add a quick note to the current buffer."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (note-name (read-string "Enter note: ")))
    ;; Source buffer insert note
    (insert (format "<<%s>>" note-name))
    ;; Sync to overview file
    (org-zettel-ref-sync-overview)))

(defun org-zettel-ref-insert-marked-text (source-buffer overview-buffer)
  "Insert marked text from SOURCE-BUFFER into OVERVIEW-BUFFER in the order they appear, preserving original markup."
  (message "Debug: Starting to insert marked text")
  (let ((marked-elements '())
        (count 0))
    (with-current-buffer source-buffer
      (message "Debug: Parsing source buffer: %s (size: %d)" (buffer-name) (buffer-size))
      (let ((parsed-buffer (org-element-parse-buffer)))
        (message "Debug: Finished parsing buffer")
        (org-element-map parsed-buffer '(bold italic underline strike-through code verbatim)
          (lambda (element)
            (let ((begin (org-element-property :begin element))
                  (end (org-element-property :end element)))
              (message "Debug: Found element %s from %d to %d" (org-element-type element) begin end)
              (when (and begin end (<= end (point-max)))
                (push (cons begin element) marked-elements)
                (setq count (1+ count))))))))
    
    (setq marked-elements (sort marked-elements (lambda (a b) (< (car a) (car b)))))
    
    (with-current-buffer overview-buffer
      (message "Debug: Inserting into overview buffer: %s (size: %d)" (buffer-name) (buffer-size))
      (dolist (elem marked-elements)
        (let* ((element (cdr elem))
               (begin (org-element-property :begin element))
               (end (org-element-property :end element)))
          (message "Debug: Attempting to insert element from %d to %d" begin end)
          (when (and begin end (<= end (buffer-size source-buffer)))
            (condition-case err
                (let ((contents-with-markers (with-current-buffer source-buffer
                                               (buffer-substring begin end))))
                  (insert (format "- %s\n" contents-with-markers))
                  (message "Debug: Successfully inserted element"))
              (error
               (message "Error inserting element: %s" (error-message-string err))))))))
    
    (message "Debug: Finished inserting marked text, processed %d elements" count)))

(defun org-zettel-ref-quick-markup ()
  "Quickly apply org-mode markup to a region or insert at the cursor."
  (interactive)
  (let* ((markup-types '(("bold" . "*")
                         ("italic" . "/")
                         ("underline" . "_")
                         ("code" . "~")
                         ("verbatim" . "=")
                         ("strike-through" . "+")))
         (markup (completing-read "Select markup: " markup-types nil t))
         (marker (cdr (assoc markup markup-types)))
         (region-active (use-region-p))
         (beg (if region-active (region-beginning) (point)))
         (end (if region-active (region-end) (point))))
    (if region-active
        (save-excursion
          (goto-char beg)
          (skip-chars-forward " \t\n")
          (setq beg (point))
          (goto-char end)
          (skip-chars-backward " \t\n")
          (setq end (point))
          (goto-char end)
          (insert marker)
          (goto-char beg)
          (insert marker)
          (backward-char))
      (insert marker marker)
      (backward-char))
    (message "Applied %s markup" markup)))

(defun org-zettel-ref-update-marked-text ()
  "Update the marked text in the overview file corresponding to the current buffer.
This function acts as a fallback mechanism when users notice missing marks."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (source-file (buffer-file-name source-buffer))
         (overview-file (org-zettel-ref-get-overview-file source-buffer))
         (overview-buffer (find-file-noselect overview-file)))
    (if (and source-file overview-file)
        (progn
          (with-current-buffer overview-buffer
            (save-excursion
              (goto-char (point-min))
              (if (search-forward "* Marked Text" nil t)
                  (progn
                    (forward-line 1)
                    (delete-region (point) (point-max))
                    (insert "\n")
                    (org-zettel-ref-insert-marked-text source-buffer overview-buffer)
                    (save-buffer)
                    (message "Marked text updated"))
                (message "No marked text section found, please check the overview file format"))))
          (pop-to-buffer overview-buffer))
      (message "Cannot find source or overview file"))))


(defun org-zettel-ref-insert-quick-notes (source-buffer overview-buffer)
  "Insert quick notes from SOURCE-BUFFER into OVERVIEW-BUFFER."
  (condition-case err
      (progn
        (unless (buffer-live-p source-buffer)
          (error "Source buffer is not live"))
        (unless (buffer-live-p overview-buffer)
          (error "Overview buffer is not live"))
        (with-current-buffer source-buffer
          (let ((ast (condition-case parse-err
                         (org-element-parse-buffer)
                       (error
                        (message "Error parsing buffer: %s" (error-message-string parse-err))
                        nil))))
            (when ast
              (org-element-map ast 'target
                (lambda (target)
                  (let* ((begin (org-element-property :begin target))
                         (end (org-element-property :end target))
                         (content (buffer-substring-no-properties begin end)))
                    (when (string-match "<<\\([^>]+\\)>>" content)
                      (let ((note-name (match-string 1 content))
                            (file-name (buffer-file-name source-buffer)))
                        (if file-name
                            (with-current-buffer overview-buffer
                              (insert (format "- [[file:%s::%s][%s]]\n"
                                              file-name
                                              note-name
                                              note-name)))
                          (message "Warning: Source buffer is not associated with a file"))))))))))
        (message "Quick notes insertion completed successfully"))
    (error
     (message "Error in org-zettel-ref-insert-quick-notes: %s" (error-message-string err)))))


(defun org-zettel-ref-jump-to-quick-note ()
  "Jump to the corresponding quick note position in the source file.
If the cursor is on a quick note link, pressing `C-o` will open the source file and jump to the position of that note."
  (interactive)
  (let* ((context (org-element-context)))
    (if (and (eq (org-element-type context) 'link)
             ;; Assume the quick note link type is "file"
             (string= (org-element-property :type context) "file"))
        (let* ((path (org-element-property :path context))
               ;; Assume `:search-option` stores the line number or position identifier
               (search (org-element-property :search-option context)))
          (if (and path search)
              (let ((source-buffer (find-buffer-visiting path)))
                (if source-buffer
                    (let ((source-window (get-buffer-window source-buffer t)))
                      (if source-window
                          (progn
                            ;; 1. If the source file window exists, switch to that window and jump
                            (select-window source-window)
                            (org-zettel-ref-goto-position source-buffer search))
                        ;; 2. If the source file window doesn't exist, and only the overview window is present, split the window and jump
                        (let ((overview-window (get-buffer-window (current-buffer))))
                          (when overview-window
                            (let ((new-window (split-window overview-window 'left nil)))
                              (select-window new-window)
                              (switch-to-buffer source-buffer)
                              (org-zettel-ref-goto-position source-buffer search))))))
                  ;; 3. If the source file is not open, open it and jump to the position
                  (progn
                    (find-file path)
                    (org-zettel-ref-goto-position (current-buffer) search))))
            (message "The current link lacks a search target.")))
      (message "The cursor is not on a valid quick note link."))))

(defun org-zettel-ref-goto-position (buffer search)
  "Jump to the position specified by SEARCH in BUFFER.
If SEARCH is a line number, move the cursor to that line."
  (with-current-buffer buffer
    (goto-char (point-min))
    (forward-line (1- (string-to-number search)))
    (recenter)))

(defun org-zettel-ref-bind-jump-key ()
  "Bind the `C-o` key to the `org-zettel-ref-jump-to-quick-note` function in the overview buffer."
  (when (derived-mode-p 'org-mode)
    (local-set-key (kbd "C-o") 'org-zettel-ref-jump-to-quick-note)))

(defun org-zettel-ref-open-source-link (link)
  "Open the source file of a link and jump to the target position.
If the source file is already visible, jump to it.
Otherwise, open the source file in a new window."
  (let* ((path (org-element-property :path link))
         (search (org-element-property :search-option link))
         (source-buffer (find-buffer-visiting path)))
    (if source-buffer
        (progn
          (pop-to-buffer source-buffer)
          (when search
            (org-link-search search)))
      (find-file-other-window path)
      (when search
        (org-link-search search)))
    (recenter)))

(defun org-zettel-ref-advice-open-at-point (orig-fun &rest args)
  "Advice function to customize org-open-at-point behavior for overview links."
  (let ((context (org-element-context)))
    (if (and (eq (org-element-type context) 'link)
             (string-equal (org-element-property :type context) "file"))
        (org-zettel-ref-open-source-link context)
      (apply orig-fun args))))

(defun org-zettel-ref-clean-multiple-targets ()
  "Remove all <<target>>, *, and = markers from the current buffer after confirmation."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to remove all <<target>>, *, and = markers? This cannot be undone. ")
    (save-excursion
      (goto-char (point-min))
      ;; Remove <<target>> markers
      (while (re-search-forward "<<\\([^>]+\\)>>" nil t)
        (replace-match "")
        (just-one-space))
      ;; Remove * markers
      (goto-char (point-min))
      (while (re-search-forward "\\*\\([^*\n]+?\\)\\*" nil t)
        (replace-match "\\1"))
      ;; Remove = and == markers
      (goto-char (point-min))
      (while (re-search-forward "\\(=\\|==\\)\\([^=\n]+?\\)\\1" nil t)
        (replace-match "\\2")
        (just-one-space)))
    (message "All <<target>>, **, and == markers have been removed.")))

(defun org-zettel-ref-clean-targets-and-sync ()
  "Clean all <<target>> markers from the current buffer and then sync the overview."
  (interactive)
  (org-zettel-ref-clean-multiple-targets)
  (save-buffer)
  (org-zettel-ref-sync-overview))

;; Function to find existing overview file matching the title and '__overview' keyword
(defun org-zettel-ref-find-denote-overview-file (slug-title)
  "Find an existing Denote overview file matching SLUG-TITLE."
  (let ((files (directory-files org-zettel-ref-overview-directory t "__overview\\.org$")))
    (cl-find-if
     (lambda (file)
       (string-match-p (regexp-quote slug-title) (file-name-nondirectory file)))
     files)))

;; Function to get the buffer name for the overview file
(defun org-zettel-ref-get-overview-buffer-name (source-buffer)
  "Get the overview buffer name for the given SOURCE-BUFFER."
  (let* ((source-file-name (buffer-file-name source-buffer))
         (title (file-name-base source-file-name)) ;; Directly use the base name of the file
         (buffer-name (format "*Org Zettel Ref: %s__overview*" title)))
    buffer-name))


;; Function to initialize org-zettel-ref-mode
(defun org-zettel-ref-init ()
  "Initialize the org-zettel-ref-mode, create or open the overview file and set up the layout."
  (interactive)
  (message "Debug: Starting org-zettel-ref-init")
  (message "Debug: Current buffer: %s" (buffer-name))
  (message "Debug: Buffer file name: %s" (buffer-file-name))
  (message "Debug: Major mode: %s" major-mode)
  
  (unless org-zettel-ref-mode
    (message "Debug: Enabling org-zettel-ref-mode")
    (org-zettel-ref-mode 1))
  
  (org-zettel-ref-load-index)
  
  (let* ((source-buffer (current-buffer))
         (source-file (buffer-file-name source-buffer)))
    (if (not source-file)
        (progn
          (message "Error: org-zettel-ref-mode cannot be initialized in buffers without associated files.")
          (message "Debug: Please open a file before initializing org-zettel-ref-mode."))
      (let* ((title (file-name-base source-file))
             (overview-buffer-name (org-zettel-ref-get-overview-buffer-name source-buffer))
             (overview-file (org-zettel-ref-get-overview-file source-buffer)))
        (message "Debug: source-buffer = %S" source-buffer)
        (message "Debug: source-file = %S" source-file)
        (message "Debug: title = %S" title)
        (message "Debug: overview-file = %S" overview-file)
        (message "Debug: overview-buffer-name = %S" overview-buffer-name)
        
        ;; Check if an overview window already exists
        (if (org-zettel-ref-overview-window-p overview-buffer-name)
            (progn
              (message "Debug: Overview window already exists.")
              (org-zettel-ref-focus-overview-window overview-buffer-name))
          ;; If no existing overview window, create a new one
          (let ((source-window (get-buffer-window source-buffer)))
            (when (window-live-p source-window)
              (select-window source-window))
            (let ((new-window (split-window-right)))
              (with-selected-window new-window
                (find-file overview-file)
                (rename-buffer overview-buffer-name t)
                (read-only-mode 1))
              (message "Debug: New window created: %S" new-window))))
        
        (message "Debug: Source buffer window: %S" (get-buffer-window source-buffer))
        ;; Set variables
        (setq-local org-zettel-ref-overview-file overview-file)
        (setq-local org-zettel-ref-current-overview-buffer overview-buffer-name)
        
        (message "Debug: Setting up buffers")
        (org-zettel-ref-setup-buffers source-buffer (get-buffer overview-buffer-name))
        
        ;; Ensure the cursor returns to the source file
        (select-window (get-buffer-window source-buffer))
        
        (org-zettel-ref-bind-jump-key)
        
        ;; Synchronize the overview file with the source file
        (org-zettel-ref-sync-overview)
        
        (message "Debug: org-zettel-ref-init completed")))))

;; Function to check if an overview window already exists
(defun org-zettel-ref-overview-window-p (overview-buffer-name)
  "Check if a window displaying OVERVIEW-BUFFER-NAME already exists."
  (cl-some (lambda (win)
             (string-equal (buffer-name (window-buffer win)) overview-buffer-name))
           (window-list)))

;; Function to focus on the existing overview window
(defun org-zettel-ref-focus-overview-window (overview-buffer-name)
  "Focus on the window displaying OVERVIEW-BUFFER-NAME."
  (let ((win (cl-find-if
              (lambda (w)
                (string-equal (buffer-name (window-buffer w)) overview-buffer-name))
              (window-list))))
    (when win
      (select-window win)
      (message "Debug: Focused on existing overview window."))))

(defun org-zettel-ref-find-source-file (overview-file)
  "Find the source file associated with OVERVIEW-FILE using the index."
  (let ((source-file nil))
    (maphash (lambda (key value)
               (when (string= value overview-file)
                 (setq source-file key)))
             org-zettel-ref-overview-index)
    source-file))

(defun org-zettel-ref-jump-to-source ()
  "Jump from the overview file to its associated source file."
  (interactive)
  (let* ((overview-file (buffer-file-name))
         (source-file (org-zettel-ref-find-source-file overview-file)))
    (if source-file
        (if (file-exists-p source-file)
            (progn
              (find-file source-file)
              (message "Jump to source file: %s" source-file))
          (message "Source file not found: %s" source-file))
      (message "No associated source file found."))))

(defun org-zettel-ref-mode-enable ()
  "Enable org-zettel-ref-mode."
  (let ((overview-file (org-zettel-ref-get-overview-file (current-buffer))))
    (org-zettel-ref-check-overview-content overview-file))
  (advice-add 'org-open-at-point :around #'org-zettel-ref-advice-open-at-point)
  (org-zettel-ref-setup-quick-markup))

(defun org-zettel-ref-mode-disable ()
  "Disable org-zettel-ref-mode."
  (advice-remove 'org-open-at-point #'org-zettel-ref-advice-open-at-point)
  (local-unset-key (kbd org-zettel-ref-quick-markup-key)) 
  (local-unset-key (kbd "C-o")))

(provide 'org-zettel-ref)
(defun org-zettel-ref-setup-buffers (source-buffer overview-buffer)
  "Set up SOURCE-BUFFER and OVERVIEW-BUFFER related variables."
  (with-current-buffer source-buffer
    ;; Set necessary local variables in the source buffer without making it read-only
    (setq-local org-zettel-ref-overview-buffer overview-buffer))
  
  (with-current-buffer overview-buffer
    ;; Set the overview buffer to read-only
    (read-only-mode 1)
    (setq-local org-zettel-ref-source-buffer source-buffer)
    ;; Add other necessary settings...
    ))


(defun org-zettel-ref-check-roam-db ()
  "Check the status of the org-roam database."
  (interactive)
  (if (require 'org-roam nil t)
      (condition-case err
          (progn
            (message "Org-roam version: %s" (org-roam-version))
            (message "Org-roam directory: %s" org-roam-directory)
            (message "Org-roam database file: %s" org-roam-db-location)
            (if (file-exists-p org-roam-db-location)
                (message "Database file exists")
              (message "Database file does not exist"))
            (let ((node-count (caar (org-roam-db-query [:select (funcall count *) :from nodes]))))
              (message "Number of nodes in database: %d" node-count)))
        (error
         (message "Error checking org-roam database: %S" err)))
    (message "Org-roam is not available")))

;; org-zettel-ref-run-python-script
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


(defcustom org-zettel-ref-quick-markup-key "C-c m"
  "Shortcut key for quick markup function."
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
         (command (format "python %s --temp %s --reference %s --archive %s"
                          (shell-quote-argument (file-name-nondirectory script-path))
                          (shell-quote-argument org-zettel-ref-temp-folder)
                          (shell-quote-argument org-zettel-ref-reference-folder)
                          (shell-quote-argument org-zettel-ref-archive-folder))))
    (if (file-exists-p script-path)
        (progn
          (message "Executing command: %s" command)
          (async-shell-command command "*Convert to Org*"))
      (error "Cannot find the specified Python script: %s" script-path))))


;; setup quick markup
(defun org-zettel-ref-setup-quick-markup ()
  "Setup quick markup key binding."
  (local-set-key (kbd org-zettel-ref-quick-markup-key) 'org-zettel-ref-quick-markup))

;; enable org-zettel-ref-mode
(defun org-zettel-ref-mode-enable ()
  "Enable org-zettel-ref-mode."
  (let ((overview-file (org-zettel-ref-get-overview-file (current-buffer))))
    (org-zettel-ref-check-overview-content overview-file))
  (advice-add 'org-open-at-point :around #'org-zettel-ref-advice-open-at-point)
  (org-zettel-ref-setup-quick-markup))

(defun org-zettel-ref-mode-disable ()
  "Disable org-zettel-ref-mode."
  (advice-remove 'org-open-at-point #'org-zettel-ref-advice-open-at-point)
  (local-unset-key (kbd org-zettel-ref-quick-markup-key))
  (local-unset-key (kbd "C-o")))


;;;###autoload
(define-minor-mode org-zettel-ref-mode
  "Minor mode for managing reference notes in Org mode."
  :init-value nil
  :lighter " ZettelRef"
  :group 'org-zettel-ref
  (if org-zettel-ref-mode
      (progn
        (org-zettel-ref-load-index)
        (org-zettel-ref-rescan-overview-files)
        (when (eq org-zettel-ref-mode-type 'org-roam)
          (if (require 'org-roam nil t)
              (condition-case err
                (progn
                  (message "Debug: org-roam loaded successfully")
                  (message "Debug: org-roam-directory is set to %s" org-roam-directory)
                  (unless (file-exists-p org-roam-directory)
                    (message "Debug: org-roam directory does not exist, creating it")
                    (make-directory org-roam-directory t))
                  (org-roam-db)
                  (org-roam-list-files)
                  (org-roam-db-autosync-mode 1)
                  (message "Debug: org-roam initialized successfully"))
                (error
                 (message "Error initializing org-roam: %s" err)
                 (setq org-zettel-ref-mode-type 'normal)
                 (message "Debug: Falling back to normal mode due to org-roam error")))
        (message "org-roam is not available, falling back to normal mode")
        (setq org-zettel-ref-mode-type 'normal)))
        (org-zettel-ref-init))
    (progn
      (when (eq org-zettel-ref-mode-type 'org-roam)
        (org-roam-db-autosync-mode -1))
      (org-zettel-ref-mode-disable))))


(provide 'org-zettel-ref-mode)
