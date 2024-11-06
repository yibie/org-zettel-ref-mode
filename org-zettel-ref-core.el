;;; org-zettel-ref-core.el --- Core functionality for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains core functionality for org-zettel-ref.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'org-zettel-ref-utils)
(require 'org-zettel-ref-db) 
(require 'org-zettel-ref-list)
(require 'org-zettel-ref-migrate)

;;-----------------------
;; Minor Mode
;;-----------------------

(defvar org-zettel-ref-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Add shortcuts here
    map)
  "Keymap for `org-zettel-ref-minor-mode'.")

(define-minor-mode org-zettel-ref-minor-mode
  "Minor mode for org-zettel-ref buffers.
This mode indicates that the buffer is part of an org-zettel-ref pair."
  :init-value nil
  :lighter " Zettel"
  :keymap org-zettel-ref-minor-mode-map
  :group 'org-zettel-ref)

(defun org-zettel-ref-setup-buffers (source-buffer overview-buffer)
  "Setup SOURCE-BUFFER and OVERVIEW-BUFFER for org-zettel-ref."
  (with-current-buffer source-buffer
    (org-zettel-ref-minor-mode 1))
  (with-current-buffer overview-buffer
    (org-zettel-ref-minor-mode 1)
    (unless (eq major-mode 'org-mode)
      (org-mode))
    (setq buffer-read-only t)))



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

(defvar org-zettel-ref-db nil
  "The persistent database instance for org-zettel-ref.")

;;-------------------------
;; START: Overview File Management
;;-------------------------

(defun org-zettel-ref-create-overview-file (source-buffer target-file)
  "Create overview file for SOURCE-BUFFER at TARGET-FILE."
  (unless (file-exists-p target-file)
    (pcase org-zettel-ref-mode-type
      ('normal (org-zettel-ref-get-normal-overview source-buffer target-file))
      ('denote (org-zettel-ref-get-overview-file-denote source-buffer target-file))
      ('org-roam (org-zettel-ref-get-overview-file-org-roam source-buffer target-file))
      (_ (error "Unsupported org-zettel-ref-mode-type: %s" org-zettel-ref-mode-type))))
  target-file)

;;-------------------------
;; START: Buffer Management
;;-------------------------
(defun org-zettel-ref-get-overview-buffer-name (source-buffer)
  "Get the overview buffer name for the given SOURCE-BUFFER."
  (let* ((source-file-name (buffer-file-name source-buffer))
         (title (if source-file-name (file-name-base source-file-name) "Untitled")))
    ;; Buffer name does not include timestamp, ensuring consistency
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
          (let* ((overview-file org-zettel-ref-overview-file)
                 (overview-buffer org-zettel-ref-current-overview-buffer))
            (if (not (file-writable-p overview-file))
                (message "Error: Overview file is not writable: %s" overview-file)
              (with-current-buffer (or overview-buffer (find-file-noselect overview-file))
                (let ((inhibit-read-only t))
                  ;; Clean existing content
                  (org-zettel-ref-clean-sections (current-buffer))
                  ;; Insert new content
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
        ;; Clean content between * Quick Notes and * Marked Text
        (when (re-search-forward "^\\* Quick Notes\n" nil t)
          (let ((start (point)))
            (if (re-search-forward "^\\* Marked Text\n" nil t)
                (delete-region start (match-beginning 0))
              (delete-region start (point-max)))))
        ;; Clean content after * Marked Text
        (goto-char (point-min))
        (when (re-search-forward "^\\* Marked Text\n" nil t)
          (delete-region (point) (point-max)))
        ;; Ensure * Quick Notes and * Marked Text titles exist
        (goto-char (point-max))
        (unless (save-excursion (re-search-backward "^\\* Quick Notes\n" nil t))
          (insert "\n* Quick Notes\n"))
        (goto-char (point-max))
        (unless (save-excursion (re-search-backward "^\\* Marked Text\n" nil t))
          (insert "\n* Marked Text\n"))))))

;;----------------------------------------------------------------
;; File namming
;;----------------------------------------------------------------

(defun org-zettel-ref-generate-filename (title)
  "Generate overview file name based on TITLE."
  (let ((timestamp (format-time-string "%Y%m%dT%H%M%S")))
    (format "%s--%s%s" 
            timestamp
            title  ; Use original title directly
            org-zettel-ref-overview-file-suffix)))

;;----------------------------------------------------------------
;; org-zettel-ref-insert-quick-notes and marked-text
;;----------------------------------------------------------------  

(defun org-zettel-ref-insert-notes-and-text (source-buffer overview-buffer)
  "Insert quick notes and marked text from SOURCE-BUFFER into OVERVIEW-BUFFER."
  (with-current-buffer overview-buffer
    (let ((inhibit-read-only t)
          (quick-notes '())
          (existing-texts (org-zettel-ref-get-existing-marked-texts))
          (new-texts '()))
      
      ;; Collect quick notes and marked text from source buffer
      (with-current-buffer source-buffer
        ;; Collet quick notes and their place
        (org-element-map (org-element-parse-buffer) 'target
          (lambda (target)
            (let* ((begin (org-element-property :begin target))
                   (end (org-element-property :end target))
                   (content (buffer-substring-no-properties begin end)))
              (when (string-match "<<\\([^>]+\\)>>\\(.*\\)" content)
                (let ((note-name (match-string 1 content))
                      (note-content (string-trim (match-string 2 content))))
                  (push (cons note-name note-content) quick-notes)))))))
        
      ;; Collect marked text and their place
      (with-current-buffer source-buffer
        (org-element-map (org-element-parse-buffer) '(bold underline)
          (lambda (element)
            (let* ((begin (org-element-property :begin element))
                   (end (org-element-property :end element))
                   (raw-text (string-trim (buffer-substring-no-properties begin end))))
              (when (and raw-text (not (string-empty-p raw-text))
                         (not (member raw-text existing-texts)))
                (push raw-text new-texts))))))
      
      ;; Insert quick notes
      (when quick-notes
        ;; Jump to * Quick Notes section
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
      
      ;; Insert marked texts
      (when new-texts
        ;; Jump to * Marked Text section
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


;;-------------------------
;; END: Synchronization
;;-------------------------

;;-------------------------
;; START: Initialization
;;-------------------------

(defun org-zettel-ref-init ()
  "Initialize org-zettel-ref-mode."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (source-file (buffer-file-name source-buffer)))
    (unless source-file
      (user-error "Current buffer is not associated with a file"))
    
    (message "DEBUG: Starting initialization: %s" source-file)
  
    (let* ((entry-pair (org-zettel-ref-ensure-entry source-buffer))
           (ref-entry (car entry-pair))
           (overview-file (cdr entry-pair)))
      
      (when overview-file
        (let* ((overview-buffer-name (format "*Overview: %s*" 
                                           (file-name-base source-file)))
               (overview-buffer (org-zettel-ref-setup-overview-window 
                               overview-file 
                               overview-buffer-name)))
          (org-zettel-ref-setup-buffers source-buffer overview-buffer)
          (setq org-zettel-ref-current-overview-buffer overview-buffer)
          (setq org-zettel-ref-overview-file overview-file))))))

(defun org-zettel-ref-ensure-entry (source-buffer)
  "Ensure source-buffer has corresponding reference and overview entries.
Return (ref-entry . overview-file) pair."
  (let* ((source-file (buffer-file-name source-buffer))
         (db (org-zettel-ref-ensure-db))
         (ref-entry (org-zettel-ref-db-ensure-ref-entry db source-file))
         (ref-id (org-zettel-ref-ref-entry-id ref-entry))
         (overview-id (org-zettel-ref-db-get-maps db ref-id))
         overview-file)
    (setq overview-file
          (if overview-id
              (when-let* ((existing-overview (org-zettel-ref-db-get-overview-by-ref-id db ref-id))  
                         (file-path (org-zettel-ref-overview-entry-file-path existing-overview)))
                (when (file-exists-p file-path)
                  file-path))
            (let* ((title (org-zettel-ref-ref-entry-title ref-entry))
                   (overview-filename (org-zettel-ref-generate-filename title))
                   (target-file (expand-file-name overview-filename org-zettel-ref-overview-directory))
                   (new-file (org-zettel-ref-create-overview-file source-buffer target-file))
                   (new-entry (org-zettel-ref-db-create-overview-entry 
                             db
                             ref-id 
                             new-file
                             title)))
              (org-zettel-ref-db-add-overview-entry db new-entry)
              (org-zettel-ref-db-add-map db ref-id (org-zettel-ref-overview-entry-id new-entry))
              new-file)))
    (org-zettel-ref-db-save db)
    (cons ref-entry overview-file)))

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
         (id (file-name-sans-extension 
              (file-name-nondirectory 
               (org-zettel-ref-generate-filename title)))))
    (unless (file-exists-p overview-file)
      (with-temp-file overview-file
        (insert (format "#+TITLE: Overview - %s\n#+SOURCE_FILE: %s\n#+filetags: :overview:\n#+startup: showall\n\n* Quick Notes\n\n* Marked Text\n" 
                       title source-file))))
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

(defun org-zettel-ref-get-overview-file-denote (source-buffer overview-file)
  "Get or create an overview file for SOURCE-BUFFER using Denote mode."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (file-name-base source-file)))
    (unless (file-exists-p overview-file)
      (with-temp-file overview-file
        (insert (format "#+title: Overview - %s\n#+startup: showall\n#+filetags: :overview:\n\n* Quick Notes\n\n\n* Marked Text\n" title))))
    overview-file))

;;----------------------------------------------------------------------------
;; Hooks
;;----------------------------------------------------------------------------

(defmacro org-zettel-ref-with-transaction (&rest body)
  "Execute operations within a database transaction."
  `(let ((db (org-zettel-ref-ensure-db)))
     (unwind-protect
         (progn ,@body
                (org-zettel-ref-db-save db org-zettel-ref-db-file))
       (setf (org-zettel-ref-db-dirty db) t))))

(defun org-zettel-ref-db--around-dired-do-rename (orig-fun &optional arg)
  "Handle database updates during dired rename.
ORIG-FUN is the original `dired-do-rename' function.
ARG is the prefix argument."
  (let* ((marked-files (dired-get-marked-files))
         (db (org-zettel-ref-ensure-db))
         (ref-dir (expand-file-name org-zettel-ref-directory))
         (overview-dir (expand-file-name org-zettel-ref-overview-directory)))
    
    ;; Execute the original function
    (funcall orig-fun arg)
    
    ;; Get the new list of files
    (let ((new-files (dired-get-marked-files)))
      ;; Process each renamed file
      (cl-loop for old-file in marked-files
               for new-file in new-files
               do (progn
                    ;; Process only files in the ref directory
                    (when (string-prefix-p ref-dir (expand-file-name old-file))
                      (when-let* ((ref-id (gethash old-file (org-zettel-ref-db-ref-paths db))))
                        (condition-case err
                            (org-zettel-ref-with-transaction
                             (when-let* ((ref-entry (gethash ref-id (org-zettel-ref-db-refs db))))
                               ;; Update file path
                               (setf (org-zettel-ref-ref-entry-file-path ref-entry) new-file)
                               ;; Update path mapping
                               (remhash old-file (org-zettel-ref-db-ref-paths db))
                               (puthash new-file ref-id (org-zettel-ref-db-ref-paths db))
                               ;; Update entry
                               (org-zettel-ref-db-update-ref-entry db ref-entry)
                               (message "Updated database for ref file rename: %s -> %s" 
                                      old-file new-file)))
                          (error
                           (message "Failed to update database for ref file %s -> %s: %s"
                                   old-file new-file (error-message-string err))))))
                    
                    ;; Process only files in the overview directory
                    (when (string-prefix-p overview-dir (expand-file-name old-file))
                      (when-let* ((overview-id (gethash old-file (org-zettel-ref-db-overview-paths db))))
                        (condition-case err
                            (org-zettel-ref-with-transaction
                             (when-let* ((overview-entry (gethash overview-id 
                                                               (org-zettel-ref-db-overviews db))))
                               ;; Update file path
                               (setf (org-zettel-ref-overview-entry-file-path overview-entry) new-file)
                               ;; Update path mapping
                               (remhash old-file (org-zettel-ref-db-overview-paths db))
                               (puthash new-file overview-id (org-zettel-ref-db-overview-paths db))
                               ;; Save changes
                               (org-zettel-ref-db-save db org-zettel-ref-db-file)
                               (message "Updated database for overview file rename: %s -> %s" 
                                      old-file new-file)))
                          (error
                           (message "Failed to update database for overview file %s -> %s: %s"
                                   old-file new-file (error-message-string err)))))))))))

(defun org-zettel-ref-db--around-dired-do-delete (orig-fun &optional arg)
  "Handle database updates during dired delete.
ORIG-FUN is the original `dired-do-delete' function.
ARG is the prefix argument."
  (let* ((marked-files (dired-get-marked-files))
         (db (org-zettel-ref-ensure-db))
         (ref-dir (expand-file-name org-zettel-ref-directory))
         (overview-dir (expand-file-name org-zettel-ref-overview-directory)))
    
    ;; Check and process database first
    (dolist (file marked-files)
      (let ((full-path (expand-file-name file)))
        (cond
         ;; Process reference files
         ((string-prefix-p ref-dir full-path)
          (when-let* ((ref-id (gethash full-path (org-zettel-ref-db-ref-paths db))))
            (condition-case err
                (org-zettel-ref-with-transaction
                 ;; Delete reference entry
                 (remhash ref-id (org-zettel-ref-db-refs db))
                 ;; Delete path mapping
                 (remhash full-path (org-zettel-ref-db-ref-paths db))
                 ;; Delete related overview mapping
                 (when-let* ((overview-id (gethash ref-id (org-zettel-ref-db-map db))))
                   (remhash ref-id (org-zettel-ref-db-map db))
                   (message "Removed ref entry and related mappings for: %s" file))
                 ;; Set database to modified state
                 (setf (org-zettel-ref-db-modified db) (current-time)
                       (org-zettel-ref-db-dirty db) t))
              (error
               (message "Failed to update database for deleted ref file %s: %s"
                       file (error-message-string err))))))
         
         ;; Process overview files
         ((string-prefix-p overview-dir full-path)
          (when-let* ((overview-id (gethash full-path (org-zettel-ref-db-overview-paths db))))
            (condition-case err
                (org-zettel-ref-with-transaction
                 ;; Delete overview entry
                 (remhash overview-id (org-zettel-ref-db-overviews db))
                 ;; Delete path mapping
                 (remhash full-path (org-zettel-ref-db-overview-paths db))
                 ;; Delete related reference mapping
                 (maphash (lambda (ref-id mapped-overview-id)
                           (when (equal mapped-overview-id overview-id)
                             (remhash ref-id (org-zettel-ref-db-map db))))
                         (org-zettel-ref-db-map db))
                 (message "Removed overview entry and related mappings for: %s" file)
                 ;; Set database to modified state
                 (setf (org-zettel-ref-db-modified db) (current-time)
                       (org-zettel-ref-db-dirty db) t))
              (error
               (message "Failed to update database for deleted overview file %s: %s"
                       file (error-message-string err)))))))))
    
    ;; Execute the original delete function
    (funcall orig-fun arg)
    
    ;; Save database
    (org-zettel-ref-db-save db)))

;; Rename handling
(advice-remove 'dired-do-rename #'org-zettel-ref-db--around-dired-do-rename)
(advice-add 'dired-do-rename :around #'org-zettel-ref-db--around-dired-do-rename)

;; Delete handling
(advice-remove 'dired-do-delete #'org-zettel-ref-db--around-dired-do-delete)
(advice-add 'dired-do-delete :around #'org-zettel-ref-db--around-dired-do-delete)


(provide 'org-zettel-ref-core)

;;; org-zettel-ref-core.el ends here
