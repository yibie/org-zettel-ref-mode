;;; org-zettel-ref-core.el --- Core functionality for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains core functionality for org-zettel-ref.

;;; Code:

(require 'org)
(require 'org-element)

(require 'org-zettel-ref-org-roam)
(require 'org-zettel-ref-denote)
(require 'org-zettel-ref-normal)

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

(defcustom org-zettel-ref-overview-file-suffix "__overview"
  "Suffix to be added to overview files created by org-zettel-ref in Denote mode.
This suffix will be appended to the filename before the file extension."
  :type 'string
  :group 'org-zettel-ref)

(defvar org-zettel-ref-overview-file nil
  "The current overview file being used.")

(defvar org-zettel-ref-current-overviee-buffer nil
  "The current overview buffer being used.")

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
          (rename-buffer overview-buffer-name t)
          (setq org-zettel-ref-overview-file overview-file)
          (setq org-zettel-ref-current-overview-buffer (buffer-name))
          (org-zettel-ref-setup-buffers source-buffer (current-buffer))
          (message "Debug: org-zettel-ref-init completed")
          (run-with-timer 0.5 nil #'org-zettel-ref-update-roam-db overview-file))))))

(defun org-zettel-ref-get-overview-file (source-buffer)
  "Get or create an overview file for SOURCE-BUFFER."
  (let ((file-path
         (cond
          ((eq org-zettel-ref-mode-type 'org-roam)
           (org-zettel-ref-get-overview-file-org-roam source-buffer))
          ((eq org-zettel-ref-mode-type 'denote)
           (org-zettel-ref-get-overview-file-denote source-buffer))
          (t
           (org-zettel-ref-get-overview-file-normal source-buffer)))))
    (unless (file-exists-p file-path)
      (error "Failed to create or find overview file"))
    file-path))

(defun org-zettel-ref-setup-buffers (source-buffer overview-buffer)
  "Set up the source and overview buffers with appropriate modes and configurations."
  (with-current-buffer source-buffer
    (org-mode))
  (with-current-buffer overview-buffer
    (org-mode)
    (setq buffer-read-only t))
 ; (other-window 1)
 )

(defun org-zettel-ref-sync-overview ()
  "Synchronize the quick notes and marked text from the source buffer to the overview file."
  (interactive)
  (let* ((source-buffer (or (buffer-base-buffer) (current-buffer)))
         (overview-file (org-zettel-ref-get-overview-file source-buffer))
         (overview-buffer (or (get-buffer org-zettel-ref-current-overview-buffer)
                              (find-file-noselect overview-file))))
    (message "Debug: Syncing overview file: %s" overview-file)
    (with-current-buffer overview-buffer
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (when (re-search-forward "^\\* Quick Notes" nil t)
          (delete-region (point) (point-max))
          (insert "\n")
          (org-zettel-ref-insert-quick-notes source-buffer overview-buffer)
          (insert "\n* Marked Text\n\n")
          (org-zettel-ref-insert-marked-text source-buffer overview-buffer)
          (save-buffer)))
    (message "Debug: Sync completed"))))

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

(provide 'org-zettel-ref-core)

;;; org-zettel-ref-core.el ends here
