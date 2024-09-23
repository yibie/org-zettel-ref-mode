;;; org-zettel-ref-org-roam.el --- Org-roam integration for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains org-roam integration for org-zettel-ref.

;;; Code:

(require 'org-zettel-ref-core)

(declare-function org-roam-node-create "ext:org-roam" t)
(declare-function org-roam-node-slug "ext:org-roam" t)

(defvar org-zettel-ref-mode-type 'org-roam 
  "The type of mode to use for org-zettel-ref.
Can be 'normal, 'denote, or 'org-roam.")



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
    (when (fboundp 'org-roam-db-update-file)
      (org-roam-db-update-file file-path))
    file-path))
  
(when (eq org-zettel-ref-mode-type 'org-roam)
  (require 'org-roam))

(provide 'org-zettel-ref-org-roam)

;;; org-zettel-ref-org-roam.el ends here
