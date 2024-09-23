;;; org-zettel-ref-normal.el --- Normal mode functionality for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains normal mode functionality for org-zettel-ref.

;;; Code:

(defun org-zettel-ref-get-overview-file-normal (source-buffer)
  "Get or create an overview file for SOURCE-BUFFER using normal mode."
  (let* ((source-file (buffer-file-name source-buffer))
         (base-name (file-name-base source-file))
         (title (format "Overview - %s" base-name))
         (sanitized-name (replace-regexp-in-string "[^a-zA-Z0-9\u4e00-\u9fa5]" "-" base-name))
         (file-name (concat sanitized-name "-overview.org"))
         (file-path (expand-file-name file-name org-zettel-ref-overview-directory)))
    (unless source-file
      (error "Source buffer is not associated with a file"))
    (unless (file-exists-p file-path)
      (with-temp-file file-path
        (insert (format "#+title: %s\n#+filetags: :overview:\n\n* Overview\n\n* Quick Notes\n\n* Marked Text\n" title))))
    (message "Debug: Normal file-path is %s" file-path)
    file-path))
(provide 'org-zettel-ref-normal)

;;; org-zettel-ref-normal.el ends here