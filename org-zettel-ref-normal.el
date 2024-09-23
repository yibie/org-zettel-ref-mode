;;; org-zettel-ref-normal.el --- Normal mode functionality for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains normal mode functionality for org-zettel-ref.

;;; Code:

(defun org-zettel-ref-get-overview-file-normal (source-buffer)
  "Get or create an overview file for SOURCE-BUFFER using normal mode."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (format "Overview of %s" (file-name-base source-file)))
         (file-path (expand-file-name (concat (replace-regexp-in-string "[^a-zA-Z0-9]" "-" title) (or org-zettel-ref-overview-file-suffix ".org"))
                                      org-zettel-ref-overview-directory)))
    (unless (file-exists-p file-path)
      (with-temp-file file-path
        (insert (format "#+title: %s\n\n* Overview\n\n* Quick Notes\n\n* Marked Text\n" title))))
    file-path))

(provide 'org-zettel-ref-normal)

;;; org-zettel-ref-normal.el ends here