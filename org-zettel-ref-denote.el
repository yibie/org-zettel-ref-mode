;;; org-zettel-ref-denote.el --- Denote integration for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains denote integration for org-zettel-ref.

;;; Code:

(require 'denote)

(defun org-zettel-ref-get-overview-file-denote (source-buffer)
  "Get or create an overview file for SOURCE-BUFFER using denote API."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (format "Overview of %s" (file-name-base source-file)))
         (date (format-time-string "[%Y-%m-%d %a %H:%M]"))
         (id (format-time-string "%Y%m%dT%H%M%S"))
         (file-path (expand-file-name (concat id "--" (replace-regexp-in-string "[^a-zA-Z0-9]" "-" title) ".org")
                                      org-zettel-ref-overview-directory)))
    (unless (file-exists-p file-path)
      (with-temp-file file-path
        (insert (format "#+title:      %s\n#+date:       %s\n#+filetags:   \n#+identifier: %s\n\n* Overview\n\n* Quick Notes\n\n* Marked Text\n"
                        title date id))))
    file-path))

(provide 'org-zettel-ref-denote)

;;; org-zettel-ref-denote.el ends here