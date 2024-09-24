;;; org-zettel-ref-normal.el --- Normal mode functionality for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains normal mode functionality for org-zettel-ref.

;;; Code:

(defun org-zettel-ref-get-normal-overview (file-path source-buffer source-file)
  "Create a normal overview file at FILE-PATH for SOURCE-BUFFER with SOURCE-FILE."
  (let* ((base-name (file-name-base source-file))
         (title (format "Overview - %s" base-name))
         (generated-filename (org-zettel-ref-generate-filename title))
         (overview-file-path (expand-file-name generated-filename org-zettel-ref-overview-directory)))
    (unless (file-exists-p overview-file-path)
      (with-temp-file overview-file-path
        (insert (format "#+title: %s\n" title))
        (insert (format "#+SOURCE_FILE: %s\n" source-file))
        (insert "#+filetags: :overview:\n\n")
        (insert "* Quick Notes\n\n* Marked Text\n")))
    (message "Debug: Normal file-path is %s" overview-file-path)
    overview-file-path))


(provide 'org-zettel-ref-normal)

;;; org-zettel-ref-normal.el ends here