;;; org-zettel-ref-denote.el --- Denote integration for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains denote integration for org-zettel-ref.

;;; Code:
(when (eq org-zettel-ref-mode-type 'denote)
  (require 'denote))

(defun org-zettel-ref-get-overview-file-denote (source-buffer)
  "Get or create an overview file for SOURCE-BUFFER using denote mode."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (format "Overview - %s" (file-name-base source-file)))
         (subdir (file-name-as-directory org-zettel-ref-overview-directory))
         (overview-file (expand-file-name (concat (file-name-base source-file) ollile-suffix) subdir)))
    (unless source-file
      (error "Source buffer is not associated with a file"))
    (unless (and (boundp 'org-zettel-ref-overview-directory)
                 (stringp org-zettel-ref-overview-directory)
                 (file-directory-p org-zettel-ref-overview-directory))
      (error "org-zettel-ref-overview-directory is not properly defined or does not exist"))
    (unless (file-exists-p subdir)
      (make-directory subdir t)
      (message "Created subdirectory: %s" subdir))
    (unless (file-exists-p overview-file)
      (with-temp-file overview-file
        (insert (format "#+title: %s\n" title))
        (insert (format "#+date: %s\n" (format-time-string "%Y-%m-%d %H:%M")))
        (insert "#+filetags: :overview:\n")
        (insert (format "#+SOURCE_FILE: %s\n\n" source-file))
        (insert "* Quick Notes\n\n* Marked Text\n")))
    ;; Add the title to denote-title-history
    (add-to-history 'denote-title-history title)
    ;; Update the index
    (org-zettel-ref-update-index source-file overview-file)
    (message "Returning new file path: %s" overview-file)
    overview-file))

(defun org-zettel-ref-get-overview-file-denote-path (source-buffer)
  "Get the path for the overview file for SOURCE-BUFFER using denote API."
  (org-zettel-ref-get-overview-file-denote source-buffer))

(provide 'org-zettel-ref-denote)

;;; org-zettel-ref-denote.el ends here
