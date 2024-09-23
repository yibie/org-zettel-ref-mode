;;; org-zettel-ref-denote.el --- Denote integration for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains denote integration for org-zettel-ref.

;;; Code:
(when (eq org-zettel-ref-mode-type 'denote)
  (require 'denote))

(defun org-zettel-ref-get-overview-file-denote (source-buffer)
  "Get or create an overview file for SOURCE-BUFFER using denote API."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (format "Overview of %s" (file-name-base source-file)))
         (date (format-time-string "[%Y-%m-%d %a %H:%M]"))
         (id (format-time-string "%Y%m%dT%H%M%S"))
         ;; Build the path for the overview file
         ;; 1. Use `concat` to join ID, separator "--", and processed title
         ;; 2. Replace illegal filename characters (like /\:*?"<>|) with "-", preserve Chinese and other legal characters
         ;; 3. Add ".org" extension
         ;; 4. Use `expand-file-name` to convert relative path to absolute path
         (file-path (expand-file-name (concat id "--" (replace-regexp-in-string "[/\\:*?\"<>|]" "-" title) (or org-zettel-ref-overview-file-suffix "__overview.org"))
                                      org-zettel-ref-overview-directory)))
    (unless (file-exists-p file-path)
      (with-temp-file file-path
        (insert (format "#+title:      %s\n#+date:       %s\n#+filetags:   \n#+identifier: %s\n\n* Overview\n\n* Quick Notes\n\n* Marked Text\n"
                        title date id))))
    file-path))

(defun org-zettel-ref-get-overview-file-denote-path (source-buffer)
  "Get the path for the overview file for SOURCE-BUFFER using denote API."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (format "Overview of %s" (file-name-base source-file)))
         (date (format-time-string "[%Y-%m-%d %a %H:%M]"))
         (id (format-time-string "%Y%m%dT%H%M%S"))
         (file-path (expand-file-name (concat id "--" (replace-regexp-in-string "[/\\:*?\"<>|]" "-" title) (or org-zettel-ref-overview-file-suffix "__overview.org"))
                                      org-zettel-ref-overview-directory))))
    file-path)




(provide 'org-zettel-ref-denote)

;;; org-zettel-ref-denote.el ends here
