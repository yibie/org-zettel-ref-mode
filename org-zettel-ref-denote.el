;;; org-zettel-ref-denote.el --- Denote integration for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains denote integration for org-zettel-ref.

;;; Code:
(when (eq org-zettel-ref-mode-type 'denote)
  (require 'denote))

(defun org-zettel-ref-get-overview-file-denote (source-buffer)
  "Get or create an overview file for SOURCE-BUFFER using denote API."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (format "Overview - %s" (file-name-base source-file)))
         (keywords '("overview"))
         (file-path (denote-directory)))
    (unless (file-exists-p file-path)
      (let ((new-file (denote title :keywords keywords :subdirectory org-zettel-ref-overview-directory)))
        (with-current-buffer (find-file-noselect new-file)
          (goto-char (point-max))
          (insert "* Quick Notes\n\n* Marked Text\n")
          (save-buffer)
          (kill-buffer))
        (setq file-path new-file)))
    ;; Add the title to denote-title-history
    (add-to-history 'denote-title-history title)
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
