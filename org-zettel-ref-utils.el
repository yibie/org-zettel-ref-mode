;;; org-zettel-ref-utils.el --- Utility functions for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains utility functions for org-zettel-ref.

;;; Code:

(require 'org-zettel-ref-core)

(defun org-zettel-ref-generate-filename (title)
  "Generate a filename based on TITLE and current mode type."
  (let ((sanitized-title (replace-regexp-in-string "\\s-" "-" (replace-regexp-in-string "[/\\:*?\"<>|]" "" title))))
    (cond
     ((eq org-zettel-ref-mode-type 'org-roam)
      (concat (org-roam-node-slug (org-roam-node-create :title title)) ".org"))
     ((eq org-zettel-ref-mode-type 'denote)
      (concat (format-time-string "%Y%m%dT%H%M%S") "--" sanitized-title ".org"))
     (t
      (concat sanitized-title ".org")))))

(defun org-zettel-ref-get-overview-buffer-name (source-buffer)
  "Get the name of the overview buffer for the given source buffer."
  (let* ((source-file-name (buffer-file-name source-buffer))
         (overview-file-name (org-zettel-ref-generate-filename
                              (file-name-base source-file-name)))
         (buffer-name (format "*Org Zettel Ref: %s*" overview-file-name)))
    (generate-new-buffer-name buffer-name)))

(defun org-zettel-ref-ensure-overview-buffer ()
  "Ensure that the overview buffer exists, creating it if necessary."
  (unless (and org-zettel-ref-current-overview-buffer
               (get-buffer org-zettel-ref-current-overview-buffer))
    (org-zettel-ref-init)))

(provide 'org-zettel-ref-utils)

;;; org-zettel-ref-utils.el ends here