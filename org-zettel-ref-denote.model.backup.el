;;; org-zettel-ref-denote.el --- Denote integration for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains denote integration for org-zettel-ref.

;;; Code:

;; 预声明来自 org-zettel-ref-core 的函数和变量
(declare-function org-zettel-ref-update-index "org-zettel-ref-core")
(declare-function org-zettel-ref-sanitize-filename "org-zettel-ref-core")
(defvar org-zettel-ref-overview-directory)
(defvar org-zettel-ref-mode-type)
(defvar org-zettel-ref-overview-file-suffix)

(require 'denote nil t)  ; 安全地尝试加载 denote

(defun org-zettel-ref-denote-title (title)
  "Generate a title for Denote by directly using the source file name."
  (org-zettel-ref-sanitize-filename title))

(defun org-zettel-ref-denote-slug-title (title)
  "Generate a slug from TITLE for Denote."
  (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" (downcase title)))

(defun org-zettel-ref-find-denote-overview-file (slug-title)
  "Find an existing Denote overview file matching SLUG-TITLE."
  (let ((files (directory-files org-zettel-ref-overview-directory t "__overview\\.org$")))
    (cl-find-if
     (lambda (file)
       (string-match-p (regexp-quote slug-title) (file-name-nondirectory file)))
     files)))

(defun org-zettel-ref-get-overview-file-denote (source-buffer)
  "Get or create an overview file for SOURCE-BUFFER using denote mode."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (format "Overview - %s" (file-name-base source-file)))
         (subdir (file-name-as-directory org-zettel-ref-overview-directory))
         (overview-file (expand-file-name (concat (file-name-base source-file) org-zettel-ref-overview-file-suffix) subdir)))
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


;; Load more functions from org-zettel-ref-core

;(with-eval-after-load 'org-zettel-ref-core
;)

(provide 'org-zettel-ref-denote)


;;; org-zettel-ref-denote.el ends here