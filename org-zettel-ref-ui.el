;;; org-zettel-ref-ui.el --- User interface for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains user interface related functions for org-zettel-ref.

;;; Code:

(require 'org-zettel-ref-core)



(defun org-zettel-ref-add-quick-note ()
  "Add a quick note to the current buffer."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (note-name (read-string "Enter note: ")))
    ;; Source buffer insert note
    (insert (format "<<%s>>" note-name))
    ;; Sync to overview file
    (org-zettel-ref-sync-overview)))

(defun org-zettel-ref-quick-markup ()
  "Quickly apply org-mode markup to the region or insert at point."
  (interactive)
  (let* ((markup-types '(("Bold" . "*")
                         ("Italic" . "/")
                         ("Underline" . "_")
                         ("Code" . "~")
                         ("Verbatim" . "=")
                         ("Strikethrough" . "+")))
         (markup (completing-read "Choose markup: " markup-types nil t))
         (marker (cdr (assoc markup markup-types)))
         (region-active (use-region-p))
         (beg (if region-active (region-beginning) (point)))
         (end (if region-active (region-end) (point))))
    (if region-active
        (progn
          (goto-char end)
          (insert marker)
          (goto-char beg)
          (insert marker))
      (insert marker marker)
      (backward-char))))


(defcustom org-zettel-ref-quick-markup-key "C-c m"
  "Key binding for quick markup function in org-zettel-ref-mode.
This should be a string that can be passed to `kbd'."
  :type 'string
  :group 'org-zettel-ref)

(defun org-zettel-ref-setup-quick-markup ()
  "Set up the key binding for quick markup."
  (local-set-key (kbd org-zettel-ref-quick-markup-key) 'org-zettel-ref-quick-markup))

(defun org-zettel-ref-clean-multiple-targets ()
  "Remove <<target>>, *, =, ~, /, and _ markers from the current buffer, preserving Org-mode links."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to remove all <<target>>, *, =, ~, /, and _ markers? This cannot be undone. ")
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      ;; Remove <<target>> markers
      (setq content (replace-regexp-in-string "<<\\([^>]+\\)>>" "" content))
      
      ;; Remove formatting markers outside of Org-mode links
      (setq content
            (replace-regexp-in-string
             "\\(\\[\\[[^]]*\\]\\[.*?\\]\\]\\)\\|\\([*=~/_]\\)\\([^*=~/_\n]+?\\)\\2"
             (lambda (match)
               (if (match-string 1 match)
                   ;; If it's an Org-mode link, leave it unchanged
                   match
                 ;; Otherwise, remove the formatting
                 (match-string 3 match)))
             content
             t  ; FIXEDCASE
             nil  ; LITERAL
             ))
      
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content))
      (message "All <<target>>, *, =, ~, /, and _ markers have been removed while preserving Org-mode links."))))

(defun org-zettel-ref-clean-targets-and-sync ()
  "Clean all markup from the current buffer and then sync the overview."
  (interactive)
  (org-zettel-ref-clean-multiple-targets)
  (save-buffer)
  (when (fboundp 'org-zettel-ref-sync-overview)
    (org-zettel-ref-sync-overview)))

(provide 'org-zettel-ref-ui)

;;; org-zettel-ref-ui.el ends here