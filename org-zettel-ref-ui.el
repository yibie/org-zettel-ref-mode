;;; org-zettel-ref-ui.el --- User interface for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains user interface related functions for org-zettel-ref.

;;; Code:

(require 'org-zettel-ref-core)

(defun org-zettel-ref-add-quick-note ()
  "Add a quick note to the current buffer."
  (interactive)
  (let* ((note-name (read-string "Enter note: ")))
    ;; Source buffer insert note
    (insert (format "<<%s>>" note-name))
    ;; Sync to overview file
    (org-zettel-ref-sync-overview)))

(defun org-zettel-ref-quick-markup ()
  "Quickly apply org-mode markup to the region or insert at point, adding spaces for Chinese text."
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
        (let* ((text (buffer-substring-no-properties beg end))
               (chinese-p (string-match-p "\\cC" text))
               (space-before (if (and chinese-p (not (string-match-p "^\\s-" text))) " " ""))
               (space-after (if (and chinese-p (not (string-match-p "\\s-$" text))) " " "")))
          (goto-char end)
          (insert marker space-after)
          (goto-char beg)
          (insert space-before marker))
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
  "Remove <<target>>, *Bold*, and _Underline_ markers from the current buffer, preserving Org-mode structure and links."
  (interactive)
  (let* ((case-fold-search nil)
         (pre (car org-emphasis-regexp-components))
         (post (nth 1 org-emphasis-regexp-components))
         (border (nth 2 org-emphasis-regexp-components))
         (body-regexp (nth 3 org-emphasis-regexp-components))
         (emphasis-regexp (concat "\\([" pre "]\\|^\\)"
                                  "\\(\\([*_]\\)\\("
                                  "[^" border "\n]\\|"
                                  "[^" border "\n]*"
                                  "[^" border "\n]\\)"
                                  "\\3\\)"
                                  "\\([" post "]\\|$\\)")))
    ;; Remove <<target>> markers
    (goto-char (point-min))
    (while (re-search-forward "<<\\([^>]+\\)>>" nil t)
      (replace-match "\\1"))
    
    ;; Remove *Bold* and _Underline_ markers
    (goto-char (point-min))
    (while (re-search-forward emphasis-regexp nil t)
      (replace-match "\\1\\4\\5"))
    
    (message "All <<target>>, *Bold*, and _Underline_ markers have been removed while preserving Org-mode structure and links.")))

(defun org-zettel-ref-clean-targets-and-sync ()
  "Clean all markup from the current buffer and then sync the overview."
  (interactive)
  (org-zettel-ref-clean-multiple-targets)
  (save-buffer)
  (when (fboundp 'org-zettel-ref-sync-overview)
    (org-zettel-ref-sync-overview)))

(provide 'org-zettel-ref-ui)

;;; org-zettel-ref-ui.el ends here
