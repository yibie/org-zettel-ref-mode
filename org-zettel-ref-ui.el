;;; org-zettel-ref-ui.el --- User interface for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains user interface related functions for org-zettel-ref.

;;; Code:

(defun org-zettel-ref-quick-markup ()
  "Quickly apply org-mode markup to the region or insert at point."
  (interactive)
  (let* ((markup-types '(("Bold" . "*")
                         ("Italic" . "/")
                         ("Underline" . "_")
                         ("Code" . "~")
                         ("Verbatim" . "=")
                         ("Strikethrough" . "+")
                         ("Quick Note" . "<<")))
         (markup (completing-read "Choose markup: " markup-types nil t))
         (marker (cdr (assoc markup markup-types)))
         (is-quick-note (string= marker "<<"))
         (region-active (use-region-p))
         (beg (if region-active (region-beginning) (point)))
         (end (if region-active (region-end) (point))))

    (if is-quick-note
        (let ((note-name (read-string "Enter quick note name: ")))
          (if region-active
              (let ((content (buffer-substring-no-properties beg end)))
                (delete-region beg end)
                (insert (format "<<%s>> %s" note-name content)))
            (insert (format "<<%s>> " note-name))))
      (if region-active
          (progn
            (goto-char end)
            (insert marker)
            (goto-char beg)
            (insert marker))
        (insert marker marker)
        (backward-char)))))

(defcustom org-zettel-ref-quick-markup-key "C-c m"
  "Key binding for quick markup function in org-zettel-ref-mode.
This should be a string that can be passed to `kbd'."
  :type 'string
  :group 'org-zettel-ref)

(defun org-zettel-ref-setup-quick-markup ()
  "Set up the key binding for quick markup."
  (local-set-key (kbd org-zettel-ref-quick-markup-key) 'org-zettel-ref-quick-markup))

(defun org-zettel-ref-clean-multiple-targets ()
  "Remove all <<target>>, **, and == markers from the current buffer after confirmation."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to remove all <<target>>, **, and == markers? This cannot be undone. ")
    (save-excursion
      (goto-char (point-min))
      ;; Remove <<target>> markers
      (while (re-search-forward "<<\\([^>]+\\)>>" nil t)
        (replace-match "")
        (just-one-space))
      ;; Remove ** markers
      (goto-char (point-min))
      (while (re-search-forward "\\*\\*\\([^*]+\\)\\*\\*" nil t)
        (replace-match "\\1")
        (just-one-space))
      ;; Remove == markers
      (goto-char (point-min))
      (while (re-search-forward "==\\([^=]+\\)==" nil t)
        (replace-match "\\1")
        (just-one-space)))
    (message "All <<target>>, **, and == markers have been removed.")))

(defun org-zettel-ref-clean-targets-and-sync ()
  "Clean all <<target>> markers from the current buffer and then sync the overview."
  (interactive)
  (org-zettel-ref-clean-targets)
  (save-buffer)
  (org-zettel-ref-sync-overview))

(provide 'org-zettel-ref-ui)

;;; org-zettel-ref-ui.el ends here