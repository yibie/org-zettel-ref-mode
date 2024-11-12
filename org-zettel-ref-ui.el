;;; org-zettel-ref-ui.el --- User interface for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains user interface related functions for org-zettel-ref.

;;; Code:

(require 'org-zettel-ref-core)

(defun org-zettel-ref-add-quick-note ()
  "使用高亮系统添加快速笔记."
  (interactive)
  (let* ((note-text (read-string "输入笔记内容: "))
         (highlight-id (org-zettel-ref-highlight-generate-id)))
    (insert (format "<<hl-%s>> §n{%s}"
                   highlight-id
                   note-text))
    (org-zettel-ref-highlight-refresh)
    (org-zettel-ref-sync-highlights)))

(defun org-zettel-ref-quick-markup ()
  "使用高亮系统快速标记文本."
  (interactive)
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (text (buffer-substring-no-properties beg end))
             (type (completing-read "选择标记类型: "
                                  (mapcar #'car org-zettel-ref-highlight-types)
                                  nil t))
             (highlight-id (org-zettel-ref-highlight-generate-id)))
        (delete-region beg end)
        (insert (format "<<hl-%s>> §%s{%s}"
                       highlight-id
                       type
                       text))
        (org-zettel-ref-highlight-refresh)
        (org-zettel-ref-sync-highlights))
    (message "请先选择要标记的文本")))

(defcustom org-zettel-ref-quick-markup-key "C-c m"
  "Key binding for quick markup function in org-zettel-ref-mode.
This should be a string that can be passed to `kbd'."
  :type 'string
  :group 'org-zettel-ref)

(defun org-zettel-ref-setup-quick-markup ()
  "Set up the key binding for quick markup."
  (local-set-key (kbd org-zettel-ref-quick-markup-key) 'org-zettel-ref-quick-markup))

(defun org-zettel-ref-clean-multiple-targets ()
  "Remove various markup and unnecessary elements from the current buffer.
Cleans:
1. <<target>> markers completely (including empty ones)
2. #+begin_html...#+end_html blocks
3. :PROPERTIES: blocks under headers
4. *Bold* and _Underline_ markers (preserving heading stars)
5. Trailing backslashes at end of lines"
  (interactive)
  (let* ((case-fold-search nil)
         (pre (car org-emphasis-regexp-components))
         (post (nth 1 org-emphasis-regexp-components))
         (border (nth 2 org-emphasis-regexp-components))
         (emphasis-regexp (concat "\\([" pre "]\\|^\\)"
                                "\\(\\([*_]\\)"
                                "\\([^*\n]\\|"
                                "[^*\n]*"
                                "[^*\n]\\)"
                                "\\3\\)"
                                "\\([" post "]\\|$\\)")))
    ;; Remove <<target>> markers completely (including empty ones)
    (goto-char (point-min))
    (while (re-search-forward "<<[^>]*>>" nil t)
      (replace-match ""))
    
    ;; Remove #+begin_html...#+end_html blocks
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^[ \t]*#\\+begin_html[ \t]*\n\\(\\(?:[^\n]*\n\\)*?\\)[ \t]*#\\+end_html[ \t]*\n" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+begin_html[ \t]*\n" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+end_html[ \t]*\n" nil t)
        (replace-match "")))
    
    ;; Remove :PROPERTIES: blocks
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*:PROPERTIES:\n\\(?:^[ \t]*:\\(?:\\w\\|-\\)+:.*\n\\)*^[ \t]*:END:\n" nil t)
      (replace-match ""))
    
    ;; Remove *Bold* and _Underline_ markers, but preserve heading stars
    (goto-char (point-min))
    (while (re-search-forward emphasis-regexp nil t)
      (unless (save-match-data
                (string-match-p "^\\*+[ \t]" 
                              (buffer-substring (line-beginning-position) 
                                              (match-beginning 0))))
        (replace-match "\\1\\4\\5")))
    
    ;; Remove trailing backslashes at end of lines (both single and double)
    (goto-char (point-min))
    (while (re-search-forward "\\\\+[ \t]*$" nil t)
      (replace-match ""))
    
    (message "Cleaned up markers, HTML blocks, properties blocks, emphasis markers, and trailing backslashes.")))

(defun org-zettel-ref-clean-targets-and-sync ()
  "Clean all markup from the current buffer and then sync the overview."
  (interactive)
  (org-zettel-ref-clean-multiple-targets)
  (save-buffer)
  (when (fboundp 'org-zettel-ref-sync-overview)
    (org-zettel-ref-sync-overview)))

(provide 'org-zettel-ref-ui)

;;; org-zettel-ref-ui.el ends here
