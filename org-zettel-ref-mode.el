;;; org-zettel-ref-mode.el --- Zettelsken-style Reference Note in Org mode -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Yibie

;; Author: Yibie <yibie@outlook.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.4"))
;; Keywords: outlines
;; URL: https://github.com/yibie/org-zettel-ref-mode

;;; Commentary

;; This package provides a mode for creating and managing a reference
;; overview file for marked text and quick notes in Org mode. When activated,
;; it will create (or open) an overview file and display it alongside the original
;; buffer. Any marked text or quick notes in the original buffer will be
;; automatically extracted and added to the overview file.

;;; Code:

(require 'org)
(require 'org-element)
(require 'pulse)

(defgroup org-zettel-ref nil
  "Customization options for org-zettel-ref-mode."
  :group 'org)

(defvar org-zettel-ref-current-overview-buffer nil
  "The name of the current overview buffer.")

(defcustom org-zettel-ref-overview-directory "~/Documents/org-overviews/"
  "Directory where the overview files will be saved."
  :type 'string
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-include-empty-notes nil
  "If non-nil, include empty quick notes in the overview."
  :type 'boolean
  :group 'org-zettel-ref)

(defvar org-zettel-ref-overview-file nil
  "The file path where the marked text overview is saved.")


(defun org-zettel-ref-init ()
  "Initialize the org-zettel-ref-mode, create or open the overview file and set up the layout."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (overview-file (org-zettel-ref-create-or-open-overview-file source-buffer))
         (overview-buffer-name (org-zettel-ref-get-overview-buffer-name source-buffer)))
    (split-window-right)
    (other-window 1)
    (find-file overview-file)
    (rename-buffer overview-buffer-name)
    (setq org-zettel-ref-overview-file overview-file)
    (setq org-zettel-ref-current-overview-buffer overview-buffer-name)
    (org-zettel-ref-setup-buffers source-buffer (current-buffer))))

(defun org-zettel-ref-create-or-open-overview-file (source-buffer)
  "Create or open the overview file based on the source buffer."
  (let* ((overview-dir (file-name-as-directory org-zettel-ref-overview-directory))
         (source-file-name (buffer-file-name source-buffer))
         (overview-file-name (concat (file-name-sans-extension
                                      (file-name-nondirectory source-file-name))
                                     "-overview.org"))
         (overview-file (expand-file-name overview-file-name overview-dir)))
    (unless (file-exists-p overview-dir)
      (make-directory overview-dir t))
    (unless (file-exists-p overview-file)
      (with-temp-file overview-file
        (insert (format "#+title: Overview for %s\n\n"
                        (file-name-nondirectory source-file-name)))))
    overview-file))

(defun org-zettel-ref-get-or-create-overview-buffer (source-buffer)
  "Get or create the overview buffer for SOURCE-BUFFER."
  (let* ((overview-file (org-zettel-ref-create-or-open-overview-file source-buffer))
         (overview-buffer-name (org-zettel-ref-get-overview-buffer-name source-buffer))
         (overview-buffer (get-buffer-create overview-buffer-name)))
    (with-current-buffer overview-buffer
      (when (or (not (buffer-file-name))
                (not (file-equal-p (buffer-file-name) overview-file)))
        (set-visited-file-name overview-file)
        (set-buffer-modified-p nil))
      (unless (eq major-mode 'org-mode)
        (org-mode))
      (setq buffer-read-only t))
    overview-buffer))

(defun org-zettel-ref-setup-buffers (source-buffer overview-buffer)
  "Set up the source and overview buffers with appropriate modes and configurations."
  (with-current-buffer source-buffer
    (org-mode))
  (with-current-buffer overview-buffer
    (org-mode)
    (setq buffer-read-only t))
  (other-window 1))



(defun org-zettel-ref-add-quick-note ()
  "Add a quick note at point in the source buffer and create a corresponding link in the overview file."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (note-name (read-string "Enter note name: "))
         (note-content (read-string "Enter note content (optional): ")))

    ;; Insert target link in source buffer
    (insert (format "<<%s>>" note-name))
    (when (not (string-empty-p note-content))
      (insert " " note-content))

    ;; Synchronize overview
    (org-zettel-ref-sync-overview)))

(defun org-zettel-ref-sync-overview ()
  "Synchronize the quick notes and marked text from the source buffer to the overview file."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (overview-buffer (org-zettel-ref-get-or-create-overview-buffer source-buffer)))
    (when (buffer-modified-p source-buffer)
      (save-buffer source-buffer))
    (with-current-buffer overview-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+title: Overview for %s\n\n"
                        (file-name-nondirectory (buffer-file-name source-buffer))))
        (insert "* Quick Notes\n\n")
        (org-zettel-ref-insert-quick-notes source-buffer overview-buffer)
        (insert "\n* Marked Text\n\n")
        (org-zettel-ref-insert-marked-text source-buffer overview-buffer))
      (save-buffer))))

(defun org-zettel-ref-insert-quick-notes (source-buffer overview-buffer)
  "Insert quick notes from SOURCE-BUFFER into OVERVIEW-BUFFER."
  (with-current-buffer source-buffer
    (org-element-map (org-element-parse-buffer) 'target
      (lambda (target)
        (let* ((begin (org-element-property :begin target))
               (end (org-element-property :end target))
               (name (org-element-property :value target))
               (content (buffer-substring-no-properties begin end)))
          (when (string-match "<<\\([^>]+\\)>>\\(.*\\)" content)
            (let ((note-name (match-string 1 content))
                  (note-content (string-trim (match-string 2 content))))
              (with-current-buffer overview-buffer
                (let ((inhibit-read-only t))
                  (insert (format "- [[file:%s::%s][%s]]\n"
                                  (buffer-file-name source-buffer)
                                  note-name
                                  (if (string-empty-p note-content)
                                      note-name
                                    note-content))))))))))))


(defun org-zettel-ref-enable-auto-sync ()
  "Enable automatic synchronization between the source buffer and the overview file."
  (interactive)
  (add-hook 'after-save-hook #'org-zettel-ref-sync-overview nil t)
  (message "Automatic synchronization enabled."))

(defun org-zettel-ref-disable-auto-sync ()
  "Disable automatic synchronization between the source buffer and the overview file."
  (interactive)
  (remove-hook 'after-save-hook #'org-zettel-ref-sync-overview t)
  (message "Automatic synchronization disabled."))


(defcustom org-zettel-ref-include-context nil
  "If non-nil, include more context in quick notes overview."
  :type 'boolean
  :group 'org-zettel-ref)

(defun org-zettel-ref-insert-quick-notes-with-target (source-buffer)
  "Insert quick notes marked with <<target>> from the source buffer into the overview buffer."
  (let ((overview-buffer (org-zettel-ref-get-or-create-overview-buffer source-buffer)))
    (with-current-buffer source-buffer
      (org-element-map (org-element-parse-buffer) 'target
        (lambda (element)
          (let* ((begin (org-element-property :begin element))
                 (end (org-element-property :end element))
                 (name (org-element-property :value element))
                 (content (buffer-substring-no-properties begin end)))
            (when (string-match "<<\\([^>]+\\)>>\\(.*\\)" content)
              (let ((target-name (match-string 1 content))
                    (actual-content (string-trim (match-string 2 content))))
                (when (or org-zettel-ref-include-empty-notes
                          (not (string-empty-p actual-content)))
                  (with-current-buffer overview-buffer
                    (let ((inhibit-read-only t)
                          (link-text (cond
                                      (org-zettel-ref-include-context content)
                                      ((string-empty-p actual-content) target-name)
                                      (t actual-content))))
                      (insert (format "- [[file:%s::%s][%s]]\n"
                                      (buffer-file-name source-buffer)
                                      target-name
                                      link-text)))))))))))))

(defun org-zettel-ref-insert-marked-text (source-buffer overview-buffer)
  "Insert marked text from SOURCE-BUFFER into OVERVIEW-BUFFER."
  (with-current-buffer source-buffer
    (org-element-map (org-element-parse-buffer) '(bold underline verbatim code)
      (lambda (element)
        (let* ((begin (org-element-property :begin element))
               (end (org-element-property :end element))
               (raw-text (string-trim (buffer-substring-no-properties begin end))))
          (when (and raw-text (not (string-empty-p raw-text)))
            (with-current-buffer overview-buffer
              (let ((inhibit-read-only t))
                (insert (format "- %s\n" raw-text))))))))))

(defun org-zettel-ref-get-overview-buffer-name (source-buffer)
  "Get the name of the overview buffer for the given source buffer."
  (let* ((source-file-name (buffer-file-name source-buffer))
         (overview-file-name (concat (file-name-sans-extension
                                      (file-name-nondirectory source-file-name))
                                     "-overview.org")))
    (format "*Org Zettel Ref: %s*" overview-file-name)))


(defun org-zettel-ref-ensure-overview-buffer ()
  "Ensure that the overview buffer exists, creating it if necessary."
  (unless (and org-zettel-ref-current-overview-buffer
               (get-buffer org-zettel-ref-current-overview-buffer))
    (org-zettel-ref-init)))


(defun org-zettel-ref-clean-targets ()
  "Remove all <<target>> markers from the current buffer after confirmation."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to remove all <<target>> markers? This cannot be undone. ")
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "<<\\([^>]+\\)>>" nil t)
        (replace-match "")
        (just-one-space)))
    (message "All <<target>> markers have been removed.")))

(defun org-zettel-ref-clean-targets-and-sync ()
  "Clean all <<target>> markers from the current buffer and then sync the overview."
  (interactive)
  (org-zettel-ref-clean-targets)
  (save-buffer)
  (org-zettel-ref-sync-overview))

(defun org-zettel-ref-open-source-link (link)
  "Open the source file of a link in another window and jump to the target position."
  (let* ((path (org-element-property :path link))
         (search (org-element-property :search-option link)))
    (find-file-other-window path)
    (when search
      (org-link-search search))
    (recenter)))

(defun org-zettel-ref-advice-open-at-point (orig-fun &rest args)
  "Advice function to customize org-open-at-point behavior for overview links."
  (let ((context (org-element-context)))
    (if (and (eq (org-element-type context) 'link)
             (string-equal (org-element-property :type context) "file")
             (buffer-file-name)
             (string-match-p "-overview\\.org$" (buffer-file-name)))
        (org-zettel-ref-open-source-link context)
      (apply orig-fun args))))

(advice-add 'org-open-at-point :around #'org-zettel-ref-advice-open-at-point)

(defun org-zettel-ref-mode-enable ()
  "Enable org-zettel-ref-mode."
  (advice-add 'org-open-at-point :around #'org-zettel-ref-advice-open-at-point))

(defun org-zettel-ref-mode-disable ()
  "Disable org-zettel-ref-mode."
  (advice-remove 'org-open-at-point #'org-zettel-ref-advice-open-at-point))

(define-minor-mode org-zettel-ref-mode
  "Minor mode for managing reference notes in Org mode."
  :init-value nil
  :lighter " ZettelRef"
  (if org-zettel-ref-mode
      (org-zettel-ref-mode-enable)
    (org-zettel-ref-mode-disable)))

;;quick mark-up menu
;; (setq org-zettel-ref-quick-markup-key "C-c C-m")
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

(defcustom org-zettel-ref-quick-markup-key "C-c m"
  "Key binding for quick markup function in org-zettel-ref-mode.
This should be a string that can be passed to `kbd'."
  :type 'string
  :group 'org-zettel-ref
  :set (lambda (sym val)
         (set-default sym val)
         (when (featurep 'org-zettel-ref-mode)
           (org-zettel-ref-setup-quick-markup))))

(defun org-zettel-ref-setup-quick-markup ()
  "Set up the key binding for quick markup."
  (local-set-key (kbd org-zettel-ref-quick-markup-key) 'org-zettel-ref-quick-markup))

(define-minor-mode org-zettel-ref-mode
  "Minor mode for managing reference notes in Org mode."
  :init-value nil
  :lighter " ZettelRef"
  (if org-zettel-ref-mode
      (progn
        (org-zettel-ref-mode-enable)
        (org-zettel-ref-setup-quick-markup))
    (org-zettel-ref-mode-disable)))

(provide 'org-zettel-ref-mode)
;;; org-zettel-ref-mode.el ends here
