;;; org-zettel-ref-mode.el --- Zettelsken-style Reference Note in Org mode -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Yibie

;; Author: Yibie <yibie@outlook.com>
;; Version: 0.3
;; Package-Requires: ((emacs "29.4"))
;; Keywords: outlines
;; URL: https://github.com/yibie/org-zettel-ref-mode

;;; Commentary:

;; This package provides a mode for creating and managing a reference
;; overview file for marked text and quick notes in Org mode. When activated,
;; it will create (or open) an overview file and display it alongside the original
;; buffer. Any marked text or quick notes in the original buffer will be
;; automatically extracted and added to the overview file.

;;; Code:

(require 'org)
(require 'org-element)
(require 'pulse)
(require 'org-id)
(require 'conda)

(defgroup org-zettel-ref nil
  "Customization options for org-zettel-ref-mode."
  :group 'org)

(defvar org-zettel-ref-current-overview-buffer nil
  "The name of the current overview buffer.")

(defcustom org-zettel-ref-overview-directory "~/org-zettel-ref-overviews/"
  "Directory to store overview files."
  :type 'directory
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-include-empty-notes nil
  "If non-nil, include empty quick notes in the overview."
  :type 'boolean
  :group 'org-zettel-ref)

(defvar org-zettel-ref-overview-file nil
  "The file path where the marked text overview is saved.")

(defcustom org-zettel-ref-mode-type 'normal
  "Specify the mode type for org-zettel-ref.
Possible values are:
'normal  - Default behavior
'denote  - Use Denote file naming convention
'org-roam - Use Org-roam format with ID property"
  :type '(choice (const :tag "Normal" normal)
                 (const :tag "Denote" denote)
                 (const :tag "Org-roam" org-roam))
  :group 'org-zettel-ref)

;;;###autoload
(defun org-zettel-ref-sanitize-filename (filename)
  "Sanitize FILENAME by replacing invalid characters with underscores.
Preserves alphanumeric characters, Chinese characters, and some common punctuation."
  (let ((invalid-chars-regexp "[[:cntrl:]\\/:*?\"<>|]"))
    (replace-regexp-in-string invalid-chars-regexp "_" filename)))
;;;###autoload

(defun org-zettel-ref-generate-filename (title)
  "Generate a filename based on TITLE and current mode type."
  (let* ((sanitized-title (replace-regexp-in-string "[^a-zA-Z0-9\u4e00-\u9fff]+" "-" title))
         (truncated-title (if (> (length sanitized-title) 50)
                              (concat (substring sanitized-title 0 47) "...")
                            sanitized-title))
         (filename
          (pcase org-zettel-ref-mode-type
            ('normal (concat truncated-title "-overview.org"))
            ('denote (let ((date-time (format-time-string "%Y%m%dT%H%M%S")))
                       (format "%s--%s.org" date-time truncated-title)))
            ('org-roam (format "%s-overview.org" truncated-title)))))
    (if (string-empty-p filename)
        (error "Generated filename is empty")
      (message "Debug: Generated filename: %s" filename))
    filename))

(defun org-zettel-ref-ensure-org-roam-db-update (file)
  "Ensure FILE is added to the org-roam database if in org-roam mode."
  (when (and (eq org-zettel-ref-mode-type 'org-roam)
             (require 'org-roam nil t))
    (let ((buffer (find-file-noselect file)))
      (if buffer
          (with-current-buffer buffer
            (org-roam-db-update-file))
        (error "Failed to open file: %s" file)))))

(defun org-zettel-ref-generate-file-content (source-buffer title)
  "Generate file content based on TITLE and current mode type."
  (pcase org-zettel-ref-mode-type
    ('normal (cons nil (format "#+title: Overview for %s\n\n" title)))
    ('denote
     (let ((date (format-time-string "[%Y-%m-%d %a %H:%M]"))
           (id (format-time-string "%Y%m%dT%H%M%S")))
       (cons nil (format "#+title:      %s\n#+date:       %s\n#+filetags:   \n#+identifier: %s\n\n"
                         title date id))))
    ('org-roam
     (if (require 'org-roam nil t)
         (let* ((id (org-id-new))  ; 使用 org-id-new 而不是 org-roam-id-new
                (creation-date (format-time-string "%Y-%m-%d %H:%M:%S"))
                (content (format ":PROPERTIES:
:ID:       %s
:END:
#+title: Overview of %s
#+date: %s
#+filetags: :overview:

* Overview

This is an overview of the note \"%s\".

* Quick Notes

* Marked Text

"
                                id
                                title
                                creation-date
                                title)))
           (cons id content))
       ;; Fallback to normal mode if org-roam is not available
       (message "Org-roam is not available. Falling back to normal mode.")
       (org-zettel-ref-generate-file-content source-buffer title 'normal)))))

(defun org-zettel-ref-init ()
  "Initialize the org-zettel-ref-mode, create or open the overview file and set up the layout."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (_ (message "Debug: source-buffer = %S" source-buffer))
         (overview-file (org-zettel-ref-get-overview-file source-buffer))
         (_ (message "Debug: overview-file = %S" overview-file))
         (overview-buffer-name (org-zettel-ref-get-overview-buffer-name source-buffer))
         (_ (message "Debug: overview-buffer-name = %S" overview-buffer-name))
         (existing-overview-buffer (get-buffer overview-buffer-name)))

    ;; 检查是否已经存在概览窗口
    (if existing-overview-buffer
        (progn
          (message "Debug: Existing overview buffer found")
          (display-buffer existing-overview-buffer
                          '(display-buffer-use-some-window (inhibit-same-window . t))))
      (progn
        (message "Debug: Creating new overview buffer")
        (split-window-right)
        (other-window 1)
        (if (and (stringp overview-file) (file-exists-p overview-file))
            (progn
              (find-file overview-file)
              (message "Debug: Opened file: %s" overview-file))
          (error "Invalid or non-existent overview file: %S" overview-file))
        (rename-buffer overview-buffer-name t)))

    (setq org-zettel-ref-overview-file overview-file)
    (setq org-zettel-ref-current-overview-buffer (buffer-name))
    (org-zettel-ref-setup-buffers source-buffer (current-buffer))
    (message "Debug: org-zettel-ref-init completed")
    (run-with-timer 0.5 nil #'org-zettel-ref-update-roam-db overview-file)))

(defun org-zettel-ref-fallback-file-path (sanitized-title)
  "Generate a fallback file path for SANITIZED-TITLE."
  (expand-file-name
   (concat sanitized-title ".org")
   org-zettel-ref-overview-directory))

(defun org-zettel-ref-update-roam-db (file)
  "Update org-roam database for FILE."
  (when (and (eq org-zettel-ref-mode-type 'org-roam)
             (require 'org-roam nil t))
    (condition-case err
        (progn
          (org-roam-db-update-file file)
          (message "Debug: org-roam database updated for %s" file)
          (let ((node (org-roam-node-from-file file)))
            (when node
              (message "Debug: Node ID: %s, Title: %s" 
                       (org-roam-node-id node) 
                       (org-roam-node-title node)))))
      (error
       (message "Error updating org-roam database: %S" err)))))

(defun org-zettel-ref-get-overview-file (source-buffer)
  "Get or create an overview file for SOURCE-BUFFER based on the current mode type."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (format "Overview of %s" (file-name-base source-file)))
         (sanitized-title (org-zettel-ref-sanitize-filename title))
         (file-path nil))
    (message "Debug: Starting org-zettel-ref-get-overview-file for %s" title)
    
    (setq file-path
          (pcase org-zettel-ref-mode-type
            ('org-roam (org-zettel-ref-get-org-roam-file title sanitized-title))
            ('denote (org-zettel-ref-get-denote-file title sanitized-title))
            ('normal (org-zettel-ref-fallback-file-path sanitized-title))
            (_ (progn
                 (message "Unknown mode type, falling back to normal mode")
                 (org-zettel-ref-fallback-file-path sanitized-title)))))

    (unless (file-exists-p file-path)
      (org-zettel-ref-create-overview-file file-path title))

    (if file-path
        (progn (message "Debug: Returning file path: %s" file-path) file-path)
      (error "Failed to create or find overview file for %s" title))))

(defun org-zettel-ref-get-org-roam-file (title sanitized-title)
  (if (require 'org-roam nil t)
      (condition-case err
          (let ((existing-node (org-roam-node-from-title-or-alias title)))
            (if existing-node
                (progn
                  (message "Debug: Existing node found. File path: %s" (org-roam-node-file existing-node))
                  (org-roam-node-file existing-node))
              (let* ((id (org-id-new))
                     (slug (org-roam-node-slug (org-roam-node-create :title title)))
                     (new-file (expand-file-name (concat slug ".org") org-roam-directory)))
                (message "Debug: New node created. File path: %s" new-file)
                new-file)))
        (error
         (message "Error in org-roam operations: %S" err)
         (org-zettel-ref-fallback-file-path sanitized-title)))
    (message "org-roam not available, falling back to normal mode")
    (org-zettel-ref-fallback-file-path sanitized-title)))

(defun org-zettel-ref-get-denote-file (title sanitized-title)
  (if (and (require 'denote nil t)
           (fboundp 'denote-create-note-using-title))
      (let ((denote-file (denote-create-note-using-title title '("overview"))))
        (message "Debug: Created denote file: %s" denote-file)
        denote-file)
    (message "Denote or denote-create-note-using-title not available, falling back to normal mode")
    (org-zettel-ref-fallback-file-path sanitized-title)))

(defun org-zettel-ref-create-overview-file (file-path title)
  (with-temp-file file-path
    (insert (format "#+title: %s\n\n* Overview\n\n* Quick Notes\n\n* Marked Text\n" title))))



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
  (let* ((source-buffer (or (buffer-base-buffer) (current-buffer)))
         (overview-file (org-zettel-ref-get-overview-file source-buffer))
         (overview-buffer (or (get-buffer org-zettel-ref-current-overview-buffer)
                              (find-file-noselect overview-file))))
    (message "Debug: Syncing overview file: %s" overview-file)
    (with-current-buffer overview-buffer
      (let ((inhibit-read-only t))
        ;; 只更特定部分，而不是重写入整个文件内容
        (goto-char (point-min))
        (when (re-search-forward "^\\* Quick Notes" nil t)
          (delete-region (point) (point-max))
          (insert "\n")
          (org-zettel-ref-insert-quick-notes source-buffer overview-buffer)
          (insert "\n* Marked Text\n\n")
          (org-zettel-ref-insert-marked-text source-buffer overview-buffer)
          (save-buffer)))
    (message "Debug: Sync completed"))))

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
         (overview-file-name (org-zettel-ref-generate-filename
                              (file-name-base source-file-name)))
         (buffer-name (format "*Org Zettel Ref: %s*" overview-file-name)))
    (generate-new-buffer-name buffer-name)))

(defun org-zettel-ref-ensure-overview-buffer ()
  "Ensure that the overview buffer exists, creating it if necessary."
  (unless (and org-zettel-ref-current-overview-buffer
               (get-buffer org-zettel-ref-current-overview-buffer))
    (org-zettel-ref-init)))

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

(defun org-zettel-ref-open-source-link (link)
  "Open the source file of a link and jump to the target position.
If the source file is already visible, jump to it.
Otherwise, open the source file in a new window."
  (let* ((path (org-element-property :path link))
         (search (org-element-property :search-option link))
         (source-buffer (find-buffer-visiting path)))
    (if source-buffer
        (progn
          (pop-to-buffer source-buffer)
          (when search
            (org-link-search search)))
      (find-file-other-window path)
      (when search
        (org-link-search search)))
    (recenter)))

(defun org-zettel-ref-advice-open-at-point (orig-fun &rest args)
  "Advice function to customize org-open-at-point behavior for overview links."
  (let ((context (org-element-context)))
    (if (and (eq (org-element-type context) 'link)
             (string-equal (org-element-property :type context) "file"))
        (org-zettel-ref-open-source-link context)
      (apply orig-fun args))))

(defun org-zettel-ref-mode-enable ()
  "Enable org-zettel-ref-mode."
  (advice-add 'org-open-at-point :around #'org-zettel-ref-advice-open-at-point)
  (org-zettel-ref-setup-quick-markup)
  (local-set-key (kbd "C-c r") 'org-zettel-ref-run-python-script))

(defun org-zettel-ref-mode-disable ()
  "Disable org-zettel-ref-mode."
  (advice-remove 'org-open-at-point #'org-zettel-ref-advice-open-at-point)
  (local-unset-key (kbd org-zettel-ref-quick-markup-key)))

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


;;org-zettel-ref-run-python-script

;; 调试函数
(defun org-zettel-ref-debug-message (format-string &rest args)
  "Print a debug message to the *Messages* buffer."
  (apply #'message (concat "ORG-ZETTEL-REF-DEBUG: " format-string) args))

(defcustom org-zettel-ref-python-file "~/Documents/emacs/package/org-zettel-ref-mode/document_convert_to_org.py"
  "Python 脚本的完整路径。"
  :type 'string
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-temp-folder "~/Documents/temp_convert/"
  "临文件夹的路径。"
  :type 'string
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-reference-folder "~/Documents/ref/"
  "参考资料文件夹的路径。"
  :type 'string
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-archive-folder "/Volumes/Collect/archives/"
  "归档文件夹的路径。"
  :type 'string
  :group 'org-zettel-ref)


(defcustom org-zettel-ref-python-environment 'system
  "指定要使用的 Python 环境类型。
可能的值包括：
'system - 使用系统 Python
'conda  - 使用 Conda 环境
'venv   - 使用 virtualenv 或 venv 环境"
  :type '(choice (const :tag "系统 Python" system)
                 (const :tag "Conda 环境" conda)
                 (const :tag "Virtualenv/venv" venv))
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-python-env-name nil
  "指定虚拟环境的名称（适用于 Conda 或 venv）。"
  :type '(choice (const :tag "无" nil)
                 (string :tag "环境称"))
  :group 'org-zettel-ref)

(defun get-user-shell ()
  "获取用户的默认 shell。"
  (file-name-nondirectory (or (getenv "SHELL") "/bin/sh")))

(defun get-shell-source-command ()
  "根据用户的 shell 返回适当的 source 命令。"
  (let ((shell (get-user-shell)))
    (cond
     ((string-match-p "fish" shell) "source")
     ((string-match-p "csh" shell) "source")
     (t "."))))

(defun org-zettel-ref-run-python-script ()
  (interactive)
  (let* ((script-path (expand-file-name org-zettel-ref-python-file))
         (default-directory (file-name-directory script-path))
         (conda-env-name org-zettel-ref-python-env-name)
         (python-command (org-zettel-ref-get-python-command))
         (command (format "conda run -n %s %s %s"
                          (shell-quote-argument conda-env-name)
                          python-command
                          (shell-quote-argument (file-name-nondirectory script-path)))))
    (if (file-exists-p script-path)
        (progn
          (message "Executing command: %s" command)
          (let ((default-directory default-directory))
            (async-shell-command command "*Convert to Org*")))
      (error "Cannot find the specified Python script: %s" script-path))))

(defun org-zettel-ref-get-python-command ()
  "根据设置的环境类型返回适当的 Python 命令。"
  (let ((command
         (pcase org-zettel-ref-python-environment
           ('system "python")
           ('conda
            (if org-zettel-ref-python-env-name
                (format "conda run -n %s python" org-zettel-ref-python-env-name)
              "conda run -n base python"))
           ('venv
            (if org-zettel-ref-python-env-name
                (format "source %s/bin/activate && python" org-zettel-ref-python-env-name)
              (error "使用 venv 时必须指定环境名称")))
           (_ (error "未知的 Python 环境类型")))))
    (org-zettel-ref-debug-message "Python command: %s" command)
    command))

(defun org-zettel-ref-get-overview-file (source-buffer)
  "Get or create an overview file for SOURCE-BUFFER based on the current mode type."
  (let* ((source-file (buffer-file-name source-buffer))
         (title (format "Overview of %s" (file-name-base source-file)))
         (sanitized-title (if (fboundp 'org-zettel-ref-sanitize-filename)
                              (org-zettel-ref-sanitize-filename title)
                            title))
         file-path)
    (message "Debug: Starting org-zettel-ref-get-overview-file for %s" title)
    
    (setq file-path
          (pcase org-zettel-ref-mode-type
            ('org-roam
             (if (require 'org-roam nil t)
                 (condition-case err
                     (let ((existing-node (org-roam-node-from-title-or-alias title)))
                       (if existing-node
                           (progn
                             (message "Debug: Existing node found. File path: %s" (org-roam-node-file existing-node))
                             (org-roam-node-file existing-node))
                         (message "Debug: No existing node found, creating new one")
                         (let* ((id (org-id-new))
                                (slug (org-roam-node-slug (org-roam-node-create :title title)))
                                (new-file (expand-file-name (concat slug ".org") org-roam-directory)))
                           (unless (file-exists-p new-file)
                             (with-temp-file new-file
                               (insert (format ":PROPERTIES:
:ID:       %s
:END:
#+title: %s
#+filetags: :overview:

* Quick Notes

* Marked Text

"
                                               id title))))
                           (message "Debug: New node created. File path: %s" new-file)
                           new-file)))
                   (error
                    (message "Error in org-roam operations: %S" err)
                    (org-zettel-ref-fallback-file-path sanitized-title))))
               (message "org-roam not available, falling back to normal mode")
               (org-zettel-ref-fallback-file-path sanitized-title)))
            
            ('denote
             (if (require 'denote nil t)
                 (let ((denote-file (denote-create-note-using-title title)))
                   (message "Debug: Created denote file: %s" denote-file)
                   denote-file)
               (message "denote not available, falling back to normal mode")
               (org-zettel-ref-fallback-file-path sanitized-title)))
            
            ('normal
             (org-zettel-ref-fallback-file-path sanitized-title))
            
            (_ (message "Unknown mode type, falling back to normal mode")
               (org-zettel-ref-fallback-file-path sanitized-title))))

    (unless (file-exists-p file-path)
      (with-temp-file file-path
        (insert (format "#+title: %s\n\n* Overview\n\n* Quick Notes\n\n* Marked Text\n" title))))

    (if file-path
        (progn (message "Debug: Returning file path: %s" file-path) file-path)
      (error "Failed to create or find overview file for %s" title)))


(defun org-zettel-ref-init-conda ()
  "Initialize and activate Conda environment."
  (let ((conda-path (executable-find "conda")))
    (when conda-path
      (setq conda-initialize-hook nil)
      (setq conda-env-home-directory (file-name-directory (directory-file-name (file-name-directory conda-path))))
      (setenv "CONDA_EXE" conda-path)
      (setq conda-env-executables-dir (file-name-directory conda-path))
      (conda-env-initialize-interactive-shells)
      (conda-env-initialize-eshell)
      (when org-zettel-ref-python-env-name
        (conda-env-activate org-zettel-ref-python-env-name)))))

;;;###autoload
(define-minor-mode org-zettel-ref-mode
  "Minor mode for managing reference notes in Org mode."
  :init-value nil
  :lighter " ZettelRef"
  (if org-zettel-ref-mode
      (progn
        (when (eq org-zettel-ref-mode-type 'org-roam)
          (if (require 'org-roam nil t)
              (condition-case err
                  (progn
                    (org-roam-db)
                    (org-roam-list-files)
                    (org-roam-db-autosync-mode 1))
                (error
                 (message "Error initializing org-roam: %s" err)
                 (setq org-zettel-ref-mode-type 'normal)))
            (message "org-roam is not available, falling back to normal mode")
            (setq org-zettel-ref-mode-type 'normal)))
        (org-zettel-ref-init-conda)  ; 在这里调用初始化函数
        (org-zettel-ref-mode-enable))
    (progn
      (when (eq org-zettel-ref-mode-type 'org-roam)
        (org-roam-db-autosync-mode -1))
      (org-zettel-ref-mode-disable))))

(defun org-zettel-ref-check-roam-db ()
  "Check the status of the org-roam database."
  (interactive)
  (if (require 'org-roam nil t)
      (condition-case err
          (progn
            (message "Org-roam version: %s" (org-roam-version))
            (message "Org-roam directory: %s" org-roam-directory)
            (message "Org-roam database file: %s" org-roam-db-location)
            (if (file-exists-p org-roam-db-location)
                (message "Database file exists")
              (message "Database file does not exist"))
            (let ((node-count (caar (org-roam-db-query [:select (funcall count *) :from nodes]))))
              (message "Number of nodes in database: %d" node-count)))
        (error
         (message "Error checking org-roam database: %S" err)))
    (message "Org-roam is not available")))



(provide 'org-zettel-ref-mode)

;;; org-zettel-ref-mode.el ends here-


