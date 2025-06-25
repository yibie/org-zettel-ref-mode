;;; org-zettel-ref-utils.el --- Utility functions for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains utility functions for org-zettel-ref.

;;; Code:


;;----------------------------------------------------------------
;; org-zettel-ref-debug
;;----------------------------------------------------------------

(defcustom org-zettel-ref-debug nil
  "When non-nil, enable debug messages globally."
  :type 'boolean
  :group 'org-zettel-ref)

(defvar org-zettel-ref-debug-categories
  '((core    . t)     ;; core functionality
    (db      . t)    ;; database operations
    (list    . t)    ;; list management
    (highlight . t)  ;; highlighting
    (ui      . t))   ;; user interface
  "Debug categories and their status.")

(defun org-zettel-ref-debug-message-category (category format-string &rest args)
  "Print debug message for specific CATEGORY if enabled.
FORMAT-STRING and ARGS are passed to `message'."
  (when (and org-zettel-ref-debug
             (alist-get category org-zettel-ref-debug-categories))
    (apply #'message
           (concat (format "ORG-ZETTEL-REF[%s]: " category) format-string)
           args)))

(defun org-zettel-ref-toggle-debug ()
  "Toggle global debug mode."
  (interactive)
  (setq org-zettel-ref-debug (not org-zettel-ref-debug))
  (message "Org-Zettel-Ref debug mode %s"
           (if org-zettel-ref-debug "enabled" "disabled")))

(defun org-zettel-ref-toggle-debug-category (category)
  "Toggle debug status for CATEGORY."
  (interactive
   (list (intern (completing-read "Category: "
                                 (mapcar #'car org-zettel-ref-debug-categories)))))
  (setf (alist-get category org-zettel-ref-debug-categories)
        (not (alist-get category org-zettel-ref-debug-categories)))
  (message "Category %s debug mode: %s"
           category
           (if (alist-get category org-zettel-ref-debug-categories)
               "enabled" "disabled")))

;; debug message
(defun org-zettel-ref-debug-message (format-string &rest args)
  "Print debug message if debug mode is enabled.
FORMAT-STRING and ARGS are passed to `message'."
  (when org-zettel-ref-debug
    (apply #'message (concat "ORG-ZETTEL-REF: " format-string) args)))

(defun org-zettel-ref-debug-status ()
  "Display current debug status for all categories."
  (interactive)
  (with-current-buffer (get-buffer-create "*Org-Zettel-Ref Debug Status*")
    (erase-buffer)
    (org-mode)
    (insert "* Org-Zettel-Ref Debug Status\n\n")
    (insert (format "Global Debug Mode: %s\n\n"
                   (if org-zettel-ref-debug "Enabled" "Disabled")))
    (insert "** Category Status\n\n")
    (dolist (category org-zettel-ref-debug-categories)
      (insert (format "- %s: %s\n"
                     (car category)
                     (if (cdr category) "Enabled" "Disabled"))))
    (insert "\n** Commands\n\n")
    (insert "- M-x org-zettel-ref-toggle-debug :: Toggle global debug mode\n")
    (insert "- M-x org-zettel-ref-toggle-debug-category :: Toggle specific category\n")
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun org-zettel-ref-check-status ()
  "Check and display the current status of org-zettel-ref-mode."
  (interactive)
  (org-zettel-ref-debug-message-category 'core "\n=== Org-Zettel-Ref Status ===")
  (org-zettel-ref-debug-message-category 'core
    "Mode enabled: %s" org-zettel-ref-mode)
  (org-zettel-ref-debug-message-category 'core
    "Overview file: %s" org-zettel-ref-overview-file)
  (org-zettel-ref-debug-message-category 'core
    "Source buffer: %s"
    (buffer-name org-zettel-ref-source-buffer))
  (org-zettel-ref-debug-message-category 'core
    "Overview buffer: %s"
    (buffer-name org-zettel-ref-current-overview-buffer)))

(defun org-zettel-ref-debug-show-all-status ()
  "Show status of all debug categories."
  (interactive)
  (with-current-buffer (get-buffer-create "*Org-Zettel-Ref Debug Status*")
    (erase-buffer)
    (org-mode)
    (insert "* Org-Zettel-Ref Debug Status\n\n")
    (insert (format "- Global Debug Mode: %s\n\n"
                   (if org-zettel-ref-debug "Enabled" "Disabled")))
    (insert "** Category Status\n\n")
    (dolist (category org-zettel-ref-debug-categories)
      (insert (format "- %s: %s\n"
                     (car category)
                     (if (cdr category) "Enabled" "Disabled"))))
    (insert "\n** Available Commands\n\n")
    (insert "- M-x org-zettel-ref-toggle-debug :: Toggle global debug mode\n")
    (insert "- M-x org-zettel-ref-toggle-debug-category :: Toggle specific category\n")
    (insert "- M-x org-zettel-ref-debug-enable-all :: Enable all debug categories\n")
    (insert "- M-x org-zettel-ref-debug-disable-all :: Disable all debug categories\n")
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun org-zettel-ref-debug-enable-all ()
  "Enable debugging for all categories."
  (interactive)
  (setq org-zettel-ref-debug t)
  (dolist (category org-zettel-ref-debug-categories)
    (setf (alist-get (car category) org-zettel-ref-debug-categories) t))
  (message "Enabled all debug categories"))

(defun org-zettel-ref-debug-disable-all ()
  "Disable debugging for all categories."
  (interactive)
  (setq org-zettel-ref-debug nil)
  (dolist (category org-zettel-ref-debug-categories)
    (setf (alist-get (car category) org-zettel-ref-debug-categories) nil))
  (message "Disabled all debug categories"))

(defun org-zettel-ref-debug-reset ()
  "Reset debug settings to default values."
  (interactive)
  (setq org-zettel-ref-debug nil)
  (setq org-zettel-ref-debug-categories
        '((core    . t)    ;; core functionality
          (db      . t)   ;; database operations
          (list    . t)    ;; list management
          (highlight . t)  ;; highlighting
          (ui      . t)))  ;; user interface
  (message "Reset debug settings to defaults"))

;;----------------------------------------------------------------
;; org-zettel-ref-run-python-script
;;----------------------------------------------------------------

(defcustom org-zettel-ref-python-file "~/Documents/emacs/package/org-zettel-ref-mode/convert_to_org.py"
  "Python script file path."
  :type 'string
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-python-path "python3"
  "Path to Python executable."
  :type 'string
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-temp-folder "~/Documents/temp_convert/"
  "Temporary folder path."
  :type 'string
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-reference-folder "~/Documents/ref/"
  "Reference folder path."
  :type 'string
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-archive-folder "/Volumes/Collect/archives/"
  "Archive folder path."
  :type 'string
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-python-environment 'venv
  "Python virtual environment type to use.
Can be either 'venv or 'conda."
  :type '(choice (const :tag "Python venv" venv)
                (const :tag "Conda" conda))
  :group 'org-zettel-ref)

;; run python script
(defun org-zettel-ref-run-python-script ()
  "Run the configured Python script with virtual environment support."
  (interactive)
  (let* ((script-path (expand-file-name org-zettel-ref-python-file))
         (default-directory (file-name-directory script-path))
         (venv-type (symbol-name org-zettel-ref-python-environment))
         (temp-folder (expand-file-name org-zettel-ref-temp-folder))
         (reference-folder (expand-file-name org-zettel-ref-reference-folder))
         (archive-folder (expand-file-name org-zettel-ref-archive-folder)))

    ;; Set virtual environment type
    (setenv "ORG_ZETTEL_REF_PYTHON_ENV" venv-type)

    ;; Run the script with appropriate checks
    (cond
     ((not (file-exists-p script-path))
      (error "Cannot find the specified Python script: %s" script-path))
     ((not (file-directory-p temp-folder))
      (error "Temporary folder does not exist: %s" temp-folder))
     ((not (file-directory-p reference-folder))
      (error "Reference folder does not exist: %s" reference-folder))
     ((not (file-directory-p archive-folder))
      (error "Archive folder does not exist: %s" archive-folder))
     (t
      (let ((command (format "%s %s --temp %s --reference %s --archive %s"
                            (shell-quote-argument org-zettel-ref-python-path)
                            (shell-quote-argument script-path)
                            (shell-quote-argument temp-folder)
                            (shell-quote-argument reference-folder)
                            (shell-quote-argument archive-folder))))
        (org-zettel-ref-debug-message "Executing command: %s" command)
        (async-shell-command command "*Convert to Org*")
        (with-current-buffer "*Convert to Org*"
          (org-zettel-ref-debug-message "Python script output:\n%s" (buffer-string))))))))

;;----------------------------------------------------------------
;; Other components
;;----------------------------------------------------------------

(defun org-zettel-ref-mode-enable ()
  "Enable org-zettel-ref-mode."
  (org-zettel-ref-init)
  (org-zettel-ref-setup-quick-markup)
  (advice-add 'org-open-at-point :around #'org-zettel-ref-advice-open-at-point))

(defun org-zettel-ref-mode-disable ()
  "Disable org-zettel-ref-mode."
  (advice-remove 'org-open-at-point #'org-zettel-ref-advice-open-at-point)
  (local-unset-key (kbd org-zettel-ref-quick-markup-key)))

(defun org-zettel-ref-advice-open-at-point (orig-fun &rest args)
  "Simplified link handling using the context of org-zettel-ref-mode."
  (let* ((context (org-element-context))
         (type (org-element-property :type context))
         (path (org-element-property :path context)))
    (if (and (eq (org-element-type context) 'link)
             (string= type "file")
             (string-match ".*::hl-\\([0-9]+\\)" path))
        ;; If it's a highlight link, jump directly using the existing association
        (with-current-buffer org-zettel-ref-source-buffer  ;; Use the associated source file buffer
          (widen)
          (goto-char (point-min))
          (let ((target-id (match-string 1 path)))
            (if (re-search-forward (concat "<<hl-" target-id ">>") nil t)
                (progn
                  (goto-char (match-beginning 0))
                  (org-reveal)
                  (org-show-entry)
                  (switch-to-buffer-other-window (current-buffer))
                  t)
              (message "Target hl-%s not found" target-id)
              nil)))
      ;; Handle other links using the original function
      (apply orig-fun args))))

(defun org-zettel-ref-find-target (file target)
  "Find TARGET in FILE and jump to its location.
FILE should be absolute path, TARGET should be the target identifier."
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (widen)
      (goto-char (point-min))
      (if (re-search-forward (concat "<<" (regexp-quote target) ">>") nil t)
          (progn
            (switch-to-buffer buf)
            (goto-char (match-beginning 0))
            (org-reveal)
            (org-show-entry)
            t)
        (message "Target %s not found in %s" target file)
        nil))))

;; Other utility functions
(defun org-zettel-ref-get-overview-buffer-name (source-buffer)
  "Get the overview buffer name corresponding to SOURCE-BUFFER."
  (format "*Org Zettel Ref: %s*"
          (file-name-base (buffer-file-name source-buffer))))

(defun org-zettel-ref-extract-id-from-filename (filename)
  "Extract the ID from the filename."
  (when (string-match "\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)" filename)
    (match-string 1 filename)))

(provide 'org-zettel-ref-utils)

;;; org-zettel-ref-utils.el ends here
