;;; org-zettel-ref-highlight-simple.el --- Simple highlighting with target links -*- lexical-binding: t; -*-

;;; Commentary:
;; Usage format:
;; <<hl-1>> §q{Highlighted text}  <- In the source file
;; ❓ Highlighted text <- Format displayed in the overview file

;;----------------------------------------------------------------------------  
;; Variables
;;----------------------------------------------------------------------------

(defun org-zettel-ref-highlight-type-to-char (type)
  "Convert highlight type to its single character identifier."
  (let ((config (cdr (assoc type org-zettel-ref-highlight-types))))
    (message "DEBUG: Converting type '%s' to char" type)
    (message "DEBUG: Config found: %S" config)
    (if config
        (let ((char (plist-get config :char)))
          (message "DEBUG: Found char: %s" char)
          char)
      (user-error "Unknown highlight type: %s" type))))

(defun org-zettel-ref-highlight-char-to-type (char)
  "Convert single character identifier to highlight type."
  (let ((found nil))
    (catch 'found
      (dolist (type-def org-zettel-ref-highlight-types)
        (when (string= (plist-get (cdr type-def) :char) char)
          (throw 'found (car type-def))))
      (user-error "Unknown highlight char: %s" char))))

(defcustom org-zettel-ref-highlight-types
  '(("question" . (:char "q"
                  :face (:background "#FFE0B2" :foreground "#000000" :extend t)
                  :name "question"
                  :prefix "❓"))
    ("fact" . (:char "f"
               :face (:background "#B2DFDB" :foreground "#000000" :extend t)
               :name "fact"
               :prefix "📝"))
    ("method" . (:char "m"
                :face (:background "#BBDEFB" :foreground "#000000" :extend t)
                :name "method"
                :prefix "🔧"))
    ("process" . (:char "p"
                 :face (:background "#E1BEE7" :foreground "#000000" :extend t)
                 :name "process"
                 :prefix "⛓️"))
    ("definition" . (:char "d"
                    :face (:background "#F8BBD0" :foreground "#000000" :extend t)
                    :name "definition"
                    :prefix "📖"))
    ("note" . (:char "n"
               :face (:background "#E8EAF6" :foreground "#000000" :extend t)
               :name "note"
               :prefix "✍️"))
    ("debate" . (:char "b"
                :face (:background "#FF8A80" :foreground "#000000" :extend t)
                :name "debate"
                :prefix "🙃"))
    ("future" . (:char "u"
                :face (:background "#FFB74D" :foreground "#000000" :extend t)
                :name "future"
                :prefix "🔮"))
    ("quote" . (:char "t"
                :face (:background "#C5CAE9" :foreground "#000000" :extend t)
                :name "quote"
                :prefix "💭"))
    ("image" . (:char "i"
                :face (:background "#FFECB3" :foreground "#000000" :extend t)
                :name "image"
                :prefix "🖼️")))
  "Configuration for highlight types.
Each type should have:
- :char    Single character identifier for the type
- :face    Face properties for highlighting
- :name    Display name of the type
- :prefix  Symbol to show in overview"
  :type '(alist :key-type string
                :value-type (plist :key-type symbol :value-type sexp))
  :group 'org-zettel-ref)


(defvar-local org-zettel-ref-highlight-counter 0
  "Global counter for highlight marks.")

(defcustom org-zettel-ref-highlight-regexp
  "<<hl-\\([0-9]+\\)>> §\\([a-z]\\){\\([^}]+\\)}"
  "Regexp for matching highlight marks.
Group 1: Reference ID
Group 2: Type (single character identifier)
Group 3: Content (for images including path and description)"
  :type 'string
  :group 'org-zettel-ref)


;;----------------------------------------------------------------------------
;; Highlight ID
;;----------------------------------------------------------------------------

(defun org-zettel-ref-highlight-ensure-counter ()
  "Ensure the highlight counter is properly initialized."
  (unless (and (boundp 'org-zettel-ref-highlight-counter)
               (numberp org-zettel-ref-highlight-counter))
    (make-local-variable 'org-zettel-ref-highlight-counter)
    (setq-local org-zettel-ref-highlight-counter 0)
    (org-zettel-ref-highlight-initialize-counter)))


(defun org-zettel-ref-highlight-generate-id ()
  "Generate the next highlight ID."
  (org-zettel-ref-highlight-ensure-counter)  
  (setq-local org-zettel-ref-highlight-counter 
              (1+ org-zettel-ref-highlight-counter))
  (number-to-string org-zettel-ref-highlight-counter))


;;----------------------------------------------------------------------------
;; Highlight Display
;;----------------------------------------------------------------------------

(defun org-zettel-ref-highlight-region (type)
  "Highlight the current region with the specified type TYPE."
  (interactive
   (list (completing-read "Highlight type: "
                         (mapcar #'car org-zettel-ref-highlight-types)
                         nil t)))
  (message "Selected type: %s" type)
  (when (use-region-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (end (if (= end (point))
                   (min (point-max) (1+ end))
                 end))
           (text (buffer-substring-no-properties beg end))
           (highlight-id (org-zettel-ref-highlight-generate-id))
           (type-char (org-zettel-ref-highlight-type-to-char type)))
      (message "DEBUG: Using char '%s' for type '%s'" type-char type)
      
      (delete-region beg end)
      (goto-char beg)
      (let ((insert-text (format "%s <<hl-%s>> §%s{%s}"
                                text
                                highlight-id
                                type-char
                                text)))
        (message "DEBUG: Inserting: %s" insert-text)
        (insert insert-text))
      
      (org-zettel-ref-highlight-refresh))))

(defun org-zettel-ref-highlight-refresh ()
  "Refresh the display of all highlights in the current buffer."
  (interactive)
  (message "Refreshing highlights...")
  (remove-overlays (point-min) (point-max) 'org-zettel-ref-highlight t)
  
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
      (let* ((type-char (match-string 2))
             (type (org-zettel-ref-highlight-char-to-type type-char)))
        (message "DEBUG: Found mark with char '%s', mapped to type '%s'" 
                type-char type)
        (let ((config (cdr (assoc type org-zettel-ref-highlight-types))))
          (when config
            (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
              (overlay-put ov 'org-zettel-ref-highlight t)
              (overlay-put ov 'face (plist-get config :face)))))))))

(defun org-zettel-ref-toggle-target-display ()
  "Toggle whether to display target marks."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((showing nil))
      (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
        (let* ((target-start (match-beginning 0))
               (target-end (+ (match-end 1) 2))
               (overlays (overlays-in target-start target-end)))
          (dolist (ov overlays)
            (when (overlay-get ov 'org-zettel-ref-highlight)
              (setq showing (not (equal (overlay-get ov 'display) "")))
              (overlay-put ov 'display (if showing "" nil))))))
      (message "Target marks are now %s" (if showing "hidden" "visible")))))     

;;----------------------------------------------------------------------------
;; Synchronization
;;----------------------------------------------------------------------------  

(defun org-zettel-ref-get-source-from-overview ()
  "Get the corresponding source file path from the current overview buffer."
  (let* ((db (org-zettel-ref-ensure-db))
         (overview-file (buffer-file-name))
         (overview-id (gethash overview-file (org-zettel-ref-db-overview-paths db))))
    (when overview-id
      (let* ((overview-entry (gethash overview-id (org-zettel-ref-db-overviews db)))
             (ref-id (org-zettel-ref-overview-entry-ref-id overview-entry))
             (ref-entry (org-zettel-ref-db-get-ref-entry db ref-id)))
        (org-zettel-ref-ref-entry-file-path ref-entry)))))

;;----------------------------------------------------------------------------
;; Image Handling
;;----------------------------------------------------------------------------  

(defun org-zettel-ref-highlight--check-init ()
  "Check if initialization is complete."
  (unless (and org-zettel-ref-overview-file
               (stringp org-zettel-ref-overview-file)
               (file-exists-p org-zettel-ref-overview-file))
    (user-error "Please run M-x org-zettel-ref-init to initialize the system")))

(defun org-zettel-ref-highlight--ensure-image-dir ()
  "Ensure the Images directory exists in the overview file's directory."
  (org-zettel-ref-highlight--check-init)  ; First check initialization
  (let* ((overview-dir (file-name-directory 
                       (expand-file-name org-zettel-ref-overview-file)))
         (image-dir (expand-file-name "Images" overview-dir)))
    (unless (file-exists-p image-dir)
      (make-directory image-dir t))
    image-dir))

(defun org-zettel-ref-highlight--copy-image (source-path)
  "Copy an image to the Images directory and return the new relative path."
  (let* ((image-dir (org-zettel-ref-highlight--ensure-image-dir))
         (file-name (file-name-nondirectory source-path))
         ;; Generate a unique filename (using timestamp)
         (new-name (format "%s-%s" 
                          (format-time-string "%Y%m%d-%H%M%S")
                          file-name))
         (dest-path (expand-file-name new-name image-dir)))
    (copy-file source-path dest-path t)
    ;; Return the path relative to the overview file
    (concat "Images/" new-name)))

(defun org-zettel-ref-add-image ()
  "Add a highlight mark to the image at the current position and copy it to the Images directory."
  (interactive)
  (org-zettel-ref-highlight--check-init)
  (save-excursion
    (let ((context (org-element-context)))
      (when (and (eq (org-element-type context) 'link)
                 (string= (org-element-property :type context) "file"))
        (let* ((path (org-element-property :path context))
               (abs-path (expand-file-name path (file-name-directory (buffer-file-name))))
               (link-end (org-element-property :end context))
               (description (read-string "Image description (optional): ")))
          (when (and (string-match-p "\\.\\(jpg\\|jpeg\\|png\\|gif\\|svg\\|webp\\)$" path)
                    (file-exists-p abs-path))
            ;; Copy the image to the Images directory
            (let ((new-path (org-zettel-ref-highlight--copy-image abs-path)))
              ;; Move to the end of the line containing the link and insert a newline
              (goto-char link-end)
              (end-of-line)
              (insert "\n")
              ;; Add the highlight mark on the new line
              (let ((highlight-id (org-zettel-ref-highlight-generate-id)))
                (insert (format "<<hl-%s>> §i{%s|%s}"
                              highlight-id
                              new-path
                              (or description "")))
                (org-zettel-ref-highlight-refresh)))))))))

;;----------------------------------------------------------------------------
;; Highlight Editing
;;----------------------------------------------------------------------------

;; Constants
(defconst org-zettel-ref-highlight-threshold 100
  "Threshold for number of highlights to consider a file as large.")

(defun org-zettel-ref-count-highlights ()
  "Count total number of highlights in current buffer."
  (save-excursion
    (save-match-data
      (let ((count 0))
        (goto-char (point-min))
        (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
          (setq count (1+ count)))
        count))))

(defun org-zettel-ref-renumber-highlights-after-point (start-number)
  "Renumber all highlights after START-NUMBER."
  (save-excursion
    (save-match-data
      (let* ((total-highlights (org-zettel-ref-count-highlights))
             (is-large-file (> total-highlights org-zettel-ref-highlight-threshold))
             (processed 0)
             (new-number start-number))
        
        (message "Buffer size: %d" (buffer-size)) ;; Debug info
        ;; Move to the beginning of the buffer
        (goto-char (point-min))
        ;; Find and renumber all highlights
        (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
          (let* ((current-number (string-to-number (match-string 1)))  ; Get the number using group 1
                 (type-char (match-string 2))                          ; Get the type using group 2
                 (text (match-string 3)))                              ; Get the text using group 3
            
            (when (>= current-number start-number)
              ;; Replace only the number part, keep the format unchanged
              (goto-char (match-beginning 1))
              (delete-region (match-beginning 1) (match-end 1))
              (insert (number-to-string new-number))
              (setq new-number (1+ new-number)))))
        
        ;; Update the counter
        (setq-local org-zettel-ref-highlight-counter (1- new-number))))))

(defun org-zettel-ref-remove-marked ()
  "Remove the highlight mark at the cursor and renumber subsequent highlights."
  (interactive)
  (let ((pos (point))
        (found nil))
    (save-excursion
      ;; Find the highlight mark on the current line
      (beginning-of-line)
      (when (re-search-forward org-zettel-ref-highlight-regexp (line-end-position) t)
        (let* ((target-start (match-beginning 0))
               (target-end (match-end 0))
               (highlight-id (match-string 1))    ; Get the number using group 1
               (type-char (match-string 2))       ; Get the type using group 2
               (text (match-string 3))            ; Get the text using group 3
               (current-number (string-to-number highlight-id)))
          (setq found t)
          ;; Confirm deletion
          (when (y-or-n-p "Remove highlight mark? ")
            ;; Delete the mark and insert original text
            (delete-region target-start target-end)
            (goto-char target-start)
            (insert (propertize text 'face 'org-zettel-ref-highlight-face))
            ;; Renumber subsequent highlights
            (org-zettel-ref-renumber-highlights-after-point current-number)
            ;; Synchronize the overview file
            (org-zettel-ref-sync-highlights)))))
    ;; Message outside save-excursion
    (unless found
      (message "No highlight mark found at point"))))

;; Edit highlighted text
(defun org-zettel-ref-edit-highlight ()
  "Edit the highlighted text under the cursor."
  (interactive)
  (save-excursion
    (when (org-zettel-ref-highlight-at-point)
      (let* ((bounds (org-zettel-ref-highlight-get-bounds))
             (old-text (org-zettel-ref-highlight-get-text bounds))
             (type (org-zettel-ref-highlight-get-type bounds))
             (ref (org-zettel-ref-highlight-get-ref bounds))
             (new-text (read-string "Edit highlighted text: " old-text)))
        (unless (string= old-text new-text)
          (save-excursion
            (goto-char (car bounds))
            (delete-region (car bounds) (cdr bounds))
            (insert (format "<<hl-%s>> §%s{%s}"
                          ref type new-text))
            (org-zettel-ref-highlight-refresh)
            (org-zettel-ref-sync-highlights)))))))

(defun org-zettel-ref-edit-note ()
  "Edit the content of the current note."
  (interactive)
  (when (org-zettel-ref-highlight-at-point)
    (let* ((bounds (org-zettel-ref-highlight-get-bounds))
           (ref (org-zettel-ref-highlight-get-ref bounds))
           (type (org-zettel-ref-highlight-get-type bounds))
           (old-text (org-zettel-ref-highlight-get-text bounds)))
      (when (string= type "n")  ; Ensure it's a note type
        (let ((new-text (read-string "Edit note: " old-text)))
          (unless (string= old-text new-text)
            (save-excursion
              (goto-char (car bounds))
              (delete-region (car bounds) (cdr bounds))
              (insert (format "<<hl-%s>> §n{%s}"
                            ref new-text))
              (org-zettel-ref-highlight-refresh)
              (org-zettel-ref-sync-highlights))))))))

;;----------------------------------------------------------------------------
;; Helper Functions
;;----------------------------------------------------------------------------

(defun org-zettel-ref-highlight-at-point ()
  "Check if the cursor is within a highlight region."
  (save-excursion
    (let ((pos (point)))
      (beginning-of-line)
      (when (re-search-forward org-zettel-ref-highlight-regexp (line-end-position) t)
        (and (>= pos (match-beginning 0))
             (<= pos (match-end 0)))))))

(defun org-zettel-ref-highlight-get-bounds ()
  "Get the start and end positions of the current highlight."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward org-zettel-ref-highlight-regexp (line-end-position) t)
      (cons (match-beginning 0) (match-end 0)))))

(defun org-zettel-ref-highlight-get-text (bounds)
  "Get the highlighted text within the specified range."
  (save-excursion
    (goto-char (car bounds))
    (when (re-search-forward org-zettel-ref-highlight-regexp (cdr bounds) t)
      (match-string 3))))

(defun org-zettel-ref-highlight-get-type (bounds)
  "Get the highlighted type within the specified range."
  (save-excursion
    (goto-char (car bounds))
    (when (re-search-forward org-zettel-ref-highlight-regexp (cdr bounds) t)
      (match-string 2))))

(defun org-zettel-ref-highlight-get-ref (bounds)
  "Get the highlighted reference number within the specified range."
  (save-excursion
    (goto-char (car bounds))
    (when (re-search-forward org-zettel-ref-highlight-regexp (cdr bounds) t)
      (match-string 1))))

(defun org-zettel-ref-highlight-initialize-counter ()
  "Scan all highlight marks in the current buffer and initialize the counter to the maximum value."
  (save-excursion
    (goto-char (point-min))
    (let ((max-id 0))
      ;; Scan all highlight marks
      (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
        (when-let* ((id-str (match-string 1))
                    (id-num (string-to-number id-str)))
          (setq max-id (max max-id id-num))))
      ;; Set the counter to the maximum value found
      (setq-local org-zettel-ref-highlight-counter max-id))))

(defun org-zettel-ref-follow-link-and-highlight ()
  "Jump to the link target and highlight it."
  (let* ((link-prop (org-element-context))
         (target-file (org-element-property :path link-prop))
         (target-id (org-element-property :search-option link-prop)))
    (when (and target-file target-id)
      (find-file target-file)
      (goto-char (point-min))
      (when (re-search-forward (concat "<<" target-id ">>") nil t)
        (org-show-context)
        (recenter)))))

(defun org-zettel-ref-highlight-enable ()
  "Enable highlight mode and initialize the counter."
  ;; Ensure the buffer-local variable is set
  (make-local-variable 'org-zettel-ref-highlight-counter)
  ;; Initialize the counter
  (org-zettel-ref-highlight-initialize-counter)
  ;; Refresh display
  (org-zettel-ref-highlight-refresh))


(defun org-zettel-ref-highlight-debug-counter ()
  "Display the highlight counter status of the current buffer."
  (interactive)
  (let ((current-counter org-zettel-ref-highlight-counter)
        (max-found (org-zettel-ref-highlight-initialize-counter)))
    (org-zettel-ref-debug-message-category 'highlight
      "Current counter: %d, Maximum found in buffer: %d"
      current-counter max-found)))

(defun org-zettel-ref-highlight-debug-info ()
  "Display the highlight debugging information of the current buffer."
  (interactive)
  (org-zettel-ref-debug-message-category 'highlight
    "Current counter value: %s" org-zettel-ref-highlight-counter)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
        (cl-incf count)
        (org-zettel-ref-debug-message-category 'highlight
          "Found highlight #%d: %s" count (match-string 0)))
      (org-zettel-ref-debug-message-category 'highlight
        "Total found %d highlight marks" count))))

(defun org-zettel-ref-highlight-add-note ()
  "Add a standalone note, using the highlight system's ID counter."
  (interactive)
  (let* ((note-text (read-string "Insert note: "))
         (highlight-id (org-zettel-ref-highlight-generate-id)))
    (insert (format "<<hl-%s>> §n{%s}"
                   highlight-id
                   note-text))
    (org-zettel-ref-highlight-refresh)))

;; Modify after-change processing function
(defun org-zettel-ref-highlight-after-change (beg end _len)
  "Handle highlight updates after text changes."
  (save-excursion
    (goto-char beg)
    (let ((line-beg (line-beginning-position))
          (line-end (line-end-position)))
      (when (and (>= end line-beg)
                 (<= beg line-end)
                 (string-match org-zettel-ref-highlight-regexp
                             (buffer-substring-no-properties line-beg line-end)))
        ;; Refresh display
        (org-zettel-ref-highlight-refresh)
        ;; Synchronize to overview
        (when (and (boundp 'org-zettel-ref-overview-file)
                  org-zettel-ref-overview-file)
          (org-zettel-ref-sync-highlights))))))

(defun org-zettel-ref-highlight-debug-config ()
  "Display current highlight type configurations."
  (interactive)
  (message "Current highlight types:")
  (dolist (type-def org-zettel-ref-highlight-types)
    (let* ((type (car type-def))
           (config (cdr type-def))
           (char (plist-get config :char))
           (face (plist-get config :face))
           (name (plist-get config :name))
           (prefix (plist-get config :prefix)))
      (message "Type: %s\n  char: %s\n  face: %s\n  name: %s\n  prefix: %s"
               type char face name prefix))))


(defun org-zettel-ref-highlight-setup ()
  "Setup highlight system."
  (interactive)
  ;; 确保变量是 buffer-local
  (make-local-variable 'org-zettel-ref-highlight-counter)
  ;; 验证配置
  (unless (org-zettel-ref-highlight-validate-types)
    (org-zettel-ref-debug-message-category 'highlight 
      "Warning: Invalid highlight types configuration"))
  ;; 初始化计数器
  (org-zettel-ref-highlight-initialize-counter)
  ;; 刷新显示
  (org-zettel-ref-highlight-refresh)
  ;; 显示当前配置状态
  (org-zettel-ref-debug-message-category 'highlight 
    "Highlight system setup complete. Use M-x org-zettel-ref-highlight-debug-config to check configuration."))

;; 在初始化时设置高亮
(defun org-zettel-ref--setup-highlight (buffer)
  "Setup highlight for BUFFER."
  (with-current-buffer buffer
    (org-zettel-ref-highlight-setup)))

(defun org-zettel-ref-highlight-validate-types ()
  "Validate highlight types configuration."
  (let ((chars (make-hash-table :test 'equal))
        (valid t))
    (dolist (type-def org-zettel-ref-highlight-types)
      (let* ((type (car type-def))
             (config (cdr type-def))
             (char (plist-get config :char)))
        ;; Check required properties
        (unless (and (plist-get config :char)
                    (plist-get config :face)
                    (plist-get config :name)
                    (plist-get config :prefix))
          (message "Warning: Type %s missing required properties" type)
          (setq valid nil))
        ;; Check for duplicate chars
        (when (gethash char chars)
          (message "Warning: Duplicate character identifier %s" char)
          (setq valid nil))
        (puthash char type chars)))
    valid))

;; When highlight system is initialized, validate configuration.
(defun org-zettel-ref-highlight-initialize ()
  "Initialize highlight system and validate configuration."
  (unless (org-zettel-ref-highlight-validate-types)
    (message "Warning: Invalid highlight types configuration")))

(add-hook 'after-init-hook #'org-zettel-ref-highlight-initialize)

(defun org-zettel-ref-reset-org-element-cache ()
  "Reset the org-element cache for the current buffer."
  (interactive)
  (when (fboundp 'org-element-cache-reset)
    (org-element-cache-reset)
    (message "Org element cache has been reset for current buffer.")))

(defun org-zettel-ref-ensure-org-element-cache ()
  "Ensure the org-element cache is in a good state."
  (condition-case err
      (progn
        (when (and (boundp 'org-element-use-cache)
                   org-element-use-cache)
          (org-element-cache-reset)))
    (error
     (message "Error resetting org-element cache: %s" (error-message-string err))
     (when (boundp 'org-element-use-cache)
       (setq-local org-element-use-cache nil)))))

(defun org-zettel-ref-jump-to-source-highlight-from-overview ()
  "Jump from a highlight entry in the single overview file to the corresponding highlight in the source file.
This command should be called when the point is on a highlight entry's headline in the single overview file."
  (interactive)
  (unless (and org-zettel-ref-use-single-overview-file
               (string= (buffer-file-name (current-buffer)) (expand-file-name org-zettel-ref-single-overview-file-path)))
    (user-error "This command can only be used from the single overview file when single-file mode is active."))

  (save-excursion
    (org-back-to-heading t) ; Ensure point is at the beginning of the headline
    (let* ((props (org-entry-properties)) 
           (source-ref-id (cdr (assoc "SOURCE_REF_ID" props)))
           (original-hl-id (cdr (assoc "ORIGINAL_HL_ID" props))))
      (message "DEBUG: Jump command - Props: %S, Source_Ref_ID: %s, Original_HL_ID: %s" props source-ref-id original-hl-id)

      (unless source-ref-id
        (user-error "Could not find :SOURCE_REF_ID: property at current heading."))
      (unless original-hl-id
        (user-error "Could not find :ORIGINAL_HL_ID: property at current heading."))

      (let* ((db (org-zettel-ref-ensure-db))
             (ref-entry (org-zettel-ref-db-get-ref-entry db source-ref-id))
             (source-file-path (when ref-entry (org-zettel-ref-ref-entry-file-path ref-entry))))
        (if source-file-path
            (progn
              (message "DEBUG: Jump command - Source file path: %s" source-file-path)
              (let ((source-buffer (find-file source-file-path))) ; find-file switches buffer and selects window
                (with-current-buffer source-buffer
                  (widen)
                  (goto-char (point-min))
                  (let ((target-mark (concat "<<hl-" original-hl-id ">>"))
                        (case-fold-search nil))
                    (if (re-search-forward (regexp-quote target-mark) nil t)
                        (progn
                          (goto-char (match-beginning 0))
                          (org-reveal)
                          (recenter)
                          (message "Jumped to %s in %s" target-mark (file-name-nondirectory source-file-path)))
                      (user-error "Target mark %s not found in %s" target-mark source-file-path))))))
          (user-error "Could not find source file for REF_ID: %s" source-ref-id))))))

(provide 'org-zettel-ref-highlight)
