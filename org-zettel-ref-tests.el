;;; org-zettel-ref-tests.el --- Tests for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains ERT tests for the org-zettel-ref package,
;; covering both multi-file and single-file note-saving styles.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ox-org) ; For org-element-parse-buffer
(require 'org-zettel-ref-core)
(require 'org-zettel-ref-db)
(require 'org-zettel-ref-utils) ;; For org-zettel-ref-highlight-regexp, etc.

;;---------------------------------------------------------------------
;;; Test Configuration & Helper Variables
;;---------------------------------------------------------------------

(defvar test-temp-dir nil "Temporary directory for test files.")
(defvar test-source-dir nil "Temporary directory for source files.")
(defvar test-overview-dir nil "Temporary directory for multi-file overviews.")
(defvar test-single-notes-file nil "Path for the single notes file.")
(defvar test-db-file nil "Path for the temporary database file.")

(defvar-local test-original-values nil
  "Store original values of customizable variables to restore them later.")

;; A simple highlight regexp for testing purposes
(defconst test-highlight-regexp "<<<\\(.*?\\)>>>"
  "A simple highlight regexp for tests: <<<TEXT>>>. Captures TEXT.")
(defconst test-highlight-types
  `(("t" . ((name . "TestHighlight")
             (prefix . "TEST:")
             (face . default)
             (template . "* %s %s\n:PROPERTIES:\n:HI_ID: [[hl:%s][hl-%s]]\n:END:\n")))))

;;---------------------------------------------------------------------
;;; Helper Functions
;;---------------------------------------------------------------------

(defun test-setup-temp-env ()
  "Set up temporary directories and files for a test."
  (setq test-temp-dir (make-temp-file "org-zettel-ref-tests_" t))
  (delete-file test-temp-dir)
  (make-directory test-temp-dir)

  (setq test-source-dir (expand-file-name "source/" test-temp-dir))
  (make-directory test-source-dir)
  (setq test-overview-dir (expand-file-name "overviews/" test-temp-dir))
  (make-directory test-overview-dir)
  (setq test-single-notes-file (expand-file-name "single-notes.org" test-temp-dir))
  (setq test-db-file (expand-file-name "test-db.sqlite" test-temp-dir))

  ;; Store original values and set test values
  (setq test-original-values
        `((org-zettel-ref-overview-directory . ,org-zettel-ref-overview-directory)
          (org-zettel-ref-single-notes-file-path . ,org-zettel-ref-single-notes-file-path)
          (org-zettel-ref-db-file . ,org-zettel-ref-db-file)
          (org-zettel-ref-highlight-regexp . ,org-zettel-ref-highlight-regexp)
          (org-zettel-ref-highlight-types . ,org-zettel-ref-highlight-types)
          (org-zettel-ref-db . ,org-zettel-ref-db)))

  (setq org-zettel-ref-overview-directory test-overview-dir)
  (setq org-zettel-ref-single-notes-file-path test-single-notes-file)
  (setq org-zettel-ref-db-file test-db-file)
  (setq org-zettel-ref-highlight-regexp test-highlight-regexp)
  (setq org-zettel-ref-highlight-types test-highlight-types)
  (setq org-zettel-ref-db nil) ; Ensure fresh DB for each test
  (org-zettel-ref-ensure-db)) ; Initialize the DB

(defun test-cleanup-temp-env ()
  "Clean up temporary files and directories after a test."
  (when (and test-temp-dir (file-exists-p test-temp-dir))
    (delete-directory test-temp-dir t t))
  (setq test-temp-dir nil
        test-source-dir nil
        test-overview-dir nil
        test-single-notes-file nil
        test-db-file nil)

  ;; Restore original values
  (dolist (pair test-original-values)
    (set (car pair) (cdr pair)))
  (setq test-original-values nil)
  (setq org-zettel-ref-db nil)) ; Reset DB variable

(defun test-create-source-file (filename &optional content)
  "Create a dummy source file FILENAME in test-source-dir with CONTENT."
  (let ((filepath (expand-file-name filename test-source-dir)))
    (with-temp-buffer
      (insert (or content (format "This is test file %s.\n" filename)))
      (write-file filepath t))
    filepath))

(defun test-get-file-content (filepath)
  "Return the content of FILEPATH as a string."
  (if (file-exists-p filepath)
      (with-temp-buffer
        (insert-file-contents filepath)
        (buffer-string))
    nil))

(defun test-parse-org-file (filepath)
  "Parse an Org file at FILEPATH and return the org-element structure."
  (when (file-exists-p filepath)
    (with-temp-buffer
      (insert-file-contents filepath)
      (org-element-parse-buffer))))

(defun test-find-heading-by-property (parsed-org property value &optional level)
  "Find a headline in PARSED-ORG by PROPERTY VALUE and optional LEVEL."
  (cl-find-if
   (lambda (el)
     (and (eq (org-element-type el) 'headline)
          (if level (= (org-element-property :level el) level) t)
          (string= (org-element-property property el) value)))
   (org-element-contents parsed-org)))

(defun test-find-subheading-by-property (parent-headline property value &optional level)
  "Find a subheading under PARENT-HEADLINE by PROPERTY and VALUE."
   (when parent-headline
     (cl-find-if
      (lambda (el)
        (and (eq (org-element-type el) 'headline)
             (if level (= (org-element-property :level el) level) t)
             (string= (org-element-property property el) value)))
      (org-element-contents parent-headline))))


(defmacro with-test-source-buffer ((filepath &optional content) &rest body)
  "Create source file, visit it in a temp buffer, and execute BODY.
FILEPATH is relative to test-source-dir."
  `(let ((source-file-path (test-create-source-file ,filepath ,content)))
     (with-temp-buffer
       (insert-file-contents source-file-path)
       (set-visited-file-name source-file-path) ; Critical for org-zettel-ref-init
       (setq-local org-zettel-ref-current-ref-entry nil) ; Ensure it's fresh
       (setq-local org-zettel-ref-overview-file nil)
       (setq-local org-zettel-ref-overview-buffer nil)
       ,@body)))

;;---------------------------------------------------------------------
;;; Test Suite Definition
;;---------------------------------------------------------------------

(ert-deftest-once-setup org-zettel-ref-tests-once-setup ()
  "Runs once before any tests in this file."
  (message "Starting org-zettel-ref tests..."))

(ert-deftest-once-teardown org-zettel-ref-tests-once-teardown ()
  "Runs once after all tests in this file."
  (message "Finished org-zettel-ref tests."))

(defmacro deftest-org-zettel-ref (name &rest body)
  "Define an ERT test with setup and teardown for org-zettel-ref."
  `(ert-deftest ,name ()
     (test-setup-temp-env)
     (unwind-protect
         (progn ,@body)
       (test-cleanup-temp-env))))

;;---------------------------------------------------------------------
;;; A. Multi-File Mode Tests
;;---------------------------------------------------------------------

(deftest-org-zettel-ref test-multi-file-note-creation-new-source ()
  (setq org-zettel-ref-note-saving-style 'multi-file)
  (let ((source-filename "test-source-multi-1.txt"))
    (with-test-source-buffer (source-filename "Source content for multi-file test 1.")
      ;; Simulate org-zettel-ref-init which calls ensure-entry
      (org-zettel-ref-init)
      
      (let* ((ref-entry org-zettel-ref-current-ref-entry)
             (overview-file-path org-zettel-ref-overview-file)
             (db org-zettel-ref-db))
        (should ref-entry "Ref entry should be created.")
        (should overview-file-path "Overview file path should be set.")
        (should (file-exists-p overview-file-path) "Overview file should be created on disk.")

        ;; Assert overview file content
        (let* ((overview-content (test-get-file-content overview-file-path))
               (parsed-overview (test-parse-org-file overview-file-path)))
          (should (string-match-p (format "#\\+SOURCE_FILE: %s" (buffer-file-name)) overview-content)
                  "Overview file should contain correct #+SOURCE_FILE property.")
          (should (test-find-heading-by-property parsed-overview :title (format "Overview - %s" (file-name-base source-filename)) 1)
                  "Overview file should contain a top-level heading for the note (checking title)."))

        ;; Assert Database entries
        (let* ((ref-id (org-zettel-ref-ref-entry-id ref-entry))
               (overview-entry (org-zettel-ref-db-get-overview-by-ref-id db ref-id)))
          (should (org-zettel-ref-db-get-ref-entry db ref-id) "Ref-entry should exist in DB.")
          (should overview-entry "Overview-entry should exist in DB.")
          (should (string= (org-zettel-ref-overview-entry-file-path overview-entry) overview-file-path)
                  "Overview-entry path should match.")
          (should (string= (org-zettel-ref-db-get-maps db ref-id) (org-zettel-ref-overview-entry-id overview-entry))
                  "DB map should link ref-id to overview-id."))))))

(deftest-org-zettel-ref test-multi-file-note-sync ()
  (setq org-zettel-ref-note-saving-style 'multi-file)
  (let ((source-filename "test-source-multi-sync.txt"))
    (with-test-source-buffer (source-filename "Content with <<<highlight1>>> and <<<highlight2>>>.")
      (org-zettel-ref-init) ; Create note and overview file
      (let ((overview-file-path org-zettel-ref-overview-file))
        (should overview-file-path "Overview file should be created before sync.")
        
        ;; Simulate adding highlights (text already in buffer) and call sync
        (org-zettel-ref-sync-highlights)
        
        (let ((parsed-overview (test-parse-org-file overview-file-path)))
          (should parsed-overview "Overview file should be parsable.")
          (let ((hl1 (test-find-heading-by-property parsed-overview :HI_ID "[[hl:1]]"))
                (hl2 (test-find-heading-by-property parsed-overview :HI_ID "[[hl:2]]")))
            (should hl1 "Highlight 1 should exist as a heading.")
            (should (string-match-p "TEST: highlight1" (org-element-property :raw-value hl1))
                    "Highlight 1 text is incorrect.")
            (should hl2 "Highlight 2 should exist as a heading.")
            (should (string-match-p "TEST: highlight2" (org-element-property :raw-value hl2))
                    "Highlight 2 text is incorrect.")))))))

;;---------------------------------------------------------------------
;;; B. Single-File Mode Tests
;;---------------------------------------------------------------------

(deftest-org-zettel-ref test-single-file-initialization ()
  (setq org-zettel-ref-note-saving-style 'single-file)
  (let ((source-filename "test-source-single-init.txt"))
    (with-test-source-buffer (source-filename "Initial content for single file mode.")
      (org-zettel-ref-init) ; This calls ensure-entry
      
      (should (file-exists-p test-single-notes-file) "Single notes file should be created.")
      (let ((notes-content (test-get-file-content test-single-notes-file)))
        (should (string-match-p "#\\+TITLE: Zettel Ref Notes" notes-content)
                "Single notes file should have correct title.")
        (should (string-match-p "#\\+OZREF_DB_ID: @SINGLE_FILE_MARKER@" notes-content)
                "Single notes file should have the correct DB ID marker."))
      
      (let ((db org-zettel-ref-db))
        (should db "Database should be initialized.")
        (let ((generic-overview (org-zettel-ref-db-get-overview db "@SINGLE_FILE_MARKER@")))
          (should generic-overview "Generic overview entry for single file should exist in DB.")
          (should (string= (org-zettel-ref-overview-entry-file-path generic-overview) test-single-notes-file)
                  "Generic overview entry should point to the single notes file."))))))

(deftest-org-zettel-ref test-single-file-note-creation-new-source ()
  (setq org-zettel-ref-note-saving-style 'single-file)
  (let ((source-filename "test-source-single-1.txt"))
    (with-test-source-buffer (source-filename "Source for single-file note creation.")
      (org-zettel-ref-init) ; Calls ensure-entry
      
      (let* ((ref-entry org-zettel-ref-current-ref-entry)
             (db org-zettel-ref-db)
             (parsed-single-notes (test-parse-org-file test-single-notes-file))
             (source-abs-path (expand-file-name source-filename test-source-dir)))
        (should ref-entry "Ref entry should be created.")
        (should parsed-single-notes "Single notes file should be parsable.")

        (let ((h1 (test-find-heading-by-property parsed-single-notes :SOURCE_FILE source-abs-path 1)))
          (should h1 "H1 for source file should exist in single notes file.")
          (should (string= (org-element-property :OZREF_ID h1) (org-zettel-ref-ref-entry-id ref-entry))
                  "H1 should have correct :OZREF_ID: property."))
        
        (should (string= (org-zettel-ref-db-get-maps db (org-zettel-ref-ref-entry-id ref-entry))
                         "@SINGLE_FILE_MARKER@")
                "DB map should link ref-id to @SINGLE_FILE_MARKER@.")))))

(deftest-org-zettel-ref test-single-file-note-creation-existing-source-heading ()
  (setq org-zettel-ref-note-saving-style 'single-file)
  (let ((source-filename "test-source-single-exist.txt")
        (source-abs-path (expand-file-name "test-source-single-exist.txt" test-source-dir)))
    (with-test-source-buffer (source-filename "Initial pass for existing source.")
      (org-zettel-ref-init)) ; First call, creates H1

    (let ((parsed-before (test-parse-org-file test-single-notes-file)))
      (should (= (length (org-element-contents parsed-before)) 1)
              "Should have one H1 after first init (ignoring non-headlines)."))

    (with-test-source-buffer (source-filename "Second pass for existing source.")
      (org-zettel-ref-init)) ; Second call, should use existing H1
      
    (let ((parsed-after (test-parse-org-file test-single-notes-file)))
      (should (= (length (org-element-contents parsed-after)) 1)
              "Should still have only one H1 after second init for same source."))))

(deftest-org-zettel-ref test-single-file-note-sync-new-source ()
  (setq org-zettel-ref-note-saving-style 'single-file)
  (let ((source-filename "test-sf-sync-new.txt")
        (source-abs-path (expand-file-name "test-sf-sync-new.txt" test-source-dir)))
    (with-test-source-buffer (source-filename "Single file sync <<<HL_A>>> and <<<HL_B>>>.")
      (org-zettel-ref-init)
      (org-zettel-ref-sync-highlights)

      (let* ((parsed-notes (test-parse-org-file test-single-notes-file))
             (h1 (test-find-heading-by-property parsed-notes :SOURCE_FILE source-abs-path 1)))
        (should h1 "H1 for source file should exist.")
        
        (let ((hl-a (test-find-subheading-by-property h1 :HI_ID "[[hl:1]]" 2))
              (hl-b (test-find-subheading-by-property h1 :HI_ID "[[hl:2]]" 2)))
          (should hl-a "Highlight A (H2) should exist under H1.")
          (should (string-match-p "TEST: HL_A" (org-element-property :raw-value hl-a))
                  "Highlight A text is incorrect.")
          (should hl-b "Highlight B (H2) should exist under H1.")
          (should (string-match-p "TEST: HL_B" (org-element-property :raw-value hl-b))
                  "Highlight B text is incorrect."))))))

(deftest-org-zettel-ref test-single-file-note-sync-multiple-sources ()
  (setq org-zettel-ref-note-saving-style 'single-file)
  (let ((source1-fname "test-sf-multi-src1.txt")
        (source2-fname "test-sf-multi-src2.org")
        (source1-abs-path (expand-file-name "test-sf-multi-src1.txt" test-source-dir))
        (source2-abs-path (expand-file-name "test-sf-multi-src2.org" test-source-dir)))

    (with-test-source-buffer (source1-fname "Source 1 has <<<S1_HL1>>>.")
      (org-zettel-ref-init)
      (org-zettel-ref-sync-highlights))
    
    (with-test-source-buffer (source2-fname "Source 2 has <<<S2_HL1>>> and <<<S2_HL2>>>.")
      (org-zettel-ref-init)
      (org-zettel-ref-sync-highlights))

    (let* ((parsed-notes (test-parse-org-file test-single-notes-file))
           (h1-src1 (test-find-heading-by-property parsed-notes :SOURCE_FILE source1-abs-path 1))
           (h1-src2 (test-find-heading-by-property parsed-notes :SOURCE_FILE source2-abs-path 1)))
      (should h1-src1 "H1 for source 1 should exist.")
      (should h1-src2 "H1 for source 2 should exist.")

      (let ((s1-hl1 (test-find-subheading-by-property h1-src1 :HI_ID "[[hl:1]]" 2)))
        (should s1-hl1 "S1_HL1 should exist under H1 for source 1.")
        (should (string-match-p "TEST: S1_HL1" (org-element-property :raw-value s1-hl1))))
      
      (let ((s2-hl1 (test-find-subheading-by-property h1-src2 :HI_ID "[[hl:1]]" 2))
            (s2-hl2 (test-find-subheading-by-property h1-src2 :HI_ID "[[hl:2]]" 2)))
        (should s2-hl1 "S2_HL1 should exist under H1 for source 2.")
        (should (string-match-p "TEST: S2_HL1" (org-element-property :raw-value s2-hl1)))
        (should s2-hl2 "S2_HL2 should exist under H1 for source 2.")
        (should (string-match-p "TEST: S2_HL2" (org-element-property :raw-value s2-hl2)))))))

(deftest-org-zettel-ref test-single-file-note-sync-update-existing ()
  (setq org-zettel-ref-note-saving-style 'single-file)
  (let ((source-filename "test-sf-sync-update.txt")
        (source-abs-path (expand-file-name "test-sf-sync-update.txt" test-source-dir)))
    ;; Initial sync with HL_OLD
    (with-test-source-buffer (source-filename "Content with <<<HL_OLD>>>.")
      (org-zettel-ref-init)
      (org-zettel-ref-sync-highlights))

    (let* ((parsed-notes1 (test-parse-org-file test-single-notes-file))
           (h1-1 (test-find-heading-by-property parsed-notes1 :SOURCE_FILE source-abs-path 1))
           (hl-old (test-find-subheading-by-property h1-1 :HI_ID "[[hl:1]]" 2)))
      (should hl-old "HL_OLD should exist after first sync.")
      (should (string-match-p "TEST: HL_OLD" (org-element-property :raw-value hl-old))))

    ;; Update source file and re-sync
    (with-test-source-buffer (source-filename "Content updated to <<<HL_NEW>>> and <<<HL_EXTRA>>>.")
      ;; Need to re-run init to set up buffer-local vars for this "session"
      (org-zettel-ref-init) 
      (org-zettel-ref-sync-highlights))

    (let* ((parsed-notes2 (test-parse-org-file test-single-notes-file))
           (h1-2 (test-find-heading-by-property parsed-notes2 :SOURCE_FILE source-abs-path 1)))
      (should h1-2 "H1 should still exist.")
      (let ((hl-old-after (test-find-subheading-by-property h1-2 :HI_ID "[[hl:1]]" 2)) ; HI_ID "1" was HL_OLD, now HL_NEW
            (hl-extra (test-find-subheading-by-property h1-2 :HI_ID "[[hl:2]]" 2)))
        (should hl-old-after "Subheading for HI_ID 1 should be updated to HL_NEW.")
        (should (string-match-p "TEST: HL_NEW" (org-element-property :raw-value hl-old-after))
                "Text for HI_ID 1 should be updated to HL_NEW.")
        (should hl-extra "HL_EXTRA should be added as a new subheading.")
        (should (string-match-p "TEST: HL_EXTRA" (org-element-property :raw-value hl-extra)))))))

(provide 'org-zettel-ref-tests)
;;; org-zettel-ref-tests.el ends here
