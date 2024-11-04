;;; org-zettel-ref-migrate-test.el --- Tests for org-zettel-ref migration -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-zettel-ref-migrate)

(defvar org-zettel-ref-test-old-data
  '((overview-index . #s(hash-table test equal data 
     ("/Users/test/ref/test1.org" "/Users/test/notes/overview1.org"
      "/Users/test/ref/test2.org" "/Users/test/notes/overview2.org")))
    (reverse-index . #s(hash-table test equal data 
     ("/Users/test/notes/overview1.org" "/Users/test/ref/test1.org"
      "/Users/test/notes/overview2.org" "/Users/test/ref/test2.org"))))
  "Sample old format data for testing.")

(ert-deftest org-zettel-ref-test-migrate ()
  "Test migration from old format to new database structure."
  (let* ((old-data org-zettel-ref-test-old-data)
         (new-db (org-zettel-ref-migrate-from-old-format old-data)))
    
    ;; 测试基本结构
    (should (org-zettel-ref-db-p new-db))
    (should (string= (org-zettel-ref-db-version new-db) "1.0"))
    
    ;; 打印调试信息
    (message "\nDEBUG: Test Results:")
    (message "Refs count: %d" (hash-table-count (org-zettel-ref-db-refs new-db)))
    (message "Overviews count: %d" (hash-table-count (org-zettel-ref-db-overviews new-db)))
    (message "Maps count: %d" (hash-table-count (org-zettel-ref-db-map new-db)))
    
    ;; 测试数量
    (should (= (hash-table-count (org-zettel-ref-db-refs new-db)) 2))
    (should (= (hash-table-count (org-zettel-ref-db-overviews new-db)) 2))
    (should (= (hash-table-count (org-zettel-ref-db-map new-db)) 2))
    
    ;; 打印详细内容
    (message "\nDEBUG: References:")
    (maphash (lambda (k v) 
               (message "  ID: %s, Path: %s" 
                       k (org-zettel-ref-ref-entry-file-path v)))
             (org-zettel-ref-db-refs new-db))
    
    (message "\nDEBUG: Overviews:")
    (maphash (lambda (k v) 
               (message "  ID: %s, Path: %s" 
                       k (org-zettel-ref-overview-entry-file-path v)))
             (org-zettel-ref-db-overviews new-db))))

(defun org-zettel-ref-run-tests ()
  "Run all org-zettel-ref migration tests."
  (interactive)
  (ert-run-tests-interactively "^org-zettel-ref-test-"))

(provide 'org-zettel-ref-migrate-test)
;;; org-zettel-ref-migrate-test.el ends here 