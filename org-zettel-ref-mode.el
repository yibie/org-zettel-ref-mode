;;; org-zettel-ref-mode.el --- Zettelsken-style Reference Note in Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Yibie

;; Author: Yibie <yibie@outlook.com>
;; Version: 0.2.1
;; Package-Requires: ((emacs "29.1") (org "9.3"))
;; Keywords: outlines
;; URL: https://github.com/yibie/org-zettel-ref-mode

;;; Commentary:

;; This package provides a mode for creating and managing a reference
;; overview file for marked text and quick notes in Org mode.

;;; Code:

(require 'org-zettel-ref-core)
(require 'org-zettel-ref-ui)
(require 'org-zettel-ref-db)
(require 'org-zettel-ref-utils)

;;;###autoload
(define-minor-mode org-zettel-ref-mode
  "Minor mode for managing reference notes in Org mode."
  :init-value nil
  :lighter " ZettelRef"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd org-zettel-ref-quick-markup-key) #'org-zettel-ref-quick-markup)
            map)
  (if org-zettel-ref-mode
      (if (buffer-file-name)
          (org-zettel-ref-mode-enable)
        (setq org-zettel-ref-mode nil)
        (message "org-zettel-ref-mode can only be enabled in buffers with associated files."))
    (org-zettel-ref-mode-disable)))

(provide 'org-zettel-ref-mode)

;;; org-zettel-ref-mode.el ends here