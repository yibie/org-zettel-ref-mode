;;; org-zettel-ref-org-roam.el --- Org-roam integration for org-zettel-ref -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains org-roam integration for org-zettel-ref.

;;; Code:

(require 'org-zettel-ref-core)

(declare-function org-roam-node-create "ext:org-roam" t)
(declare-function org-roam-node-slug "ext:org-roam" t)

(defvar org-zettel-ref-mode-type 'org-roam 
  "The type of mode to use for org-zettel-ref.
Can be `normal`, `denote`, or `org-roam`.")





(provide 'org-zettel-ref-org-roam)

;;; org-zettel-ref-org-roam.el ends here
