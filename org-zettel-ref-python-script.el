;;; org-zettel-ref-python-script.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Steve Chan
;;
;; Author: Steve Chan <yibie@outlook.com>
;; Maintainer: Steve Chan <yibie@outlook.com>
;; Created: August 31, 2024
;; Modified: August 31, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/chenyibin/org-zettel-ref-python-script
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


(defcustom org-zettel-ref-python-script
  'inline
  "Python conversion script specification.
This can be either:
- A string containing the full path to a Python script file
- The symbol 'inline, indicating that the script content is defined inline"
  :type '(choice (file :tag "Script file path")
                 (const :tag "Use inline script" inline))
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-python-script-content
  "
import sys

# Default Python script content
def main():
    # Default script logic
    print('Default org-zettel-ref conversion script')
    # Add your default script logic here

if __name__ == '__main__':
    main()
"
  "Inline Python conversion script content.
This is used when `org-zettel-ref-python-script` is set to 'inline."
  :type 'string
  :group 'org-zettel-ref)

(defun org-zettel-ref-get-python-script ()
  "Get the Python script content or file path based on the configuration."
  (if (eq org-zettel-ref-python-script 'inline)
      org-zettel-ref-python-script-content
    (when (and (stringp org-zettel-ref-python-script)
               (file-exists-p org-zettel-ref-python-script))
      org-zettel-ref-python-script)))

(defun org-zettel-ref-convert-to-org (file)
  "Convert FILE to org format using the external Python script."
  (interactive "fSelect file to convert: ")
  (let* ((input-file (expand-file-name file))
         (output-file (concat (file-name-sans-extension input-file) ".org")))
    (if (file-exists-p org-zettel-ref-python-script)
        (progn
          (call-process "python" nil "*conversion-output*" nil
                        org-zettel-ref-python-script input-file output-file)
          (if (file-exists-p output-file)
              (progn
                (find-file output-file)
                (message "File converted and opened: %s" output-file))
            (message "Conversion failed. Check *conversion-output* buffer for details.")))
      (message "Python script not found: %s" org-zettel-ref-python-script))))

(provide 'org-zettel-ref-python-script)
;;; org-zettel-ref-python-script.el ends here
