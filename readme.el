;; readme.el  -*- lexical-binding: t; -*-
(message "default-directory: %s" default-directory)
(let ((dir (file-name-directory load-file-name)))
  (load (concat dir "ox-gfm/ox-gfm"))
  (load (concat dir "ox-md-title/ox-md-title"))
  (load (concat dir "ox-gfm-alerts.el")))

(require 'ox-gfm)
(require 'ox-md-title)
(require 'ox-gfm-alerts)

(defun readme/to-markdown (outfile alerts use-title-helper)
  "Export current buffer to OUTFILE using gfm or gfm-alerts backend.
ALERTS and USE-TITLE-HELPER are strings: \"true\"/\"false\"."
  (let* ((make-backup-files nil)
         (org-md-title (string= alerts alerts) ) ; keep var bound; real flag set below
         (use-title (string= (downcase (or use-title-helper "")) "true"))
         (backend (if (string= (downcase (or alerts "")) "true")
                      'gfm-alerts
                    'gfm)))
    (when use-title (org-md-title-add))
    (org-export-to-file backend outfile)))
