;;; ox-gfm-alerts.el --- Org → GFM with GitHub alerts  -*- lexical-binding: t; -*-

(require 'ox)
(require 'ox-gfm)

(defconst org-gfm-alerts--map
  '(("note"      . "[!NOTE]")
    ("tip"       . "[!TIP]")
    ("important" . "[!IMPORTANT]")
    ("warning"   . "[!WARNING]")
    ("caution"   . "[!CAUTION]"))
  "Map Org special block types to GitHub alert headers.")

(defun org-gfm-alerts--blockquote (s)
  (when s
    (mapconcat (lambda (l) (concat "> " l))
               (split-string s "\n") "\n")))

(defun org-gfm-alerts--special-block (special-block contents info)
  (let* ((type (downcase (or (org-element-property :type special-block) "")))
         (tag  (cdr (assoc type org-gfm-alerts--map))))
    (if tag
        (concat (org-gfm-alerts--blockquote tag) "\n"
                (when (and contents (not (string-empty-p contents)))
                  (concat (org-gfm-alerts--blockquote contents) "\n")))
      (org-export-with-backend 'gfm special-block contents info))))

(org-export-define-derived-backend 'gfm-alerts 'gfm
  :translate-alist '((special-block . org-gfm-alerts--special-block)))

(provide 'ox-gfm-alerts)
;;; ox-gfm-alerts.el ends here
