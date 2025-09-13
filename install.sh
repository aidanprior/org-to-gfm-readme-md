#!/bin/sh
set -eu

# ---- Pins (use tags or SHAs for reproducibility) ----
OX_GFM_REF="${OX_GFM_REF:-bd85f6a}"         # e.g., 1b6b3a9
OX_MD_TITLE_REF="${OX_MD_TITLE_REF:-0.3.0}"  # e.g., v0.1.0 or SHA

HOOKS_DIR=".githooks"
ELISP_FILE="$HOOKS_DIR/org-to-gfm.el"

need() { command -v "$1" >/dev/null 2>&1 || { echo "Missing: $1" >&2; exit 1; }; }
fetch() {
  url="$1"; dest="$2"
  if command -v curl >/dev/null 2>&1; then
    curl -fsSL "$url" -o "$dest"
  else
    wget -qO "$dest" "$url"
  fi
}

need emacs
if ! command -v curl >/dev/null 2>&1 && ! command -v wget >/dev/null 2>&1; then
  echo "Need curl or wget" >&2; exit 1
fi

mkdir -p "$HOOKS_DIR"
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT

echo "➡️  Fetching pinned elisp…"
fetch "https://raw.githubusercontent.com/larstvei/ox-gfm/$OX_GFM_REF/ox-gfm.el" "$tmp/ox-gfm.el"
fetch "https://raw.githubusercontent.com/jeffkreeftmeijer/ox-md-title/$OX_MD_TITLE_REF/ox-md-title.el" "$tmp/ox-md-title.el"

# ---- Build org-to-gfm.el (single file, includes both deps) ----
cat >"$ELISP_FILE" <<'EOF'
;;; readme-inline.el --- Inline Org→GFM exporter  -*- lexical-binding: t; -*-
;; Inlines ox-gfm.el (GPLv3, © Lars Tveito) and ox-md-title.el (© Jeff Kreeftmeijer).
;; Keep embedded license headers intact when updating.

;; Quiet batch mode
(setq load-no-message t
      inhibit-message t
      message-log-max nil)

;; Org exporter core (built-ins)
(require 'ox) (require 'ox-md) (require 'ox-publish)

;;; --------------------------- BEGIN ox-md-title.el ---------------------------
EOF
cat "$tmp/ox-md-title.el" >>"$ELISP_FILE"
cat >>"$ELISP_FILE" <<'EOF'

;;; --------------------------- BEGIN ox-gfm.el -------------------------------
EOF
cat "$tmp/ox-gfm.el" >>"$ELISP_FILE"
cat >>"$ELISP_FILE" <<'EOF'
;;; ---------------- gfm-alerts + collapsible RESULTS -----------------
;;; ox-gfm-alerts.el --- Alerts + <details>-wrapped RESULTS  -*- lexical-binding: t; -*-

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
               (split-string (or s "") "\n") "\n")))

(defun org-gfm-alerts--blank-p (s)
  (or (null s) (string-match-p "\\`[ \t\n\r]*\\'" s)))

(defun org-gfm-alerts-special-block (special-block contents info)
  "Render Org special blocks as GitHub Alerts; fallback to gfm."
  (let* ((type (downcase (or (org-element-property :type special-block) "")))
         (tag  (cdr (assoc type org-gfm-alerts--map))))
    (if tag
        (concat (org-gfm-alerts--blockquote tag) "\n"
                (unless (org-gfm-alerts--blank-p contents)
                  (concat (org-gfm-alerts--blockquote (org-trim contents)) "\n")))
      (org-export-with-backend 'gfm special-block contents info))))

(defun org-gfm-alerts-example-block (example-block _contents info)
  "Render example/RESULTS blocks as a collapsible <details> with fenced code."
  (let ((code (org-export-format-code-default example-block info)))
    (concat
     "<details>\n<summary>Results</summary>\n\n"
     "```\n" code "```\n"
     "\n</details>\n")))

(org-export-define-derived-backend 'gfm-alerts 'gfm
  :translate-alist '((special-block . org-gfm-alerts-special-block)
                     (example-block . org-gfm-alerts-example-block)))

(provide 'ox-gfm-alerts)
;;; ox-gfm-alerts.el ends here

;;; --------------------------- wrapper entrypoint ----------------------------
(defun readme/to-markdown (outfile)
  "Export current buffer to OUTFILE via GFM with alerts + title helper enabled."
  (let* ((make-backup-files nil)
         (org-md-title t)            
         (backend 'gfm-alerts))
    (org-md-title-add)
    (unwind-protect
        (org-export-to-file backend outfile)
      (org-md-title-remove))))

(provide 'readme-inline)
;;; readme-inline.el ends here
EOF

# ---- Create pre-commit hook (POSIX, one quiet Emacs run) ----
cat >"$HOOKS_DIR/pre-commit" <<EOF
#!/bin/sh
set -eu
[ "\${SKIP_ORG_MD-}" = "1" ] && { echo "[pre-commit] Skipping"; exit 0; }
command -v emacs >/dev/null 2>&1 || { echo "[pre-commit] ERROR: emacs not found" >&2; exit 1; }

ELISP="./$ELISP_FILE"

FILES=\$(git diff --cached --name-only --diff-filter=ACM -- '*.org' || true)
[ -z "\$FILES" ] && { echo "[pre-commit] No staged .org files; nothing to do."; exit 0; }

PAIRS="'("
for org in \$FILES; do
  md=\$(printf '%s' "\$org" | sed 's/\\.org\$/\\.md/')
  q_org=\$(printf '%s' "\$org" | sed 's/"/\\"/g')
  q_md=\$(printf '%s' "\$md"  | sed 's/"/\\"/g')
  PAIRS="\$PAIRS(\"\$q_org\" . \"\$q_md\") "
done
PAIRS="\$PAIRS)"

emacs -Q --batch \
  --eval '(setq load-no-message t inhibit-message t message-log-max nil)' \
  -l "\$ELISP" \
  --eval "(let* ((pairs \$PAIRS))
           (dolist (p pairs)
             (let* ((org (car p)) (md (cdr p))
                    (buf (find-file-noselect org)))
               (with-current-buffer buf
                 (readme/to-markdown md))
               (kill-buffer buf))))"

for org in \$FILES; do
  md=\$(printf '%s' "\$org" | sed 's/\\.org\$/\\.md/')
  git add -- "\$md"
done
echo "[pre-commit] Org→MD done"
EOF
chmod +x "$HOOKS_DIR/pre-commit"

# ---- Configure git hooks ----
git config core.hooksPath "$HOOKS_DIR"

echo "✅ Installed:"
echo "  - $ELISP_FILE  (ox-gfm @ $OX_GFM_REF, ox-md-title @ $OX_MD_TITLE_REF)"
echo "  - $HOOKS_DIR/pre-commit"
echo "Git configured: core.hooksPath -> $HOOKS_DIR"
echo "
Next:
  git add $ELISP_FILE $HOOKS_DIR/pre-commit
  git commit -m 'chore: installed org→gfm pre-commit'
  # Test: edit & stage an .org file, then 'git commit'
"
