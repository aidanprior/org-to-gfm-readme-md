#!/bin/sh
set -eu

# ---- Options handling ----
ELISP_ONLY=false
HELP=false

while [ $# -gt 0 ]; do
  case $1 in
    --elisp-only)
      ELISP_ONLY=true
      shift
      ;;
    --help|-h)
      HELP=true
      shift
      ;;
    *)
      echo "Unknown option: $1" >&2
      exit 1
      ;;
  esac
done

if [ "$HELP" = true ]; then
  cat <<EOF
Usage: $0 [OPTIONS]

Install org-to-gfm conversion tool for git hooks.

OPTIONS:
  --elisp-only    Install only elisp files, skip git hooks setup
                  (useful when using with pre-commit framework)
  --help, -h      Show this help message

EXAMPLES:
  # Full installation with manual git hooks
  $0

  # Install elisp only (for use with pre-commit framework)
  $0 --elisp-only
EOF
  exit 0
fi

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

(defun org-gfm-alerts--ensure-nl (s)
  (if (string-match-p "\n\\'" s) s (concat s "\n")))

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

(defun org-gfm-alerts--wrap-details (body)
  (concat "<details>\n<summary>Results</summary>\n\n" body "\n</details>\n"))

(defun org-gfm-alerts-example-block (example-block _contents info)
  (let* ((code (org-export-format-code-default example-block info))
         (code* (org-gfm-alerts--ensure-nl code)))
    (org-gfm-alerts--wrap-details (concat "```\n" code* "```"))))

(defun org-gfm-alerts-fixed-width (fixed _contents _info)
  (let* ((raw (or (org-element-property :value fixed) ""))
         (raw* (org-gfm-alerts--ensure-nl raw)))
    (org-gfm-alerts--wrap-details (concat "```\n" raw* "```"))))

(org-export-define-derived-backend 'gfm-alerts 'gfm
  :translate-alist '((special-block . org-gfm-alerts-special-block)
                     (example-block . org-gfm-alerts-example-block)
                     (fixed-width   . org-gfm-alerts-fixed-width)))

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
if [ "$ELISP_ONLY" = false ]; then
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

emacs -Q --batch \\
  --eval '(setq load-no-message t inhibit-message t message-log-max nil)' \\
  -l "\$ELISP" \\
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
fi

echo "✅ Installed:"
echo "  - $ELISP_FILE  (ox-gfm @ $OX_GFM_REF, ox-md-title @ $OX_MD_TITLE_REF)"

if [ "$ELISP_ONLY" = false ]; then
  echo "  - $HOOKS_DIR/pre-commit"
  echo "Git configured: core.hooksPath -> $HOOKS_DIR"
  echo "
Manual git hooks installation complete!

Next (for manual git hooks):
  git add $ELISP_FILE $HOOKS_DIR/pre-commit
  git commit -m 'chore: installed org→gfm pre-commit'
  # Test: edit & stage an .org file, then 'git commit'

Alternative: Use with pre-commit framework
  Instead of the above, you can use this with the pre-commit framework:
  1. Install pre-commit: pip install pre-commit
  2. Create .pre-commit-config.yaml with:
     repos:
       - repo: https://github.com/aidanprior/org-to-gfm-readme-md
         rev: main
         hooks:
           - id: org-to-gfm
  3. Run: pre-commit install
  4. The elisp file ($ELISP_FILE) is still needed for pre-commit framework.
"
else
  echo "
Elisp dependencies installed!

This installation is suitable for use with the pre-commit framework.

Next steps for pre-commit framework:
  1. Install pre-commit: pip install pre-commit
  2. Create .pre-commit-config.yaml with:
     repos:
       - repo: https://github.com/aidanprior/org-to-gfm-readme-md
         rev: main
         hooks:
           - id: org-to-gfm
  3. Run: pre-commit install
  4. Test: git add some .org file && git commit

To use manual git hooks instead, run this script without --elisp-only.
"
fi
