#!/bin/sh
set -eu

# Skip if environment variable is set
[ "${SKIP_ORG_MD-}" = "1" ] && { echo "[org-to-gfm] Skipping"; exit 0; }

# Check if emacs is available
command -v emacs >/dev/null 2>&1 || { echo "[org-to-gfm] ERROR: emacs not found" >&2; exit 1; }

# Path to elisp file (relative to repo root)
ELISP_FILE=".githooks/org-to-gfm.el"

# Check if elisp file exists
[ ! -f "$ELISP_FILE" ] && {
    echo "[org-to-gfm] ERROR: $ELISP_FILE not found. Run install.sh first or ensure elisp file exists." >&2
    exit 1
}

# Get the .org files passed as arguments from pre-commit
[ $# -eq 0 ] && { echo "[org-to-gfm] No .org files to process."; exit 0; }

# Build pairs for emacs processing
PAIRS="'("
for org in "$@"; do
    md=$(printf '%s' "$org" | sed 's/\.org$/\.md/')
    q_org=$(printf '%s' "$org" | sed 's/"/\\"/g')
    q_md=$(printf '%s' "$md" | sed 's/"/\\"/g')
    PAIRS="$PAIRS(\"$q_org\" . \"$q_md\") "
done
PAIRS="$PAIRS)"

# Process files with emacs
emacs -Q --batch \
    --eval '(setq load-no-message t inhibit-message t message-log-max nil)' \
    -l "$ELISP_FILE" \
    --eval "(let* ((pairs $PAIRS))
               (dolist (p pairs)
                 (let* ((org (car p)) (md (cdr p))
                        (buf (find-file-noselect org)))
                   (with-current-buffer buf
                     (readme/to-markdown md))
                   (kill-buffer buf))))"\
    || exit 1

# Add generated markdown files to git
for org in "$@"; do
    md=$(printf '%s' "$org" | sed 's/\.org$/\.md/')
    git add -- "$md"
done

echo "[org-to-gfm] Converted $# .org file(s) to .md"
