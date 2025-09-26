# Pre-commit Hook Implementation

This directory contains the pre-commit framework hook implementation for org-to-gfm conversion.

## Files

- `org-to-gfm-hook.sh`: The main hook script executed by pre-commit framework
- `README.md`: This documentation file

## How it works

The pre-commit framework passes the filenames of staged `.org` files as arguments to the hook script. The script then:

1. Checks for emacs availability
2. Loads the elisp file from `.githooks/org-to-gfm.el`
3. Converts each `.org` file to corresponding `.md` file
4. Adds the generated `.md` files to git staging area

## Testing

To test the hook locally:

1. Install pre-commit: `pip install pre-commit`
2. Set up a test repository with this hook
3. Create a `.org` file and stage it
4. Run `pre-commit run org-to-gfm` or commit to trigger the hook

## Requirements

- emacs must be available in PATH
- `.githooks/org-to-gfm.el` must exist (created by `install.sh`)