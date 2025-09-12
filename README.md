
# Generate repository README files from Org documents

A fork of [jeffkreeftmeijer/readme.el](https://github.com/jeffkreeftmeijer/readme.el) as a Github Action to avoid including uneccesary submodules in your repos


## Features

Adds support for \*G\*ithub \*F\*lavored \*M\*arkdown's alerts.

Turning:

```org
#+begin_note
This is a note
#+end_note
```

Into:

> [!NOTE]
> This is a note
> 

For all GFM alert styles *(NOTE, TIP, WARNING, CAUTION, IMPORTANT)*


## Usage

Include a github action by copying the following into a `.github/workflows/generate-markdown-readme.yaml` file (you can name it whatever you like)

*or*

Download the below [workflow](https://raw.githubusercontent.com/aidanprior/org-to-gfm-readme-md/refs/heads/main/example-workflow.yaml).

```yaml
name: Generate README.md from README.org
on:
  push:
    branches-ignore:
      - main                      # recommended to if commit: true
    paths:
      - '**/.org'
  workflow_dispatch:

jobs:
  generate:
    runs-on: ubuntu-latest
    permissions:
      contents: write             # needed if commit: true
    steps:
      - uses: aidanprior/org-to-gfm-readme-md@v1
	with:
	  org-path: README.org
	  md-path: README.md
	  alerts: true            # convert #+begin_note → [!NOTE]
	  use-title-helper: true  # Jeff's title tweak
	  commit: true            # auto-commit README.md
```