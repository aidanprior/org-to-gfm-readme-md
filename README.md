
# Generate repository README files from Org documents

A Github action to convert `org` READMEs to `markdown` for better integration with Github's web markdown viewer


## Introduction

A fork of [jeffkreeftmeijer/readme.el](https://github.com/jeffkreeftmeijer/readme.el) as a Github Action to avoid including uneccesary submodules in your repos

Adds support for <span class="underline">G</span>​ithub <span class="underline">F</span>​lavored <span class="underline">M</span>​arkdown's alerts.

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

For all GFM alert styles.

*(NOTE, TIP, WARNING, CAUTION, IMPORTANT)*


## Usage

Include a github action by copying the following into a `.github/workflows/generate-markdown-readme.yaml` file (you can name it whatever you like)

*or*

Download the below [workflow](https://raw.githubusercontent.com/aidanprior/org-to-gfm-readme-md/refs/heads/main/example-workflow.yaml) directly.

```yaml
name: Generate README.md from README.org
on:
  push:
    branches-ignore:
      - main                      # recommended to if force-amend: true
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
	  commit: true            # auto-commit README.md
```


### Options

Use the following options in the `with:` block

| option           | default                                        | description                                          | notes                                                                                       |
|---------------- |---------------------------------------------- |---------------------------------------------------- |------------------------------------------------------------------------------------------- |
| `org-path`       | `README.org`                                   | Path to the source Org file                          |                                                                                             |
| `md-path`        | `README.md`                                    | Output Markdown path                                 |                                                                                             |
| `alerts`         | `true`                                         | Convert Org special blocks to GitHub alert callouts  | As described in the [Introduction](#org77a6b74)                                             |
| `title`          | `true`                                         | Insert document title from org's `#+title:` property | [See Jeff's Tweaks](https://github.com/jeffkreeftmeijer/ox-md-title.el/blob/main/README.md) |
| `commit`         | `false`                                        | Add/commit/push the generated file                   | 1. Requires `permissions: contents: write` on the job                                       |
| `force-amend`    | `false`                                        | Amend the current commit instead of adding a new one | Requires `commit: true`, Recommended to use with `branches-ignore: [ main ]`                |
| `git-user-name`  | `github-actions[bot]`                          | Git username to use when committing                  |                                                                                             |
| `git-user-email` | `github-actions[bot]@users.noreply.github.com` | Git email to use when committing                     |                                                                                             |