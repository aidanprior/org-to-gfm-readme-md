
# A git Hook to Generate Github Flavored Markdown files from Org documents

Github's web markdown viewer supports `README.org` files, but its feature list is lacking compared to `README.md` files.

This precommit script tries to reconcile that, by converting org files to markdown file.


## About

A fork of [jeffkreeftmeijer/readme.el](https://github.com/jeffkreeftmeijer/readme.el) as a portable git hook to avoid including unnecessary submodules in your repos


## Features

1.  Adds support for <span class="underline">G</span>​ithub <span class="underline">F</span>​lavored <span class="underline">M</span>​arkdown's alerts.
    
    Turning:

```org
#+begin_note
  This is a note
#+end_note
```

Into:

> [!NOTE]
> This is a note

For all GFM alert styles.

*(NOTE, TIP, WARNING, CAUTION, IMPORTANT)*

1.  Adds support for `org`'s results blocks as `<details>` html code that is rendered nicely by Github For example

```sh
cowsay "That's pretty neat!"
```

<details>
<summary>Results</summary>

```
 _____________________
< That's pretty neat! >
 ---------------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
```
</details>


## Getting Started


### Prerequisites

It assumes you have `emacs` installed.

*This feels like a fairly safe assumption if you are editing `org` files.*


### Installation

You can use this tool in two ways:


#### Option 1: Manual Git Hooks Installation

Install with the following curl command:

> [!WARNING]
> Always inspect scripts you intend to run on your computer. Do not trust me.

```sh
curl -fsSL "https://raw.githubusercontent.com/aidanprior/org-to-gfm-readme-md/refs/heads/main/install.sh" | sh
```

```sh
➡️  Fetching pinned elisp…
✅ Installed:
  - .githooks/org-to-gfm.el  (ox-gfm @ bd85f6a, ox-md-title @ 0.3.0)
  - .githooks/pre-commit
Git configured: core.hooksPath -> .githooks

Manual git hooks installation complete!

Next (for manual git hooks):
  git add .githooks/org-to-gfm.el .githooks/pre-commit
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
  4. The elisp file (.githooks/org-to-gfm.el) is still needed for pre-commit framework.

```

As you can see, it creates the pre-commit script files, and configures `git` to use them. From there, follow the next steps. Commit org files, and watch them convert automatically


#### Option 2: Pre-commit Framework (Recommended)

If you prefer using the [pre-commit framework](https://pre-commit.com/), you can use this hook without running the install script:

1. Install pre-commit:
   ```sh
   pip install pre-commit
   ```

2. First, you still need the elisp dependencies. Run just the elisp setup:
   ```sh
   curl -fsSL "https://raw.githubusercontent.com/aidanprior/org-to-gfm-readme-md/refs/heads/main/install.sh" | sh -s -- --elisp-only
   ```

3. Create `.pre-commit-config.yaml` in your repository:
   ```yaml
   repos:
     - repo: https://github.com/aidanprior/org-to-gfm-readme-md
       rev: main  # or use a specific tag/commit
       hooks:
         - id: org-to-gfm
   ```

4. Install the pre-commit hook:
   ```sh
   pre-commit install
   ```

5. Test it:
   ```sh
   # Edit an .org file, stage it, and commit
   git add README.org
   git commit -m "test conversion"
   ```

The pre-commit framework approach gives you better control over hook execution and integrates well with other code quality tools.


#### Which Option Should You Choose?

- **Manual Git Hooks** (Option 1): 
  - ✅ Simple, standalone solution
  - ✅ No additional Python dependencies
  - ✅ Works immediately after running install.sh
  - ❌ Less integration with modern development workflows
  - ❌ Harder to manage multiple hooks

- **Pre-commit Framework** (Option 2): 
  - ✅ Better integration with modern development workflows
  - ✅ Easy to manage multiple hooks and formatting tools
  - ✅ Supports hook versioning and updates
  - ✅ Can run hooks on CI/CD systems
  - ❌ Requires Python and pre-commit installation
  - ❌ Slightly more complex setup

Both options use the same core conversion logic and produce identical results.


## Bugs

If you encounter any bugs, please use the issues tab. If you can fix them yourself, please still create an issue and then a pull request that mentions it
