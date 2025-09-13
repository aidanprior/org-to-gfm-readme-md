
# A git Hook to Generate Github Flavored Markdown files from Org documents

Github's web markdown viewer supports `README.org` files, but its feature list is lacking compared to `README.md` files This precommit script tries to reconcile that, by converting org files to markdown file.


## About

A fork of [jeffkreeftmeijer/readme.el](https://github.com/jeffkreeftmeijer/readme.el) as a portable git hook to avoid including uneccesary submodules in your repos


## Features

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


## Getting Started


### Prerequisites

It assumes you have `emacs` installed.

*This feels like a fairly safe assumption if you are editing `org` files.*


### Installation

Install with the following curl command:

> [!WARNING]
> Always inspect scripts you intend to run on your computer. Do not trust me.
> 

```sh
curl -fsSL "https://raw.githubusercontent.com/aidanprior/org-to-gfm-readme-md/refs/heads/main/install.sh" | sh
```

As you can see, it creates the pre-commit script files, and configures `git` to use them. From there, follow the next steps. Commit org files, and watch them convert automatically


## Bugs

If you encounter any bugs, please use the issues tab. If you can fix them yourself, please still create an issue and then a pull request that mentions it
