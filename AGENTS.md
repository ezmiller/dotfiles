# Agent Development Guide

## Build/Install Commands
- **Install dotfiles**: `./install` (idempotent, safe to run multiple times)
- **Install Alacritty**: `./install-alacritty.sh`
- **Update dotfiles**: `dfu` (alias: cd ~/.dotfiles && git pull --ff-only && ./install -q)
- **Install Homebrew packages**: `brew bundle`
- **Update submodules**: See "Updating Git Submodules" section below

## Install System Architecture

### Dotbot Framework
This repository uses **Dotbot** — a declarative configuration tool for managing dotfiles through symlinks.

**How it works:**
1. **`./install` script** (entry point):
   - Updates the `.dotbot` git submodule
   - Executes Dotbot with `.install.conf.yaml` configuration

2. **`.install.conf.yaml`** (configuration blueprint):
   - **`clean`**: Removes broken symlinks in `~` and `~/.config`
   - **`link`**: Creates symlinks from repo files → home directory (e.g., `~/.bashrc` → `~/.dotfiles/.bashrc`)
   - **`create`**: Ensures required directories exist
   - **`shell`**: Runs post-install commands (submodules, brew bundle, Alacritty build)

3. **Symlink approach**:
   - Configuration files remain in the repository
   - Home directory contains symlinks pointing to repo files
   - **Implication**: Editing `~/.bashrc` directly modifies `~/.dotfiles/.bashrc` (ready to commit)

4. **Idempotency**:
   - Safe to run `./install` multiple times without breaking existing setup
   - Automatically relinks changed files
   - Non-destructive to manual changes in home directory

**Key files:**
- `install` — Main entry script
- `.install.conf.yaml` — Symlink and command definitions
- `.dotbot/` — Dotbot framework (git submodule)

### ⚠️ Symlinked Directory Complexity

**Important**: Some directories are symlinked from `~` directly into the repository (e.g., `~/.nvm`, `~/.pyenv`, `~/.rbenv`). This creates a unique challenge:

**The problem:**
- These directories contain both **configuration files** (should be tracked) and **dynamic runtime files** (should NOT be tracked)
- Because they're symlinked, dynamic files can appear in `git status` and risk being committed

**Examples of dynamic files that should NOT be committed:**
- npm/node cache and installed packages in `~/.nvm`
- Python virtualenvs and cached bytecode in `~/.pyenv`
- Ruby gems and build artifacts in `~/.rbenv`
- Downloaded/cached files from package managers

**Best practices:**
1. **Always check `git status` carefully** before committing
2. **Never use `git add .`** blindly — inspect what's being staged
3. **Review `.gitignore`** to ensure dynamic files are excluded
4. **Prefer explicit `git add <file>`** for specific configuration changes
5. **When in doubt**, ask before committing files in symlinked directories

**For AI agents**: When suggesting changes to or working with symlinked directories (`~/.nvm`, `~/.pyenv`, `~/.rbenv`, etc.), proactively remind the user about this complexity and recommend careful review of `git status` before committing.

## Updating Git Submodules

This repository includes multiple git submodules (dotbot, nvm, pyenv, rbenv, etc.). Some have **nested submodules** (e.g., dotbot contains lib/pyyaml).

### Update a single submodule to latest:

```bash
# Method 1: Update to latest from remote (recommended for dotbot)
cd .dotbot
git fetch origin
git checkout origin/master
git submodule update --init --recursive  # Update nested submodules
cd ..
git add .dotbot

# Method 2: Using git submodule command
git submodule update --remote --init --recursive .dotbot
```

### Verify the update:

```bash
# Check version
python3 .dotbot/bin/dotbot --version

# Check what changed
git diff --cached .dotbot
git submodule status

# View new commits
cd .dotbot && git log --oneline v1.19.1..HEAD | head -20
```

### Important notes:
- **Dotbot has nested submodules**: Always use `--recursive` flag or manually update `lib/pyyaml`
- **Test before committing**: Verify `python3 .dotbot/bin/dotbot --version` works
- **The install script**: Runs `git submodule update --init --recursive` on each run, so it will use the committed version
- **After committing**: Subsequent `./install` runs will automatically use the new submodule version

### Update all submodules:

```bash
git submodule update --remote --recursive
```

**Warning**: Be selective when updating all submodules. Some (like pyenv, rbenv, nvm) may introduce breaking changes. Review each update individually.

## Testing
- **Emacs Lisp tests**: `cd org-journal && make test` (runs ert-run-tests-batch-and-exit)
- **Single Emacs test**: `emacs -Q -batch -L . -l tests/org-journal-test -f ert-run-tests-batch-and-exit`
- **Compile Emacs Lisp**: `cd org-journal && make compile`

## Code Style

### Shell Scripts (.sh, .bash, .zsh)
- **Indentation**: 4 spaces (bash), standard for shell
- **Shebang**: Use `#!/usr/bin/env bash` or `#!/bin/bash`
- **Error handling**: Use `set -e` for failing on errors (see install script)
- **Naming**: lowercase with underscores (e.g., `path_remove`, `tunnel_to_dev`)
- **Functions**: No `function` keyword needed, use `func_name() { ... }`
- **Quoting**: Always quote variables: `"${variable}"` not `$variable`
- **Line endings**: LF (Unix-style)

### Emacs Lisp (.el)
- **Configuration**: Uses org-mode literate programming (init.org tangles to init.el)
- **Package manager**: straight.el + use-package
- **Indentation**: 2 spaces (Emacs Lisp standard)
- **Naming**: kebab-case for functions (e.g., `my-custom-function`)

### General
- **Encoding**: UTF-8
- **Trailing whitespace**: Remove
- **Final newline**: Required
- **Comments**: Use `#` for shell, `;` for Lisp, document complex logic
- **Git**: Commits should explain "why" not "what"
