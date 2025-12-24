# Agent Development Guide

## Build/Install Commands
- **Install dotfiles (quick)**: `./install` or `./install -q` (~0.5s, symlinks only - default mode)
- **Install dotfiles (full)**: `./install -f` or `./install --full` (~5-10min, includes submodules, brew, Alacritty build)
- **Update dotfiles**: `dfu` (alias: `cd ~/.dotfiles && git pull --ff-only && ./install -q`)
- **Install Alacritty**: `./install-alacritty.sh` (builds from source)
- **Install Homebrew packages**: `brew bundle`
- **Update submodules**: See "Updating Git Submodules" section below

### 🚀 Performance & Usage Guidelines

**Default Workflow** (use 99% of the time):
```bash
cd ~/.dotfiles
git pull
./install          # Fast! ~0.5 seconds
# or use the dfu alias
```

**Full Install** (use rarely):
```bash
./install --full   # Slow! ~5-10 minutes
```

**When to use Quick Mode** (`./install` or `./install -q`):
- ✅ After pulling dotfile changes from git
- ✅ After editing any config file (`.bashrc`, `.zshrc`, etc.)
- ✅ Daily/frequent updates
- ✅ Testing configuration changes
- ✅ The `dfu` alias uses this automatically

**When to use Full Mode** (`./install -f` or `./install --full`):
- ⚠️ First-time setup on a new machine
- ⚠️ After updating git submodules (pyenv, rbenv, nvm, dotbot, etc.)
- ⚠️ After modifying `Brewfile` (to install new packages)
- ⚠️ After updating Alacritty source code
- ⚠️ When troubleshooting - need to rebuild everything

**What each mode does:**

| Operation | Quick Mode | Full Mode |
|-----------|------------|-----------|
| Create/update symlinks | ✅ Yes | ✅ Yes |
| Clean broken symlinks | ✅ Yes | ✅ Yes |
| Update git submodules | ❌ No | ✅ Yes |
| Run `brew bundle` | ❌ No | ✅ Yes |
| Build Alacritty | ❌ No | ✅ Yes |
| **Time taken** | **~0.5s** | **~5-10min** |

## Install System Architecture

### Dotbot Framework
This repository uses **Dotbot** — a declarative configuration tool for managing dotfiles through symlinks.

**How it works:**
1. **`./install` script** (entry point):
   - **Default mode**: Fast symlink-only updates (`.install.conf.yaml`)
   - **Quick mode** (`-q` flag): Same as default, explicitly skips heavy operations
   - **Full mode** (`-f` flag): Includes all heavy operations (`.install.conf.full.yaml`)
     - Updates all git submodules (dotbot, nvm, pyenv, rbenv, etc.)
     - Runs `brew bundle` to install/update Homebrew packages
     - Builds and installs Alacritty from source
   - Executes Dotbot with appropriate configuration file

2. **Configuration files**:
   - **`.install.conf.yaml`**: Default/quick mode (symlinks only)
   - **`.install.conf.quick.yaml`**: Explicit quick mode (same as default)
   - **`.install.conf.full.yaml`**: Full mode with all heavy operations
   
   All configs include:
   - **`clean`**: Removes broken symlinks in `~` and `~/.config`
   - **`link`**: Creates symlinks from repo files → home directory (e.g., `~/.bashrc` → `~/.dotfiles/.bashrc`)
   - **`create`**: Ensures required directories exist
   - Full config adds **`shell`**: Runs post-install commands (submodules, brew bundle, Alacritty build)

3. **Symlink approach**:
   - Configuration files remain in the repository
   - Home directory contains symlinks pointing to repo files
   - **Implication**: Editing `~/.bashrc` directly modifies `~/.dotfiles/.bashrc` (ready to commit)

4. **Idempotency**:
   - Safe to run `./install` multiple times without breaking existing setup
   - Automatically relinks changed files
   - Non-destructive to manual changes in home directory
   - Quick mode is very fast for frequent updates
   - Full mode should be run occasionally or after pulling submodule changes

**Key files:**
- `install` — Main entry script with mode selection
- `.install.conf.yaml` — Default mode config (symlinks only, ~0.5s)
- `.install.conf.quick.yaml` — Explicit quick mode config (identical to default)
- `.install.conf.full.yaml` — Full mode config (includes heavy operations)
- `.dotbot/` — Dotbot framework (git submodule)

**Performance:**
- **Quick/Default mode**: ~0.5 seconds (symlinks only)
- **Full mode**: Several minutes (compiles Alacritty, updates all submodules, runs brew bundle)

**When to use each mode:**
- **Default/Quick** (`./install` or `./install -q`): 
  - Daily updates after pulling changes
  - Testing config file changes
  - When you only changed dotfiles (not submodules or dependencies)
  - ✅ Use 99% of the time

- **Full** (`./install -f` or `./install --full`):
  - First-time setup on a new machine
  - After updating git submodules (dotbot, pyenv, rbenv, nvm, etc.)
  - After modifying `Brewfile` (adding/removing packages)
  - After updating Alacritty source code
  - When something isn't working and you want to rebuild everything
  - ⚠️ Use rarely, only when needed

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
- **The install script**: In full mode (`./install -f`), runs `git submodule update --init --recursive` to update all submodules
- **Quick/default mode**: Does NOT update submodules (fast!)
- **After committing submodule changes**: Run `./install -f` to update to the committed version

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
