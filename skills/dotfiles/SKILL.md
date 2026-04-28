---
name: dotfiles
description: Edit and manage dotfiles/configuration files. Use when the user mentions config, dotfiles, shell config, terminal config, editor config, gitconfig, zshrc, ghostty, alacritty, tmux, vim, emacs, spacemacs, or any tool configuration that lives in their dotfiles repo.
user_invocable: true
---

# Dotfiles

You are helping the user manage their dotfiles repository.

## Repository

- **Location:** `~/Projects/.dotfiles` (symlinked from `~/.dotfiles`)
- **Remote:** `github.com:ezmiller/dotfiles.git`
- **Branch:** `master`

## How configs are deployed

Config files in the repo are symlinked into their expected locations (e.g. `~/.config/ghostty/config` -> `~/.dotfiles/ghostty/config`). Always edit the real file in the dotfiles repo, not the symlink target — they're the same file, but be aware of the repo context for commits.

## Key directories

| Directory | What it configures |
|---|---|
| `ghostty/` | Ghostty terminal |
| `alacritty/` | Alacritty terminal |
| `zsh/`, `zshrc` | Zsh shell |
| `bash/`, `bashrc`, `bash_profile` | Bash shell |
| `shell/` | Shared shell config |
| `bin/` | Personal scripts on PATH |
| `emacs-29.d/`, `spacemacs`, `spacemacs-private/` | Emacs / Spacemacs |
| `vim/`, `vimrc` | Vim |
| `tmux/`, `tmuxinator/` | Tmux |
| `gitconfig`, `gitignore_global` | Git |
| `clojure/` | Clojure / deps.edn |
| `Brewfile` | Homebrew packages |

## Workflow

1. Edit the file in `~/.dotfiles/` (or via its symlink — same file)
2. Commit and push from the `~/.dotfiles` repo
3. If creating a new config file, remind the user they may need to add a symlink via the `install` script
