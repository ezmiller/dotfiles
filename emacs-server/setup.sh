#!/usr/bin/env bash
# Set up minimal Emacs config on a remote server
# Usage: curl -sL https://raw.githubusercontent.com/<user>/dotfiles/main/emacs-server/setup.sh | bash
#    or: bash ~/.dotfiles/emacs-server/setup.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EMACS_DIR="${HOME}/.emacs.d"

echo "==> Setting up server Emacs config..."

# Create .emacs.d if it doesn't exist
mkdir -p "$EMACS_DIR"

# Back up existing init.el if present
if [ -f "$EMACS_DIR/init.el" ]; then
  echo "    Backing up existing init.el to init.el.bak"
  cp "$EMACS_DIR/init.el" "$EMACS_DIR/init.el.bak"
fi

# Copy or symlink init.el
if [ -f "$SCRIPT_DIR/init.el" ]; then
  # Running from dotfiles clone — symlink
  ln -sf "$SCRIPT_DIR/init.el" "$EMACS_DIR/init.el"
  echo "    Symlinked init.el -> $SCRIPT_DIR/init.el"
else
  # Running standalone — download it
  REPO_URL="https://raw.githubusercontent.com/ezmiller/dotfiles/main/emacs-server/init.el"
  curl -sL "$REPO_URL" -o "$EMACS_DIR/init.el"
  echo "    Downloaded init.el to $EMACS_DIR/"
fi

# Create support directories
mkdir -p "$EMACS_DIR/backups"
mkdir -p "$EMACS_DIR/auto-saves"

echo "==> Installing language servers..."

# Node.js language servers (if npm available)
if command -v npm &>/dev/null; then
  echo "    Installing typescript-language-server..."
  npm install -g typescript-language-server typescript 2>/dev/null || true
  echo "    Installing bash-language-server..."
  npm install -g bash-language-server 2>/dev/null || true
else
  echo "    npm not found — skipping TS/bash language servers"
  echo "    Install Node.js, then run:"
  echo "      npm i -g typescript-language-server typescript bash-language-server"
fi

# Clojure LSP
if ! command -v clojure-lsp &>/dev/null; then
  echo "    clojure-lsp not found. Install from: https://clojure-lsp.io/installation/"
else
  echo "    clojure-lsp already installed"
fi

# Tree-sitter grammars need a C compiler
if command -v gcc &>/dev/null || command -v cc &>/dev/null; then
  echo "    C compiler found — tree-sitter grammars can be compiled"
  echo "    Run in Emacs: M-x my/treesit-install-all"
else
  echo "    No C compiler found — install gcc for tree-sitter support"
fi

echo ""
echo "Done! Start Emacs and run:"
echo "  M-x my/treesit-install-all    (one-time: compile tree-sitter grammars)"
echo "  C-z h                          (show keybinding quick reference)"
