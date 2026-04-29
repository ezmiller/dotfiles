# Misc Stuff that is sometimes needed
brew "libyaml"
brew "pkg-config"
brew "cairo"
brew "pango"
brew "libpng"
brew "jpeg"
brew "giflib"
brew "librsvg"
brew "pixman"
brew "curl"
brew "cmake"

# Prettier alternative to cat
brew "bat"

# Tmux, of course
brew "tmux"
brew "tmuxinator"
brew "gitmux"

# JQ, of course
brew "jq"

# socat - networking utility for socket connections
brew "socat"

# A useeful tool for learning about cli commands
# https://github.com/tldr-pages/tldr
brew "tldr"

# Different code search tools
brew "ripgrep"
brew "ag"
brew "fd"

# GitHub CLI (replaces hub)
# https://cli.github.com/
brew "gh"

# GnuPG for GPG key management
# Pinning to v2.2 because latest version 2.4.2 has bug
# that causes Emacs to hang on decrypt. See:
# - https://www.reddit.com/r/emacs/comments/137r7j7/gnupg_241_encryption_issues_with_emacs_orgmode/
# - https://dev.gnupg.org/T6481
# It sounds like this is being worked on by GnuPG team and may be fixed in newer versions...
#
# Note: Brew does not automatically symlink these so we need to do a brew link command after.
brew "gnupg@2.2"

# Databases
brew "postgresql@14"

# Programming languages & related
brew "clojure"
tap "clojure-lsp/brew"
brew "clojure-lsp-native"
brew "rust"
brew "rust-analyzer"
brew "python"
brew "python-setuptools"
brew "java"
brew "openjdk"
brew "npm"
brew "yarn"

# Fonts used in Emacs
cask 'font-fira-code'
# Coordinated mono + proportional pair for org-modern harmony.
# Iosevka — monospace coding font, used for `default` and `fixed-pitch`.
# Iosevka Aile — proportional sans companion with matching metrics,
# used for `variable-pitch`.
# Note: Homebrew does not ship a standalone "Iosevka Term" cask anymore
# (only the Nerd Font variant). Default Iosevka has ligatures, but they
# only render when ligature.el is active — currently commented out.
cask 'font-iosevka'
cask 'font-iosevka-aile'

# Ghostty terminal
cask "ghostty"

# AWS Cli
brew "awscli"

# Terraform for infrastructure as code
brew "terraform"

# Emacs
brew "gcc"
brew "tree-sitter" 
tap 'jimeh/emacs-builds'
cask 'emacs-app'
#Used this for emacs29 with native comp originally
#brew "emacs-plus@29", args: ["with-native-comp"]

# Docker - often needed
# cask "docker"

# OrbStack - an alternative to Docker Desktop
cask "orbstack"
