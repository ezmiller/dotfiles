# External plugins (initialized after)

# Syntax highlighting

source ~/.zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)

# IBM Design colorblind-safe palette.
# blue #648FFF, purple #785EF0, magenta #DC267F, orange #FE6100, yellow #FFB000
if [[ "$(tput colors)" == "256" ]]; then
    ZSH_HIGHLIGHT_STYLES[default]=none
    ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=#DC267F,bold'
    ZSH_HIGHLIGHT_STYLES[reserved-word]='fg=#785EF0,bold'
    ZSH_HIGHLIGHT_STYLES[alias]='fg=#648FFF,bold'
    ZSH_HIGHLIGHT_STYLES[builtin]='fg=#648FFF,bold'
    ZSH_HIGHLIGHT_STYLES[function]='fg=#648FFF,bold'
    ZSH_HIGHLIGHT_STYLES[command]='fg=#648FFF,bold'
    ZSH_HIGHLIGHT_STYLES[precommand]='fg=#648FFF,underline'
    ZSH_HIGHLIGHT_STYLES[commandseparator]=none
    ZSH_HIGHLIGHT_STYLES[hashed-command]='fg=#785EF0'
    ZSH_HIGHLIGHT_STYLES[path]='fg=#FE6100,underline'
    ZSH_HIGHLIGHT_STYLES[globbing]='fg=#FFB000'
    ZSH_HIGHLIGHT_STYLES[history-expansion]=fg=white,underline
    ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=#785EF0,bold'
    ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=#785EF0,bold'
    ZSH_HIGHLIGHT_STYLES[back-quoted-argument]=none
    ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg=#FFB000'
    ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg=#FFB000'
    ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]='fg=#FFB000'
    ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]='fg=#FFB000'
    ZSH_HIGHLIGHT_STYLES[assign]='fg=#648FFF'
fi

export LSCOLORS="Gxfxcxdxbxegedabagacad"

# For zsh-history-substring-search plugin
source ~/.zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh

# Bindings for zsh-history-substring-search
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

