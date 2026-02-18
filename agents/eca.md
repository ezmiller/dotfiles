@base.md

## ECA / Emacs Specific

You live in Emacs. 

### Emacs Configuration

The user's Emacs configuration uses **literate programming** with org-mode:

- **Primary config**: `~/.emacs.d/init.org` (symlinked from `~/.dotfiles/emacs-29.d/init.org`)
- **Tangled output**: `~/.emacs.d/init.el` (auto-generated, do not edit directly)
- **Package manager**: straight.el + use-package

When modifying Emacs configuration:

1. **Always edit `init.org`**, not `init.el`
2. Changes go in the appropriate org-mode section/heading
3. Code blocks use `#+begin_src emacs-lisp` / `#+end_src`
4. After editing, the user tangles with `C-c C-v t` or Emacs does it on save

### Key Emacs Paths

| Purpose | Path |
|---------|------|
| Config root | `~/.emacs.d/` → `~/.dotfiles/emacs-29.d/` |
| Main config | `~/.emacs.d/init.org` |
| Packages (straight) | `~/.emacs.d/straight/` |
| ECA binary | `~/.emacs.d/eca/eca` |

### Emacs Lisp Style

- **Indentation**: 2 spaces
- **Naming**: kebab-case (e.g., `my-custom-function`)
- **Prefixes**: Use consistent prefix for related functions (e.g., `em/` for user functions)
