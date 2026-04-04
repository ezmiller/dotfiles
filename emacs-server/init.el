;;; init.el --- Minimal server Emacs config (Emacs 30) -*- lexical-binding: t; -*-

;; Designed for: terminal-only use on remote servers (EC2 AL2023)
;; Languages: Clojure, TypeScript, Bash
;; External packages: evil, evil-collection, evil-nerd-commenter

;;; ============================================================
;;; QUICK REFERENCE (press SPC and wait for which-key popup)
;;; ============================================================
;;
;; Evil (vim) mode is active. Normal vim motions work.
;;
;; LEADER KEY: SPC  (normal mode)
;;   SPC f    find file in project     SPC g    grep in project
;;   SPC b    switch buffer            SPC p    switch project
;;   SPC d    dired                    SPC r    recent files
;;   SPC s    shell                    SPC v    vc-dir (git)
;;   SPC k    kill buffer
;;
;; VIM BASICS
;;   h/j/k/l  move                     w/b      word forward/back
;;   gg/G     top/bottom of file       C-u/C-d  page up/down
;;   /        search forward           ?        search backward
;;   n/N      next/prev match          *        search word under cursor
;;   dd       delete line              yy       copy line
;;   p        paste                    u        undo
;;   C-r      redo                     .        repeat last change
;;   v        visual select            V        visual line
;;   C-v      visual block             >/<      indent/dedent (visual)
;;   M-;      comment/uncomment
;;
;; CODE NAVIGATION (eglot/LSP)
;;   gd       go to definition         M-,      go back
;;   M-?      find references          C-c C-r  rename symbol
;;   C-c C-a  code action              C-c C-f  format buffer
;;
;; FLYMAKE (errors)
;;   C-c ! n  next error               C-c ! p  prev error
;;   C-c ! l  list errors
;;
;; MISC
;;   :        ex command               M-x      run any emacs command
;;   C-g      cancel anything          C-h k    describe key
;;

;;; ============================================================
;;; Core Settings
;;; ============================================================

(setq user-full-name "Ethan Miller")
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq use-short-answers t)
(setq ring-bell-function 'ignore)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)
(setq default-fill-column 80)
(setq create-lockfiles nil)
(setq xterm-extra-capabilities nil)

;; Reduce ESC delay so C-[ works reliably as ESC in terminal/mosh.
;; Terminal ESC is ambiguous (same byte starts arrow/function key sequences),
;; so Emacs waits to see if more keys follow. Over SSH this compounds with
;; network latency. Both layers need short timeouts.
(setq evil-esc-delay 0)
(setq-default evil-escape-delay 0.1)

;; This is the Emacs-level timeout for multi-byte terminal escape sequences.
;; Default is 0.1s which feels sluggish over SSH. Setting lower means C-[
;; registers faster, but too low can break arrow keys. 0.01 is usually safe.
(setq read-key-sequence-timeout 0.01)

;; Better performance
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold (* 50 1000 1000))

;; Disable GUI chrome (no-op in terminal but safe to call)
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))
(menu-bar-mode -1)

;;; ============================================================
;;; Package Management
;;; ============================================================

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (pkg '(evil evil-collection evil-nerd-commenter which-key cider corfu corfu-terminal cape))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; clay.el is not on MELPA — install from GitHub
(unless (package-installed-p 'clay)
  (package-vc-install "https://github.com/scicloj/clay.el"))

;;; ============================================================
;;; Evil Mode (vim keybindings)
;;; ============================================================

;; Must be set before loading evil
(setq evil-want-C-u-scroll t)
(setq evil-want-C-d-scroll t)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-shift-width 2)

(require 'evil)
(evil-mode 1)

;; Restore M-. and M-, for xref navigation (evil overrides M-.)
(define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
(define-key evil-normal-state-map (kbd "M-,") 'xref-go-back)

;; Visual mode indent keeps selection (like your local config)
(defun my/evil-shift-right ()
  (interactive)
  (evil-shift-right evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(defun my/evil-shift-left ()
  (interactive)
  (evil-shift-left evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(define-key evil-visual-state-map (kbd ">") 'my/evil-shift-right)
(define-key evil-visual-state-map (kbd "<") 'my/evil-shift-left)

;; evil-collection for dired, vc, flymake, etc.
(require 'evil-collection)
(setq evil-collection-mode-list '(dired flymake eglot cider))
(evil-collection-init)

;; Comment/uncomment with M-;
(require 'evil-nerd-commenter)
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

;; which-key: show available keys after pressing a prefix
(require 'which-key)
(setq which-key-idle-delay 0.3)
(which-key-mode 1)

;; Leader key: SPC in normal mode
(define-prefix-command 'my-leader-map)
(define-key evil-normal-state-map (kbd "SPC") 'my-leader-map)

;; Leader bindings
(define-key my-leader-map (kbd "f") 'project-find-file)
(define-key my-leader-map (kbd "g") 'project-find-regexp)
(define-key my-leader-map (kbd "p") 'project-switch-project)
(define-key my-leader-map (kbd "b") 'switch-to-buffer)
(define-key my-leader-map (kbd "r") 'recentf-open-files)
(define-key my-leader-map (kbd "d") 'dired-jump)
(define-key my-leader-map (kbd "k") 'kill-buffer)
(define-key my-leader-map (kbd "s") 'eshell)
(define-key my-leader-map (kbd "v") 'vc-dir)
(define-key my-leader-map (kbd "1") 'delete-other-windows)
(define-key my-leader-map (kbd "2") 'split-window-below)
(define-key my-leader-map (kbd "3") 'split-window-right)
(define-key my-leader-map (kbd "0") 'delete-window)

;;; ============================================================
;;; Terminal Setup
;;; ============================================================

;; Mouse support
(xterm-mouse-mode 1)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

;; OSC 52 clipboard (works over SSH/tmux)
;; Emacs 29 has built-in xterm-paste but not OSC 52 copy.
;; We define a simple OSC 52 copy function.
(defun my/osc52-copy (text)
  "Copy TEXT to system clipboard via OSC 52 escape sequence."
  (let ((encoded (base64-encode-string (encode-coding-string text 'utf-8) t)))
    (send-string-to-terminal (concat "\e]52;c;" encoded "\a"))))

(setq interprogram-cut-function #'my/osc52-copy)

;;; ============================================================
;;; Editing Defaults
;;; ============================================================

(setq show-paren-delay 0)
(show-paren-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(savehist-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(setq recentf-max-saved-items 50)
(winner-mode 1)
(column-number-mode 1)

;; Line numbers in code
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Clickable links in code/text
(add-hook 'prog-mode-hook #'goto-address-prog-mode)
(add-hook 'text-mode-hook #'goto-address-mode)

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)

;;; ============================================================
;;; Backups & Auto-saves (centralized)
;;; ============================================================

(let ((backup-dir (expand-file-name "backups" user-emacs-directory))
      (autosave-dir (expand-file-name "auto-saves" user-emacs-directory)))
  (make-directory backup-dir t)
  (make-directory autosave-dir t)
  (setq backup-directory-alist `(("." . ,backup-dir)))
  (setq auto-save-file-name-transforms `((".*" ,autosave-dir t))))

(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)

;;; ============================================================
;;; Completion (fido-vertical-mode — built-in)
;;; ============================================================

(fido-vertical-mode 1)
(setq completion-styles '(flex basic partial-completion))
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;; ============================================================
;;; Theme
;;; ============================================================

(load-theme 'modus-vivendi t)

;; Use terminal's own background instead of theme's — avoids mismatched bg color
(unless (display-graphic-p)
  (set-face-background 'default "unspecified-bg"))

;; Convenience
(global-set-key (kbd "M-o") 'other-window)

;; which-key (built-in since Emacs 30)
(which-key-mode 1)
(setq which-key-idle-delay 0.3)

;;; ============================================================
;;; which-key descriptions for leader keys
;;; ============================================================

(which-key-add-keymap-based-replacements my-leader-map
  "f" "find file"
  "g" "grep project"
  "p" "switch project"
  "b" "switch buffer"
  "r" "recent files"
  "d" "dired"
  "k" "kill buffer"
  "s" "eshell"
  "v" "vc-dir (git)"
  "1" "only window"
  "2" "split horiz"
  "3" "split vert"
  "0" "close window")

;;; ============================================================
;;; Flymake
;;; ============================================================

(add-hook 'prog-mode-hook #'flymake-mode)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c ! l") 'flymake-show-buffer-diagnostics))

;;; ============================================================
;;; Eglot (built-in LSP)
;;; ============================================================

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c C-r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c C-a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c C-f") 'eglot-format-buffer)
  (setq eglot-autoshutdown t)
  (setq eglot-connect-timeout 120))

;; Auto-start eglot for supported languages
;; Clojure excluded — clojure-lsp indexing is slow on first run
;; Use M-x eglot manually for Clojure when needed
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'bash-ts-mode-hook 'eglot-ensure)
(add-hook 'sh-mode-hook 'eglot-ensure)

;;; ============================================================
;;; Tree-sitter (Emacs 29)
;;; ============================================================

(when (treesit-available-p)
  (setq treesit-font-lock-level 4)

  ;; Grammar sources — run (my/treesit-install-all) once to compile them
  (setq treesit-language-source-alist
        '((typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                      "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
               "master" "tsx/src")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                      "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json"
                "master" "src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml"
                "master" "src")
          (bash "https://github.com/tree-sitter/tree-sitter-bash"
                "master" "src")
          (clojure "https://github.com/sogaiu/tree-sitter-clojure"
                   "master" "src")))

  ;; Use tree-sitter modes when grammars are available
  (setq major-mode-remap-alist
        '((js-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (css-mode . css-ts-mode)
          (sh-mode . bash-ts-mode))))

(defun my/treesit-install-all ()
  "Install all tree-sitter grammars. Run this once on a new machine."
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (message "Installing %s grammar..." (car grammar))
    (treesit-install-language-grammar (car grammar))))

;;; ============================================================
;;; Language-specific
;;; ============================================================

;; Clojure — cider provides clojure-mode + REPL
;; Use cider-jack-in (C-c M-j) to start a REPL
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojurec-mode))
(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))

;; TypeScript/TSX — handled by tree-sitter remap above
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; YAML
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

;;; ============================================================
;;; Dired
;;; ============================================================

(setq dired-listing-switches "-alh --group-directories-first")
(setq dired-dwim-target t)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;;; ============================================================
;;; Eshell niceties
;;; ============================================================

(setq eshell-scroll-to-bottom-on-input 'this)
(setq eshell-scroll-to-bottom-on-output 'this)

;;; ============================================================
;;; VC (built-in git interface)
;;; ============================================================

(setq vc-follow-symlinks t)
(setq vc-git-show-stash 5)

;;; ============================================================
;;; Completion at point (basic, no company)
;;; ============================================================

(setq tab-always-indent 'complete)
(setq completion-auto-help 'always)

;; Corfu — completion popup (with terminal support)
(require 'corfu)
(setq corfu-auto t)
(setq corfu-auto-delay 0.2)
(setq corfu-auto-prefix 2)
(global-corfu-mode 1)

;; Terminal support for corfu (child frames don't work in terminal)
(unless (display-graphic-p)
  (require 'corfu-terminal)
  (corfu-terminal-mode 1))

;; Cape — extra completion backends (buffer words, file paths, etc.)
(require 'cape)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)

;;; ============================================================
;;; CIDER (Clojure REPL)
;;; ============================================================

(with-eval-after-load 'cider
  (setq cider-repl-display-help-banner nil)
  (setq cider-show-error-buffer 'only-in-repl)
  (setq cider-font-lock-dynamically nil)
  ;; Use evil in CIDER REPL
  (evil-set-initial-state 'cider-repl-mode 'normal)

  ;; Make inline eval results readable in terminal (no background, just bold)
  (set-face-attribute 'cider-result-overlay-face nil
                      :background 'unspecified
                      :foreground "yellow"
                      :weight 'bold))

;;; ============================================================
;;; Clay (notebook rendering via CIDER)
;;; ============================================================
;; Requires CIDER connected. View output at http://100.97.168.36:1971
;; Uses clay.el from https://github.com/scicloj/clay.el

(require 'clay nil t)

;; Leader bindings for Clay
(define-key my-leader-map (kbd "c f") 'clay-make-file)
(define-key my-leader-map (kbd "c d") 'clay-make-defun-at-point)
(define-key my-leader-map (kbd "c n") 'clay-make-ns)

;;; ============================================================
;;; Server (so emacsclient works)
;;; ============================================================

(unless (daemonp)
  (require 'server)
  (unless (server-running-p)
    (server-start)))

(provide 'init)
;;; init.el ends here
