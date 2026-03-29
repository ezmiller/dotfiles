;;; init.el --- Minimal server Emacs config (Emacs 30, no packages) -*- lexical-binding: t; -*-

;; Designed for: terminal-only use on remote servers (EC2 AL2023)
;; Languages: Clojure, TypeScript, Bash
;; Zero external dependencies — built-ins only

;;; ============================================================
;;; QUICK REFERENCE (C-z h to show this anytime)
;;; ============================================================
;;
;; LEADER KEY: C-z  (press C-z then the next key)
;;   C-z f    find file in project     C-z g    grep in project
;;   C-z b    switch buffer            C-z p    switch project
;;   C-z d    dired                    C-z r    recent files
;;   C-z s    shell                    C-z v    vc-dir (git)
;;   C-z h    show this help
;;
;; FILES & BUFFERS
;;   C-x C-f  open file               C-x C-s  save file
;;   C-x b    switch buffer            C-x k    kill buffer
;;   C-x C-b  list buffers
;;
;; MOVEMENT
;;   C-f/b    char forward/back        M-f/b    word forward/back
;;   C-n/p    next/prev line           C-a/e    begin/end of line
;;   M-</M->  begin/end of buffer      C-l      recenter
;;   M-g g    go to line number
;;
;; SEARCH & REPLACE
;;   C-s      search forward           C-r      search backward
;;   M-%      query replace            C-M-%    regex replace
;;
;; SELECTION & EDITING
;;   C-SPC    set mark (start select)  C-w      cut region
;;   M-w      copy region              C-y      paste (yank)
;;   M-y      cycle paste history      C-/      undo
;;   C-x u    undo                     M-u/l    upper/lowercase word
;;   C-M-\    indent region            TAB      indent line
;;
;; WINDOWS
;;   C-x 2    split horizontal         C-x 3    split vertical
;;   C-x 1    close other windows      C-x 0    close this window
;;   C-x o    other window             M-o      other window (alias)
;;
;; CODE NAVIGATION (with eglot/LSP)
;;   M-.      go to definition         M-,      go back
;;   M-?      find references          C-c C-r  rename symbol
;;   C-c C-a  code action              C-c C-f  format buffer
;;
;; FLYMAKE (errors)
;;   C-c ! n  next error               C-c ! p  prev error
;;   C-c ! l  list errors
;;
;; MISC
;;   M-x      run any command          C-g      cancel anything
;;   C-h m    show current mode help   C-h b    all keybindings
;;   C-h k    describe key             C-h f    describe function
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

;; Better performance
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold (* 50 1000 1000))

;; Disable GUI chrome (no-op in terminal but safe to call)
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))
(menu-bar-mode -1)

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

;;; ============================================================
;;; Leader Key (C-z)
;;; ============================================================

;; C-z is normally `suspend-frame` — useless in terminal with tmux
(define-prefix-command 'my-leader-map)
(global-set-key (kbd "C-z") 'my-leader-map)

;; Project
(define-key my-leader-map (kbd "f") 'project-find-file)
(define-key my-leader-map (kbd "g") 'project-find-regexp)
(define-key my-leader-map (kbd "p") 'project-switch-project)

;; Buffers & files
(define-key my-leader-map (kbd "b") 'switch-to-buffer)
(define-key my-leader-map (kbd "r") 'recentf-open-files)
(define-key my-leader-map (kbd "d") 'dired-jump)
(define-key my-leader-map (kbd "k") 'kill-buffer)

;; Tools
(define-key my-leader-map (kbd "s") 'eshell)
(define-key my-leader-map (kbd "v") 'vc-dir)

;; Window management
(define-key my-leader-map (kbd "1") 'delete-other-windows)
(define-key my-leader-map (kbd "2") 'split-window-below)
(define-key my-leader-map (kbd "3") 'split-window-right)
(define-key my-leader-map (kbd "0") 'delete-window)

;; Help
(define-key my-leader-map (kbd "h") 'my/show-help)

;; Convenience
(global-set-key (kbd "M-o") 'other-window)

;; which-key (built-in since Emacs 30)
(which-key-mode 1)
(setq which-key-idle-delay 0.3)

;;; ============================================================
;;; Help Buffer
;;; ============================================================

(defun my/show-help ()
  "Show quick-reference keybinding help."
  (interactive)
  (let ((buf (get-buffer-create "*Quick Reference*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert
       "LEADER (C-z)                         FILES & BUFFERS\n"
       "  f  find file in project              C-x C-f  open file\n"
       "  g  grep in project                   C-x C-s  save\n"
       "  b  switch buffer                     C-x b    switch buffer\n"
       "  p  switch project                    C-x k    kill buffer\n"
       "  r  recent files\n"
       "  d  dired                           MOVEMENT\n"
       "  s  eshell                            C-n/p    next/prev line\n"
       "  v  vc-dir (git)                      C-f/b    char fwd/back\n"
       "  k  kill buffer                       M-f/b    word fwd/back\n"
       "  1  close other windows               C-a/e    begin/end line\n"
       "  2  split horizontal                  M-g g    go to line\n"
       "  3  split vertical                    M-</>    begin/end buffer\n"
       "  0  close this window\n"
       "  h  this help                       SEARCH & REPLACE\n"
       "                                       C-s/r    search fwd/back\n"
       "SELECTION & EDITING                    M-%      query replace\n"
       "  C-SPC   start selection\n"
       "  C-w     cut                        CODE (eglot)\n"
       "  M-w     copy                         M-.      go to definition\n"
       "  C-y     paste                        M-,      go back\n"
       "  C-/     undo                         M-?      find references\n"
       "  C-M-\\   indent region                C-c C-r  rename symbol\n"
       "                                       C-c C-a  code action\n"
       "WINDOWS\n"
       "  M-o     other window               FLYMAKE\n"
       "  C-x 1   only this window             C-c ! n  next error\n"
       "  C-x 2   split horiz                  C-c ! p  prev error\n"
       "  C-x 3   split vert                   C-c ! l  list errors\n"
       "\n"
       "  C-g  cancel anything    M-x  run command    C-h m  mode help\n")
      (goto-char (point-min))
      (read-only-mode 1))
    (display-buffer buf '(display-buffer-in-side-window
                          (side . bottom)
                          (window-height . 0.4)))))

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
  (setq eglot-autoshutdown t))

;; Auto-start eglot for supported languages
;; Requires language servers to be installed:
;;   npm i -g typescript-language-server typescript
;;   npm i -g bash-language-server
;;   clojure-lsp (see https://clojure-lsp.io/installation/)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'clojure-mode-hook 'eglot-ensure)
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

;; Clojure — basic support via built-in lisp modes
;; For REPL: run `clj` in eshell or use M-x run-lisp
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojure-mode))
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

;;; ============================================================
;;; Server (so emacsclient works)
;;; ============================================================

(unless (daemonp)
  (require 'server)
  (unless (server-running-p)
    (server-start)))

(provide 'init)
;;; init.el ends here
