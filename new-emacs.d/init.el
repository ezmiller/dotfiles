;; Some resources
;; - https://jamiecollinson.com/blog/my-emacs-config/

(setq delete-old-versions -1)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)
(setq default-fill-column 80)
(setq initial-scratch-message "")

(setq-default mode-line-format nil)

(global-auto-revert-mode t)
(electric-pair-mode 1)

(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))

;; Functions
(defun my/toggle-buffers ()
  (interactive)
  (switch-to-buffer nil))

(defun my/cider-test-run-focused-test ()
  "Run test around point."
  (interactive)
  (cider-load-buffer)
  (cider-test-run-test))

;; Minimal UI
(scroll-bar-mode -2)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Font - font needs to be installed in the Mac Font Book
(add-to-list 'default-frame-alist '(font . "Fira Code-16"))
(set-face-attribute 'default t :font "Fira Code-16")


;; Package management
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
			 ("gnu"       . "http://elpa.gnu.org/packages/")
			 ("melpa"     . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; ensure everything is installed, use `:ensure nil` to override
(setq use-package-always-ensure t)

;; Initialize general for key bindings
(use-package general
  :config
  (general-create-definer spc-key-definer
    :states '(normal visual insert motion emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC"
    :prefix-map 'dominant-prefix-map))

;; Global keybindings
(with-eval-after-load 'evil
  (spc-key-definer
    "TAB" 'my/toggle-buffers
    "pp"  'projectile-switch-project
    "pf"  'consult-find
    "bb"  'consult-buffer
    "rr"  'consult-recent-file))

;; Allow C-u/d for page up/down
(setq evil-want-C-u-scroll t)
(setq evil-want-C-d-scroll t)

;; Set this to match clojure indent style
;; May need to be set per mode at some point?
(setq evil-shift-width 2)

(use-package evil
  :init
  ;; These needs to be set when using evil-collection
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq-default evil-escape-delay 0.2))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list nil) ;; disable all evil bindings as default
  (evil-collection-init 'magit))

(use-package evil-nerd-commenter
  :config
  (general-define-key
   "M-;" 'evilnc-comment-or-uncomment-lines))

(use-package doom-themes
  :config
  (load-theme 'doom-molokai t)
  ;; (load-theme 'doom-snazzy t)
  )

;; Which Key
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; Path management
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package restart-emacs
  :config
  (general-def "C-c R" 'restart-emacs))

(use-package projectile
  :diminish projectile-mode
  :config
  (progn
    (general-def "C-c p" 'projectile-command-map)
    (projectile-mode +1)
    (setq projectile-completion-system 'auto)
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'alien)
    (add-to-list 'projectile-globally-ignored-files "node-modules")
    (autoload 'projectile-project-root "projectile")
    (setq consult-project-root-function #'projectile-project-root)))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(use-package all-the-icons)
(use-package doom-modeline
  :requires all-the-icons
  :init
  (doom-modeline-mode 1)
  :config
  (progn
    (setq doom-modeline-height 15)))

;; Selectrum, etc
(use-package selectrum
  :config
  (selectrum-mode +1))
(use-package prescient :config (prescient-persist-mode +1))
(use-package selectrum-prescient :init (selectrum-prescient-mode +1) :after selectrum)
(use-package consult :after projectile)

;; Richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle' to
  ;; switch between the annotators.
  ;; (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
)

;; Git
(use-package magit
  :config
  (spc-key-definer "gs" 'magit-status))

;; Structural editing - For keybinding reference: https://github.com/syl20bnr/evil-lisp-state
(use-package smartparens
  :config
  ;; Taken from: https://github.com/syl20bnr/evil-lisp-state/blob/master/evil-lisp-state.el#L313-L335
  (defun my-lisp/insert-sexp-after ()
    "Insert sexp after the current one." (interactive)
    (let ((sp-navigate-consider-symbols nil))
      (if (char-equal (char-after) ?\() (forward-char))
      (sp-up-sexp)
      (evil-insert-state)
      (sp-newline)
      (sp-insert-pair "(")))

  (defun my-lisp/insert-sexp-before ()
    "Insert sexp before the current one."
    (interactive)
    (let ((sp-navigate-consider-symbols nil))
      (if (char-equal (char-after) ?\() (forward-char))
      (sp-backward-sexp)
      (evil-insert-state)
      (sp-newline)
      (evil-previous-visual-line)
      (evil-end-of-line)
      (insert " ")
      (sp-insert-pair "(")
      (indent-for-tab-command)))
  ;; structural editing keybindings
  (general-define-key
   :states 'normal
   :prefix "SPC k"
   "y"  'sp-copy-sexp
   "dx" 'sp-kill-sexp
   "s" 'sp-forward-slurp-sexp
   "b" 'sp-forward-barf-sexp
   ")" 'my-lisp/insert-sexp-after
   "(" 'my-lisp/insert-sexp-before))


;; Clojure
(show-paren-mode 1)
(use-package clojure-mode :defer t)
(use-package cider :defer t)
(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(use-package company
  :config
  (progn
    (add-hook 'cider-repl-mode-hook #'company-mode)
    (add-hook 'cider-mode-hook #'company-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dashboard writeroom-mode which-key use-package selectrum-prescient restart-emacs projectile magit general exec-path-from-shell evil-nerd-commenter evil-lisp-state evil-collection doom-themes consult cider)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
