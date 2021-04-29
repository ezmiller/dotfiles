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
(defun toggle-buffers ()
  (interactive)
  (switch-to-buffer nil))

;; Minimal UI
(scroll-bar-mode -2)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Font
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
    "TAB" 'toggle-buffers
    "pp"  'projectile-switch-project
    "pf"  'consult-find))

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
  :ensure t
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

;; Selectrum, etc
(use-package selectrum
  :config
  (selectrum-mode +1))
(use-package prescient :config (prescient-persist-mode +1))
(use-package selectrum-prescient :init (selectrum-prescient-mode +1) :after selectrum)
(use-package consult :after projectile)

;; Git
(use-package magit
  :config
  (spc-key-definer "gs" 'magit-status))


;; Clojure
(use-package clojure-mode :defer t)
(use-package cider :defer t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" default))
 '(package-selected-packages
   '(consult restart-emacs writeroom-mode use-package exec-path-from-shell evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#1c1e1f" :foreground "#d6d6d4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "nil" :family "Source Code Proc")))))
