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
(setq use-package-always-ensure t) ;; use `:ensure nil' to avoid

;; Functions
(defun toggle-buffers ()
  (interactive)
  (switch-to-buffer nil))

;; Minimal UI
(scroll-bar-mode -2)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Themes
(use-package doom-themes
  :config
  (load-theme 'doom-molokai t))

;; Which Key
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; Keybindings
(use-package general
 :init
  (general-override-mode 1)
  :config
  (general-create-definer dominant-def
    :states '(normal visual insert motion emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC"
    :prefix-map 'dominant-prefix-map)
  (dominant-def
    "TAB" 'toggle-buffers))

;; Path management
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package restart-emacs
  :config
  (general-def "C-c R" 'restart-emacs))

;; Vim mode
(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq-default evil-escape-delay 0.2))
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init 'magit))
(use-package evil-nerd-commenter)

;; Package manager
(use-package projectile
  :diminish projectile-mode
  :config
  (progn
    (general-def "C-c p" 'projectile-command-map)
    (projectile-mode +1)
    (setq projectile-completion-system 'auto)
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'alien)
    (add-to-list 'projectile-globally-ignored-files "node-modules")))

;; Selectrum, etc
(use-package selectrum
  :config
  (selectrum-mode +1))
(use-package prescient :config (prescient-persist-mode +1))
(use-package selectrum-prescient :init (selectrum-prescient-mode +1) :after selectrum)

(use-package consult
  :after projectile)

;; Git
(use-package magit
  :defer t
  :config
  (dominant-def "gs" 'magit-status))


;; Clojure
(use-package clojure-mode
  :defer t)
(use-package cider
  :defer t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(consult restart-emacs writeroom-mode use-package exec-path-from-shell evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
