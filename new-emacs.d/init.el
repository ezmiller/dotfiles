;;; Initial notes

  ;; Some resources
  ;; - https://jamiecollinson.com/blog/my-emacs-config/

  ;; Note on navigation
  ;;   - Use `C-c @ C-t` to hide all
  ;;   - Use `C-c @ C-a` to show all
  ;;   - Other options for org-outline-minor-mode: `C-c @`

;;; Initial setup

  ;; Setup outline-minor-mode for emacs-lisp, useful for this file
  ;; See: https://emacs.stackexchange.com/questions/3143/can-i-use-org-mode-to-structure-my-emacs-or-other-el-configuration-file
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (make-local-variable 'outline-regexp)
	      (setq outline-regexp "^;;; ")
	      (make-local-variable 'outline-heading-end-regexp)
	      ;; (setq outline-heading-end-regexp ":\n")
	      (outline-minor-mode 1)))

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


  ;; Enables ligatures in the emacs macport
  (if (fboundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode))

  ;;; Minimal UI
  (scroll-bar-mode -2)
  (tool-bar-mode   -1)
  (tooltip-mode    -1)
  (menu-bar-mode   -1)


;;; Font
  ;;- font needs to be installed in the Mac Font Book
  (add-to-list 'default-frame-alist '(font . "Fira Code-16"))
  (set-face-attribute 'default t :font "Fira Code-16")

;;; Theme using `doom-themes`
  (use-package doom-themes
    :config
    (load-theme 'doom-molokai t)
    ;; (load-theme 'doom-snazzy t)
    )

;;; Global Functions
  (defun my/toggle-buffers ()
    (interactive)
    (switch-to-buffer nil))

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

  (defun my/cider-test-run-focused-test ()
    "Run test around point."
    (interactive)
    (cider-load-buffer)
    (cider-test-run-test))

;;; Package management
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

;;; Initialize `general` for keybindings
  (use-package general
    :config
    (general-create-definer spc-key-definer
      :states '(normal visual insert motion emacs)
      :prefix "SPC"
      :non-normal-prefix "C-SPC"
      :prefix-map 'dominant-prefix-map))

;;; Global keybindings
  (with-eval-after-load 'evil
    (spc-key-definer
      "TAB" 'my/toggle-buffers
      "pp"  'projectile-switch-project
      "pf"  'consult-find
      "bb"  'consult-buffer
      "rr"  'consult-recent-file
      "u"   'universal-argument))

;;; Initialize Evil
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
    (setq-default evil-escape-delay 0.2)
    (general-define-key
    :states 'visual
    ">" 'my/evil-shift-right
    "<" 'my/evil-shift-left)
    )

  (use-package evil-collection
    :after evil
    :config
    (setq evil-collection-mode-list nil) ;; disable all evil bindings as default
    (evil-collection-init '(magit dired)))

  (use-package evil-nerd-commenter
    :config
    (general-define-key
    "M-;" 'evilnc-comment-or-uncomment-lines))

;;; Initialize `which-key` for dynamic key binding menus
  (use-package which-key
    :init
    (setq which-key-separator " ")
    (setq which-key-prefix-prefix "+")
    :config
    (which-key-mode))

;;; Path management
  (use-package exec-path-from-shell
    :config
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)))

;;; Restart-emacs package
  (use-package restart-emacs
    :config
    (general-def "C-c R" 'restart-emacs))

;;; Projectile project management
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

;;; Dashboard on boot
  (use-package dashboard
    :config
    (dashboard-setup-startup-hook))

;;; Window management with `ace-window`
  (use-package ace-window
    :init
    (ace-window-display-mode 1)
    :config
    (general-define-key
    "M-o" 'ace-window))

;;; Doom-modeline for status bar
  (use-package all-the-icons)
  (use-package doom-modeline
    :requires all-the-icons
    :init
    (doom-modeline-mode 1)
    :config
    (progn
      (setq doom-modeline-height 15)
      (setq column-number-mode t
	    line-number-mode t)))

;;; Selectrum, etc.
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

;;; Version management
  (use-package magit
    :config
    (spc-key-definer "gs" 'magit-status))

;;; Structural editing with smartparens
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

;;; Javascript
  (setq js-indent-level 2)
  
  ;; (use-package js2-mode
  ;;   :defer t
  ;;   :mode (("\\.m?js\\'"  . js2-mode)))
  (use-package add-node-modules-path
    :defer t
    :hook (((js2-mode rjsx-mode) . add-node-modules-path)))

  ;; rjsx-mode extends js2-mode, so it provides js2-mode plus functionality for jsx
  (use-package rjsx-mode
    :defer t
    :mode ("\\.jsx?\\'" "\\.tsx?\\'")
    :config
    (setq js2-mode-show-parse-errors nil
	  js2-mode-show-strict-warnings nil))
;;; Clojure Configuration
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

;;; Org-mode configuation
  (setq org-directory "~/org")

  ;; Sets the column width to 80 columns and enables line breaking, ie. auto-fill.
  (add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
  (add-hook 'org-mode-hook 'auto-fill-mode)

  ;; Indent contents along with tree/bullet depth
  (add-hook 'org-mode-hook 'org-indent-mode)

  (use-package org-superstar
    :init
    (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;;; YAML support
  (use-package yaml-mode
      :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
	    ("Procfile\\'" . yaml-mode))
      :config (add-hook 'yaml-mode-hook
			#'(lambda ()
			  (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;;; Ensure that emacs window is focused when switching desktops
  ;; See: https://emacs.stackexchange.com/questions/28121/osx-switching-to-virtual-desktop-doesnt-focus-emacs
  (menu-bar-mode t) 

;;; Custom-set-variables - do not edit (autogenerated)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(add-node-modules-path web-mode typescript-mode org-superstar org-bullets ace-window dashboard writeroom-mode which-key use-package selectrum-prescient restart-emacs projectile magit general exec-path-from-shell evil-nerd-commenter evil-lisp-state evil-collection doom-themes consult cider)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
