;; Bootstrap  the 'straight' package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Now use-package that can work with straight
(straight-use-package 'use-package)

(eval-after-load 'use-package
  (setq
   ;; use-package should use straight unless otherwise specified
   straight-use-package-by-default t
   ;; ensure everything is installed, use `:ensure nil` to override
   use-package-always-ensure t))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; Secrets Setup - skip if no ~/secrets.el.gpg file instead warn
(if (file-exists-p "~/secrets.el.gpg")
    (progn
      (setq epg-pinentry-mode 'loopback) ;; this line I think allows prompt for passphrase in minibuffer
      (require 'epa-file)
      (setq auth-source-debug t)
      (load-library "~/secrets.el.gpg"))
  (warn "No ~/secrets.el.gpg file found, skipping secrets setup"))

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29

(if (boundp 'secret/github-pkg-auth-token)
  (setenv "GITHUB_PKG_AUTH_TOKEN" secret/github-pkg-auth-token))

;; Minimal UI
(scroll-bar-mode -1) ;; Disable visible scrollbar
(tool-bar-mode -1) ;; Disable the toolbar 
(tooltip-mode -1) ;; Disable tool tips

;; Ensure that emacs window is focused when switching desktops
;; See: https://emacs.stackexchange.com/questions/28121/osx-switching-to-virtual-desktop-doesnt-focus-emacs
(menu-bar-mode -1) ;; Disable menus

;;;; Make links clickable in comments and code
(add-hook 'prog-mode-hook #'goto-address-prog-mode)
(add-hook 'text-mode-hook #'goto-address-mode) 

(setq user-full-name "Ethan Miller")
(setq xterm-extra-capabilities nil)
(setq ring-bell-function 'ignore)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)
(setq default-fill-column 80)
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq use-short-answers t)

(setq show-paren-delay 0.5)
(show-paren-mode 1)

;; Enables minor mode that adds matching delimiters
(electric-pair-mode 1)

(use-package autorevert
  :config
  (setq auto-revert-interval 2)
  (setq auto-revert-check-vc-info t)
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode +1))

;; Centralize backup files
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))

;; Backup settings - keep them useful
(setq backup-by-copying t)       ; Don't clobber symlinks
(setq version-control t)         ; Use version numbers on backups
(setq delete-old-versions t)     ; Delete old versions silently
(setq kept-new-versions 6)       ; Keep 6 newest versions
(setq kept-old-versions 2)       ; Keep 2 oldest versions

;; Centralize auto-save files
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))

;; Disable lock files (these are the .# files)
;; Since you have git, you have good protection against simultaneous edits
(setq create-lockfiles nil)

;; Ensure directories exist
(make-directory (expand-file-name "backups" user-emacs-directory) t)
(make-directory (expand-file-name "auto-saves" user-emacs-directory) t)

(defun my/clean-old-backups ()
"Delete backup files older than a week."
(interactive)
(let ((week-ago (time-subtract (current-time) (days-to-time 7))))
  (dolist (file (directory-files-recursively 
                 (expand-file-name "backups" user-emacs-directory) ".*"))
    (when (time-less-p (nth 5 (file-attributes file)) week-ago)
      (delete-file file)))))

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

(defun my/org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
                                        ; take screenshot
  (if (eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" filename))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
                                        ; insert into file if correctly taken
  (if (file-exists-p filename)
      (insert (concat "[[file:" filename "]]"))))

;; Font needs to be installed in the Mac Font Book

(when (display-graphic-p)
  (set-face-attribute 'default nil :family "M Plus 1 Code" :height 170)
  (set-face-attribute 'variable-pitch nil :family "Concourse T3" :height 1.2)
  (set-face-attribute 'fixed-pitch nil :family "Sarasa Term Slab J" :height 170))

(setq display-line-numbers 'visual)
(setq display-line-numbers-type 'relative)

(use-package modus-themes
  :custom
  (modus-themes-disable-other-themes t)
  (modus-themes-mode-line '(accented borderless))
  (modus-themes-italic-constructs nil)
  (modus-themes-bold-constructs nil)
  (modus-themes-paren-match '(bold intense))
  (modus-themes-use-slanted-constructs t)
  (modus-themes-mixed-fonts t)
  :config
  (modus-themes-load-theme 'modus-vivendi-tinted))

(use-package spacious-padding
  :custom
  (spacious-padding-subtle-mode-line t)
  :init
  (spacious-padding-mode 1))

(use-package minions
  :custom
  (minions-mode-line-delimiters '("" . ""))
  (minions-mode-line-lighter "|")
  ;; Other modes related to information displayed on mode-line
  (column-number-mode +1)
  (display-time-mode +1)
  (display-time-default-load-average nil)
  (display-time-format "%Y-%m-%d %H:%M")
  :init
  (minions-mode 1))

;;; Initialize `general` for keybindings
(use-package general
  :config
  (general-create-definer spc-key-definer
    :states '(normal visual insert motion emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC"
    :prefix-map 'dominant-prefix-map))

(with-eval-after-load 'evil
  (spc-key-definer
    "TAB" 'my/toggle-buffers
    "pp"  'projectile-switch-project
    "pf"  'consult-find ;'projectile-find-file
    "/"   'consult-git-grep ;'consult-ripgrep
    "bb"  'consult-buffer
    "rr"  'consult-recent-file
    "u"   'universal-argument))

(with-eval-after-load 'evil
  (general-define-key
   :states 'normal
   :keymaps 'process-menu-mode-map
   "d" 'process-menu-delete-process))

(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

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
  (evil-collection-init '(magit dired wgrep org)))

(use-package evil-nerd-commenter
  :config
  (general-define-key
  "M-;" 'evilnc-comment-or-uncomment-lines))

(use-package vertico
  :init
  (vertico-mode))

;;Persists history between restarts, vertico sorts by history position. 
(use-package savehist
  :init
  (savehist-mode))

(use-package vertico-prescient
  :after vertico
  :init (vertico-prescient-mode +1))

(use-package consult
  :after projectile
  :config
  ;; This is to prevent consult-find from picking up node_modules.  For more, see:
  ;; https://github.com/minad/consult/wiki#skipping-directories-when-using-consult-find
  (setq consult-find-args "find . -not ( -wholename */.* -prune -o -name node_modules -prune )"))

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

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-," . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
   ("M-." . embark-occur)       ;; occur-edit-mode
   ;;("M-;" . embark-export)         ; export current view
   )

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
 )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (evil-make-overriding-map wgrep-mode-map 'normal)
  (evil-make-overriding-map wgrep-mode-map 'visual)
  (evil-make-overriding-map wgrep-mode-map 'motion))

(use-package ace-window
  :init
  (ace-window-display-mode 1)
  :config
  (general-define-key
  "M-o" 'ace-window))

(use-package golden-ratio
  :after ace-window
  :init
  ;; Exclude vterm buffers and enable mode (simple setup)
  (setq golden-ratio-exclude-modes '(vterm-mode vterm-copy-mode))
  (golden-ratio-mode 1)
  :config
  (add-to-list 'golden-ratio-extra-commands 'ace-window))

(use-package origami
  :config
  (add-hook 'js-to-mode 'origami-mode))

(use-package flycheck
  :hook ((prog-mode . flycheck-mode))
  :config
  (setq flycheck-indication-mode 'left-margin)
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-check-syntax-automatically '(save mode-enabled newline))
  (setq flycheck-display-errors-delay 0.1))

(use-package quick-peek)
(use-package flycheck-inline
  :after (flycheck quick-peek)
  :hook (flycheck-mode-hook . flycheck-inline-mode)
  :config
  (setq flycheck-inline-clear-delay 3)
  (setq flycheck-inline-display-function
	(lambda (msg pos err)
	  (let* ((ov (quick-peek-overlay-ensure-at pos))
		(contents (quick-peek-overlay-contents ov)))
	    (setf (quick-peek-overlay-contents ov)
		  (concat contents (when contents "\n") msg))
	    (quick-peek-update ov)))
	flycheck-inline-clear-function #'quick-peek-hide)
  )

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

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

(use-package magit
  :config
  (spc-key-definer "gs" 'magit-status))

(use-package git-link)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

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

(use-package tree-sitter
  :config
  (customize-set-variable 'treesit-font-lock-level 5)
  (setq treesit-language-source-alist
    '((elisp "https://github.com/Wilfred/tree-sitter-elisp")
      (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
      (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
      (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
      (clojure "https://github.com/sogaiu/tree-sitter-clojure" "master" "src")
	  (yaml "https://github.com/ikatyang/tree-sitter-yaml" "master" "src")
      (json "https://github.com/tree-sitter/tree-sitter-json" "master" "src")))
  (setq major-mode-remap-alist
    '((js2-mode . js-ts-mode)
      (typescript-mode . typescript-ts-mode)
      (rjsx-mode . tsx-ts-mode)
      (json-mode . json-ts-mode)
      (css-mode . css-ts-mode))))

;; Optimizations for lsp, see https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)

(use-package lsp-mode
  :defer t
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred
  :config
  (setq lsp-auto-configure t
        lsp-auto-guess-root t
        lsp-log-io t ;; speed
        lsp-restart t ;; b/c server dies
        lsp-eslint-enable t
	lsp-diagnostics-provider :flycheck
        ))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-max-width 100
        lsp-ui-doc-max-height 30
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'bottom
        
        ;; Turn off sideline diagnostics
	lsp-ui-sideline-enable nil
	lsp-ui-sideline-show-diagnostics nil
	lsp-ui-sideline-show-hover t
	lsp-ui-sideline-show-code-actions nil
        ))

(defun my/setup-lsp-company ()
  (setq-local company-backends
              '(company-capf company-dabbrev company-dabbrev-code)))

(add-hook 'lsp-completion-mode-hook #'my/setup-lsp-company)

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq
   company-minimum-prefix-length 2
   company-idle-delay 0.35
   company-tooltip-align-annotations t
   company-require-match nil     ;; allow free typing
   company-dabbrev-ignore-case t ;; don't ignore case for completions
   company-dabbrev-downcase t    ;; don't downcase completions
   ))

(use-package eca
  :straight (eca
	     :type git
	     :host github
	     :repo "editor-code-assistant/eca-emacs"
	     :files ("*.el"))
  :config
  (setenv "OPENAI_API_KEY" secret/openai-api-key)
  (setenv "CLAUDE_API_KEY" secret/claude-api-key)
  )

(setq js-indent-level 2)

(use-package add-node-modules-path
  :defer t
  :hook (((js2-mode rjsx-mode) . add-node-modules-path)))

;; rjsx-mode extends js2-mode, so it provides js2-mode plus functionality for jsx
(use-package rjsx-mode
  :defer t
  :mode ("\\.jsx?\\'" "\\.tsx?\\'" "\\.m?js\\'")
  :hook (((js2-mode
           rjsx-mode
	   js-ts-mode
	   typescript-ts-mode
	   tsx-ts-mode
           ) . lsp-deferred)) ;; enable lsp-mode
  :config
  (setq lsp-auto-guess-root t)
  ;; (setq lsp-diagnostic-package :none)
  (setq lsp-idle-delay 1.0)
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (define-key rjsx-mode-map ">" nil)

  ;; setup hook to enable line numbers mode
  (add-hook 'rjsx-mode-hook 'display-line-numbers-mode)
  )

(use-package prettier-js
  :defer t
  :diminish prettier-js-mode
  :hook (((js2-mode rjsx-mode js-ts-mode tsx-ts-mode typescript-ts-mode) . prettier-js-mode))
  )

(use-package jest-test-mode 
  :commands jest-test-mode
  :hook (typescript-mode js-mode typescript-tsx-mode))

(use-package graphql-mode)

(use-package clojure-mode
  :defer t
  :hook ((clojure-mode . lsp-deferred))) 

(use-package clojure-ts-mode
  :defer t)

(use-package cider
  :defer t
  :config
  (setq cider-repl-pop-to-buffer-on-connect nil))

(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package company
  :config
  (progn
    (add-hook 'cider-repl-mode-hook #'company-mode)
    (add-hook 'cider-mode-hook #'company-mode)))

(use-package clay
  :straight (clay
             :type git
             :host github
             :repo "scicloj/clay.el"))

(use-package yaml-mode
    :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
          ("Procfile\\'" . yaml-mode))
    :config (add-hook 'yaml-mode-hook
                      #'(lambda ()
                        (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package scss-mode
  :mode (("\\.scss\\'" . scss-mode)
         ("\\.css\\'" . scss-mode))
  :config
  ;; set the css-indent-offset to 2
  (setq css-indent-offset 2))

(use-package ruby-mode
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :mode "Berksfile\\'"
  :mode "Vagrantfile\\'"
  :interpreter "ruby"

  :init
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil)
  (add-hook 'ruby-mode 'superword-mode)
  )

(use-package rubocop
  :init
  (add-hook 'ruby-mode-hook 'rubocop-mode)
  :diminish rubocop-mode)

(setq org-directory "~/org")
(setq org-log-into-drawer t)
(setq org-export-backends '(md html odt latex))

(use-package org
  :bind
  ("C-c c" . org-capture)

  :custom
  (org-startup-indented t) ;; enables indent mode
  (org-catch-invisible-edits 'error) ;; errors if trying to add character to invisible region
  (org-tags-column 0) ;; tags immediately after header text
  (org-auto-align-tags nil) ;; do not align tags between header
  (org-pretty-entities t) ;; use UTF-8 characters
  ;; Don't interpret underscores as TEX syntax.
  (org-use-sub-superscripts nil)
  (org-attach-id-dir (expand-file-name "assets" org-roam-directory))
  (org-link-file-path-type 'relative)

  :config
  ;; Ellipsis styling
  (setq org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

  (defun my/org-open-jira-issue ()
    (interactive)
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward "EPD-[0-9]+" (line-end-position) t)
	(let ((issue (match-string 0)))
	  (browse-url (format "https://primary.atlassian.net/browse/%s" issue)))))
    nil))

(with-eval-after-load 'modus-themes
  (setq modus-themes-scale-headings t)
  (setq modus-themes-headings '((1 . (rainbow overline background 1.3))
				(2 . (rainbow background 1.2))
				(3 . (rainbow bold 1.1))
				(4 . (semilight 1.0))))
  (modus-themes-load-theme 'modus-vivendi-tinted))

(use-package olivetti
  :hook
  (org-mode . olivetti-mode)
  :custom
  (olivetti-body-width 0.75)
(olivetti-style 'fancy))

;; Setup status tags
(setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t)" "STARTED(s)" "REVIEW(r)" "|" "BLOCKED(b!)" "DONE(d!)" "CANCELED(c!)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#ff39a3" :weight bold))
	("STARTED" . "#E35DBF")
	("REVIEW" . "lightblue")
	("BLOCKED" . "pink")
	("CANCELED" . (:foreground "white" :background "#4d4d4d" :weight bold))
	("DONE" . "#008080")))

(require 'org-tempo)
(with-eval-after-load 'org-tempo
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package org-journal
  :defer t
  :after (org-roam)
  :init
  (general-define-key
      :prefix "C-c"
      "C-j" nil ;; override default C-j binding for org-journal
      "C-j o" 'org-journal-open-current-journal-file
      "C-j n" 'org-journal-new-entry
      "C-j d" 'org-journal-new-date-entry)
    :config
    ;; set org-journal-dir by concatting org-roam-directory with "journals"
    (setq org-journal-dir (concat org-roam-directory "journals/"))
    (setq org-agenda-file-regexp "\\`\\([^.].*\\.org\\|[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org\\(\\.gpg\\)?\\)\\'")
    (setq org-journal-file-type 'daily)
    (setq org-journal-file-format "%Y-%m-%d.org")
    (setq org-journal-date-format "%B %d %Y")
    (setq org-journal-carryover-items "TODO=\"TODO\"|TODO=\"STARTED\"|TODO=\"REVIEW\"|TODO=\"BLOCKED\"")
    (setq org-journal-find-file #'find-file-other-window)
    (defun org-journal-file-header-func (time)
      "Custom function to create journal header."
      (concat
      (pcase org-journal-file-type
	(`daily (format-time-string "#+TITLE: %Y-%m-%d\n#+STARTUP: folded")))))
    (setq org-journal-file-header 'org-journal-file-header-func)
    ;;(setq org-journal-after-header-create-hook #'org-id-get-create)
    )

(use-package emacsql)
;; (use-package emacsql-sqlite)

(use-package org-roam
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n t" . org-roam-dailies-find-today))
  :init
  ;; my setup now is that i'm using multiple notes folders
  ;; the org-roam-directory and other org-roam settings are
  ;; set locally by .dir-locals.el files. But we'll start here
  ;; with a "default" on load setting for org-roam-directory.
  (setq org-roam-directory "~/org/techwork/")
  (setq org-roam-dailies-directory "journals/")
  :config
  (setq org-roam-file-exclude-regexp "\\.git/.*\\|logseq/.*$"
        org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           ;; Accomodates for the fact that Logseq uses the "pages" directory
           :target (file+head "pages/${slug}.org" "#+title: ${title}\n")
           :unnarrowed t))
        org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org" ;; format matches Logseq
                              "#+title: %<%Y-%m-%d>\n"))))
  )

(use-package logseq-org-roam
 :straight (:host github
            :repo "sbougerel/logseq-org-roam"
            :files ("*.el")))

(use-package consult-org-roam
  :after org-roam
  :init
  (require 'consult-org-roam)
  (consult-org-roam-mode 1) ;; activate minor mode
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  :config
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-.")
   )
  :bind
  ("C-c n e" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n B" . consult-org-roam-backlinks-recursive)
  ("C-c n l" . consult-org-roam-forward-links)
  ("C-c n r" . consult-org-roam-search))

(use-package org-contrib
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

(setq org-capture-templates
  ;; Wine Template
  `(("w" "Wine Tasting Note" entry (file "~/org/wines.org")
      ,(concat "* %^{Wine}\n"
	      ":PROPERTIES:\n"
	      ":AddedOn: %u\n"
	      ":TastedOn: %^{TastedOn (as yyyy-mm-dd)}\n"
	      ":Vintage: %^{Vintage}\n"
	      ":Varietal: %^{Varietal}\n"
	      ":Region: %^{Region}\n"
	    ":SubRegion: %^{SubRegion}\n"
	      ":Country: %^{Country}\n"
	      ":Rating: %^{Rating}\n"
	      ":END:\n\n%^{Tasting Note}\n%i\n"
	      ))
      ("c" "Coffee Tasting Note" entry (file "~/org/coffee.org")
	  ,(concat "* %^{Coffee}\n"
		    ":PROPERTIES:\n"
		    ":TastedOn: %^{Date Tasted (as yyyy-mm-dd)}\n"
		    ":Variety: %^{Variety}\n"
		    ":Country: %^{Country}\n"
		    ":Region: %^{Region}\n"
		    ":Process: %^{Washed/Natural...}\n"
		    ":Roaster: %^{Roaster}\n"
		    ":FlavorNotes: %^{Flavor Notes}\n"
		    ":Rating: %^{Rating}\n"
		    ":END:\n\n%^{Tasting Note}\n%i\n"
		    ))))

(use-package org-download
  :custom
  (org-download-method 'attach)
  (org-download-link-format "[[file:%s]]"))

(use-package vterm
  :hook (vterm-mode . goto-address-mode)
  :custom
  (vterm-toggle-fullscreen-p nil)
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  (vterm-mouse-mode t)
  :config
  ;; Disable evil mode in vterm
  (setq evil-emacs-state-cursor nil)
  (add-hook 'vterm-mode-hook
          (lambda () (evil-emacs-state))))

(defun my/vterm-jack-in ()
    "Toggle vterm and jack into a tmux session named after the project folder.
Only sends the tmux command once per vterm buffer to avoid nesting warnings."
    (interactive)
    (require 'project) (require 'vterm)
    (let* ((root (or (when-let ((p (project-current)))
  		     (project-root p))
  		   default-directory))
           (session (file-name-nondirectory (directory-file-name root)))
           (cmd (format "tmux new-session -As %s -c %s"
                        (shell-quote-argument session)
                        (shell-quote-argument (expand-file-name root)))))
      (if (fboundp 'vterm-toggle) (vterm-toggle) (vterm))
      ;; Only send the tmux attach command if we haven't already done so
      (when (derived-mode-p 'vterm-mode)
        (unless (and (boundp 'my/vterm-tmux-attached-p)
                     my/vterm-tmux-attached-p)
          (vterm-send-string cmd)
          (vterm-send-return)
          (setq-local my/vterm-tmux-attached-p t)
          (setq-local my/vterm-tmux-session session)
          (setq-local my/vterm-project-root root)))))

(use-package vterm-toggle
  :commands (vterm-toggle vterm-toggle-cd)
  :bind
  (
   ;; ("s-t" . vterm-toggle)
   ("s-t" . my/vterm-jack-in) ;; for termux
   ("s-T" . vterm-toggle-cd))
  :custom
  (vterm-toggle-scope 'project)
  (vterm-toggle-project-root t)
  (vterm-toggle-fullscreen-p nil)
  :config
  ;; Show vterm in a 30% tall bottom side window
  (add-to-list 'display-buffer-alist
               '((lambda (buf _)
                   (with-current-buffer buf
                     (derived-mode-p 'vterm-mode)))
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3))))

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-api-key secret/openai-api-key))

;; This registers an autoloaded command for pdf-view-mode, defers
;; loading of pdf-tools, and runs pdf-view-mode if the beginning of a
;; buffer matches the string "%PDF".
(use-package pdf-tools
  :load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

(use-package denote
  :custom
  (denote-directory "~/Documents/2025")
  :config
  (defun my/denote-rename-file-date ()
    (declare (interactive-only t))
    (interactive)
    (let ((denote-prompts (denote-add-prompts '(date))))
      (call-interactively #'denote-rename-file))))
