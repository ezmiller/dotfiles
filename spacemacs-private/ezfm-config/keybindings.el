;; Keybindings for scicloj/notespace
(add-hook 'clojure-mode-hook
          (lambda ()
            (local-set-key "\C-cy" 'notespace/compute-note-at-this-line)
            (local-set-key "\C-cu" 'notespace/compute-this-notespace)))

;; Trying this out - might work better w/ minivan keyboard
(define-key evil-insert-state-map (kbd "C-;") 'evil-normal-state)
;; Hmmm. Haven't been using this much...
(define-key evil-insert-state-map (kbd "C-c C-g m") 'golden-ratio-mode)

;; Shortcut for org files
(defun my-org-finder ()
  (interactive)
  (ido-find-file-in-dir org-directory))
(global-set-key (kbd "C-c f") 'my-org-finder)

;; Org-journal
(global-set-key (kbd "C-c j c") 'org-journal-open-current-journal-file)
