;; loads and install packages
(load (concat user-emacs-directory "init-packages"))

;; keeps emacs custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; enables undo tree mode globaly
(global-undo-tree-mode)

;; removes toolbar
(tool-bar-mode -1)

;; displays line numbers everywhere
(global-display-line-numbers-mode +1)

;; fades out inactive buffers
(dimmer-mode +1)

;; loads preferred theme
(load-theme 'solarized-light t)

;; enables autocomplete
(add-hook 'after-init-hook 'global-company-mode)

;; automatically restores last session
(desktop-save-mode 1)

;; uses ibuffer instead of the default buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; switches windows using Meta + Arrows
(windmove-default-keybindings 'meta)

;; enables winner-mode
;; undo/redo window changes with C-c left/right
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; CUSTOM TWEAKS
;; loads $PATH into emacs
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(require 'smartparens-config) ;; loads smartparens
(show-paren-mode 1) ;; highlights matching parenthesis
(setq uniquify-buffer-name-style 'forward) ;; prepends path to filename in repeated buffer
(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      inhibit-startup-screen t
      ediff-window-setup-function 'ediff-setup-windows-plain
      savehist-mode t
      ns-pop-up-frames nil
      savehist-file (concat user-emacs-directory "savehist")
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; PACKAGE CONFIGURATIONS
;; setup smartparens
(--each '(elixir-mode-hook)
  (add-hook it 'turn-on-smartparens-mode))

;; default hooks
(add-hook 'prog-mode-hook #'aggressive-indent-mode)
(--each '(clojure-mode-hook
          cider-repl-mode-hook
          emacs-lisp-mode-hook)
  (add-hook it 'rainbow-delimiters-mode-enable))

;; clojure hooks
(--each '(clojure-mode-hook
          cider-repl-mode-hook
          emacs-lisp-mode-hook)
  (add-hook it 'enable-paredit-mode))

;; orgmode hooks
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(add-hook 'org-mode-hook #'(lambda ()
                             (org-bullets-mode) ;; Displays bullet points instead of asterisks
                             (visual-line-mode) ;; make the lines in the buffer wrap around the edges of the screen.
                             (org-indent-mode)))  ;; to press C-c q  or fill-paragraph ever again!

;; ido configurations
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching 1)
(setq ido-use-faces nil)
(ido-ubiquitous-mode 1)
(ido-vertical-mode 1)

;; neotree
(global-set-key [f8] 'neotree-toggle)

;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; html-mode
;; loads JSP files with html-mode
(add-to-list 'auto-mode-alist '("\\.jsp$" . html-mode))

