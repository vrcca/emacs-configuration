;; loads and install packages
(load (concat user-emacs-directory "init-packages"))

;; keeps emacs custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; =====================================
;; OLD CONFIGS BELOW ARE DEPRECATED
;; =====================================

;; enables undo tree mode globaly
(global-undo-tree-mode)

;; removes toolbar
(tool-bar-mode -1)

;; displays line numbers everywhere
(global-display-line-numbers-mode +1)

;; fades out inactive buffers
(dimmer-mode +1)

;; loads preferred theme
(load-theme 'monokai t)
;;(load-theme 'solarized-light t)
(add-to-list 'default-frame-alist '(ns-appearance . dark))
;; loads preferred font
(add-to-list 'default-frame-alist
             '(font . "JetBrains Mono"))

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

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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
      ns-pop-up-frames nil)
(global-set-key (kbd "C-.") 'imenu-anywhere)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-flycheck-mode 1)
(global-git-gutter-mode 1)
(global-hl-line-mode +1)

;; PACKAGE CONFIGURATIONS
;; default hooks
(--each '(clojure-mode-hook
          cider-repl-mode-hook
          elixir-mode-hook
          perl-mode-hook
          emacs-lisp-mode-hook)
  (add-hook it 'rainbow-delimiters-mode-enable))

(--each '(clojure-mode-hook
          cider-repl-mode-hook
          elixir-mode-hook
          emacs-lisp-mode-hook)
  (add-hook it 'whitespace-cleanup-mode))
(add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))

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

;; aggressive indent mode hooks
(--each '(clojure-mode-hook
          cider-repl-mode-hook
          emacs-lisp-mode-hook)
  (add-hook it 'aggressive-indent-mode))

;; ido configurations
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching 1
      ido-use-faces nil)
(ido-ubiquitous-mode 1)
(ido-vertical-mode 1)


;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; web-mode
;; loads JSP files with html-mode
(add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode))

;; go-mode
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))

;; Elixir hooks
;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
(add-hook 'elixir-mode-hook #'(lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
(add-hook 'elixir-mode-hook #'(lambda () (alchemist-mode)))
(add-hook 'elixir-format-hook (lambda ()
                                (if (projectile-project-p)
                                    (setq elixir-format-arguments
                                          (list "--dot-formatter"
                                                (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))
                                  (setq elixir-format-arguments nil))))
(add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))

;; setup smartparens
(--each '(elixir-mode-hook)
  (add-hook it 'turn-on-smartparens-mode))

;; perl-mode
(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))

;; dockerfile-mode
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Ruby Hooks
(add-hook 'ruby-mode-hook #'rubocopfmt-mode)
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;; =====================================
;; NEW CONFIGS USING use-package.
;; =====================================
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :config
  (use-package all-the-icons
    :ensure t))
(global-set-key [f8] 'neotree-project-dir)
(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))
(setq neo-autorefresh nil
      neo-smart-open t
      neo-window-width 30
      neo-create-file-auto-open nil
      neo-auto-indent-point nil
      neo-mode-line-type 'none
      neo-show-updir-line nil
      neo-theme 'icons
      neo-banner-message nil
      neo-show-hidden-files nil
      neo-keymap-style 'concise
      neo-show-hidden-files t
      neo-hidden-regexp-list
      '(;; vcs folders
        "^\\.\\(?:git\\|hg\\|svn\\)$"
        ;; compiled files
        "\\.\\(?:pyc\\|o\\|elc\\|lock\\|css.map\\|class\\)$"
        ;; generated files, caches or local pkgs
        "^\\(?:node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
        ;; org-mode folders
        "^\\.\\(?:sync\\|export\\|attach\\)$"
        ;; temp files
        "~$"
        "^#.*#$"))
(setq projectile-switch-project-action 'neotree-projectile-action)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :diminish lsp-mode
  :hook
  (elixir-mode . lsp)
  (ruby-mode . lsp)
  :init
  (add-to-list 'exec-path "~/.emacs.d/elixir-ls")
  :config
  (use-package lsp-ui
    :ensure t))

;; Only starts a Emacs server if there is none running
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package magit
  :ensure t)

(use-package ace-window
  :ensure t)
(global-set-key (kbd "M-o") 'ace-window)

(provide 'init)
;;; init.el ends here
