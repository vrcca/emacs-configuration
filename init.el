;; loads and install packages
(load (concat user-emacs-directory "init-packages"))

;; CUSTOM TWEAKS
(global-undo-tree-mode)
;; loads $PATH into emacs
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(tool-bar-mode -1) ;; removes toolbar
(global-display-line-numbers-mode +1)
(dimmer-mode +1) ;; visually highlight the selected buffer
(load-theme 'solarized-light t) ;; Theme
(require 'smartparens-config) ;; loads smartparens
(savehist-mode t) ;; saves the buffer history
(add-hook 'after-init-hook 'global-company-mode) ;; enables autocomplete
(desktop-save-mode 1) ;; automatically reloads last session
(setq uniquify-buffer-name-style 'forward) ;; prepends path to filename in repeated buffer
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; opens ibuffer instead
(show-paren-mode 1) ;; highlights matching parenthesis
(setq save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        require-final-newline t
        visible-bell t
        load-prefer-newer t
	inhibit-startup-screen t
        ediff-window-setup-function 'ediff-setup-windows-plain
	savehist-file (concat user-emacs-directory "savehist")
        save-place-file (concat user-emacs-directory "places")
        backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups"))))

;; PACKAGE CONFIGURATIONS
;; clojure hooks
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
;; elixir hooks
(add-hook 'elixir-mode-hook #'smartparens-mode)

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

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
