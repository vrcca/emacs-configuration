;; loads and install packages
(load "~/.emacs.d/init-packages")

;; CUSTOM TWEAKS
(global-undo-tree-mode)
;; loads $PATH into emacs
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(tool-bar-mode -1) ;; removes toolbar
(global-display-line-numbers-mode +1)
(setq inhibit-startup-screen t) ;; disables startup tutorial
(dimmer-mode +1) ;; visually highlight the selected buffer
(load-theme 'solarized-light t) ;; Theme
(require 'smartparens-config) ;; loads smartparens
;; saves the buffer history
(savehist-mode t) ;; saves the buffer history
(setq savehist-file "~/.emacs.d/savehist") ;; this is gitignored
(add-hook 'after-init-hook 'global-company-mode) ;; enables autocomplete
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(desktop-save-mode 1) ;; automatically reloads last session

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
