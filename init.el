;; loads and install packages
(load "~/.emacs.d/init-packages")

;; custom tweaks
(add-to-list 'exec-path "/usr/local/bin") ;; adds executables to execution-path
(tool-bar-mode -1) ;; removes toolbar
(setq inhibit-startup-screen t) ;; disables startup tutorial
(dimmer-mode +1) ;; visually highlight the selected buffer
(load-theme 'zenburn t) ;; Theme
(require 'smartparens-config)

;; clojure hooks
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

;; ido configurations
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching 1)
(setq ido-use-faces nil)

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
