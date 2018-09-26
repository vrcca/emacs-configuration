(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

;; All packages in this lists are automatically installed on startup
(setq package-list
      '(exec-path-from-shell
	multiple-cursors
	neotree
	dimmer
        rg
        org-bullets
        markdown-mode
	rainbow-delimiters
	projectile
	clojure-mode
	cider
	aggressive-indent
	elixir-mode
	alchemist
	smartparens
        paredit
	solarized-theme
	undo-tree
	flx-ido
	ido-completing-read+
	ido-vertical-mode
	company
        ;; project specifics
        mustache-mode
        terraform-mode
        docker-compose-mode
        ))

(package-initialize)

;; Refreshes package cache
(unless package-archive-contents
  (package-refresh-contents))

;; Automatically install new packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
