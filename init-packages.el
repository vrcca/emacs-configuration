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
        ripgrep
        org-bullets
        markdown-mode
	projectile
	aggressive-indent
	smartparens
	solarized-theme
        intellij-theme
        monokai-theme
	undo-tree
	flx-ido
	ido-completing-read+
	ido-vertical-mode
	company
        whitespace-cleanup-mode
        imenu-anywhere
        ;; language support
        elixir-mode   alchemist
        go-mode       company-go
	clojure-mode  cider       rainbow-delimiters        paredit
        web-mode
        ;; project specifics
        mustache-mode
        terraform-mode
        docker-compose-mode
        dockerfile-mode
        json-mode
        magit
        ))

(package-initialize)

;; Refreshes package cache
(unless package-archive-contents
  (package-refresh-contents))

;; Automatically install new packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
