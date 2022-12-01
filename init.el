;; Remove welcome screen
(setq inhibit-startup-message t)

;; Remove default menu
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Highlight matching pair
(show-paren-mode 1)

;; Disable #autosave#
(setq auto-save-default nil)

;; Disable backup~
(setq make-backup-files nil)

;; Start frame with full screen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Start every frame maximazed
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Remove scrollbar mode
(scroll-bar-mode -1)

;; Set line number global
(global-linum-mode t)

;; Packages
(require 'package)
(setq package-enable-at-startup nil) ; disable package init

;; MELPA -> repo
(add-to-list 'package-archives
	     '("melpa". "https://melpa.org/packages/"))

(package-initialize) ; init packages

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Install helm
;; (when (not (package-installed-p 'helm))
;;   (package-install 'helm))

;; Install web-mode
(use-package web-mode
  :ensure t)

;; Install Markdown mode
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "markdown"))

(use-package try
  :ensure t)

;; set a package helper for commands
(use-package which-key
  :ensure t
  :config
  (progn
    (which-key-setup-side-window-right-bottom)
    (which-key-mode)))

;; set autocomplete
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))

;; download icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; install and config neotree
(use-package neotree
  :ensure t
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; install ace-window
(use-package ace-window
  :ensure t)

;; Install magit 
(use-package magit
  :ensure t)

;; key shortcuts
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<right>") 'shrink--window-horizontally)
(global-set-key (kbd "M-9") 'eshell)
(global-unset-key "\C-z")
(global-set-key "\C-z" 'undo)
(global-set-key [f8] 'neotree-toggle)
(global-set-key [f9] 'neotree-dir)
(global-set-key (kbd "M-o") 'ace-window)

;; Themes :)
;; (use-package moe-theme
;;   :ensure t)
;; (load-theme 'moe-dark t)

(load-theme 'tango-dark t)

;; Melpa Stuff

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(web-mode markdown-mode zenburn-theme which-key use-package try subatomic256-theme subatomic-theme spacemacs-theme solarized-theme projectile neotree moe-theme magit helm gruvbox-theme gruber-darker-theme gotham-theme ergoemacs-mode dracula-theme color-theme-sanityinc-tomorrow auto-complete all-the-icons ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
