;;======================================================================
;; Configuration file to Emacs by Joaquim Almeida
;;
;; This file is hosted at https://github.com/joaq-almeida/emacs-config.
;;
;; Almost all the content available here was obtained/inspired by
;; queries on the internet. Please, send questions, problems and/or
;; suggestions as an issue on GitHub project of this file.
;;======================================================================

(setq inhibit-startup-message t)                             ;; Remove welcome screen
(tool-bar-mode -1)                                           ;; Remove tool menu
(menu-bar-mode -1)                                           ;; Remove bar menu
(show-paren-mode 1)                                          ;; Highlight matching pair
(setq auto-save-default nil)                                 ;; Disable #autosave#
(setq make-backup-files nil)                                 ;; Disable backup~
(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ;; Start frame with full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Start every frame maximazed
(scroll-bar-mode -1)                                         ;; Remove scrollbar mode
(global-linum-mode t)                                        ;; Set line number global

;;======================================================================
;; Package config
;;======================================================================

(require 'package)
(setq package-enable-at-startup nil) ; disable package init

;; MELPA -> repo
(add-to-list 'package-archives
	     '("melpa". "https://melpa.org/packages/"))

(package-initialize) 
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;======================================================================
;; Packages
;;======================================================================

;; ESS
(use-package ess
  :ensure t)

;;set autocomplete
;; (use-package auto-complete
;;   :ensure t
;;   :init
;;   (progn
;;     (ac-config-default)
;;     (global-auto-complete-mode t)))


;; set autocomplete
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (global-company-mode t))

;; Parentheses
(use-package highlight-parentheses
  :ensure t
  :config
  (progn
    (highlight-parentheses-mode)
    (global-highlight-parentheses-mode)))


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

;; Install web-mode
;; (use-package web-mode
;;   :ensure t)


;;======================================================================
;; Python configs.
;;======================================================================

;; elpy
;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable))

;; Enable elpy
;; (elpy-enable)

;; Enable Flycheck
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

;;======================================================================
;; key shortcuts.
;;======================================================================

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

;;======================================================================
;; Themes load
;;======================================================================

;; Themes :)
;; (use-package moe-theme
;;   :ensure t)
;; (load-theme 'moe-dark t)

(load-theme 'tango-dark t)

;;======================================================================
;; MELPA stuffs
;;======================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(elpy web-mode markdown-mode zenburn-theme which-key use-package try subatomic256-theme subatomic-theme spacemacs-theme solarized-theme projectile neotree moe-theme magit helm gruvbox-theme gruber-darker-theme gotham-theme ergoemacs-mode dracula-theme color-theme-sanityinc-tomorrow auto-complete all-the-icons ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
