;; Remove welcome screen
(setq inhibit-startup-message t)

;; Remove default menu
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Remove scrollbar mode
(scroll-bar-mode -1)

;; Set line number global
(global-linum-mode t)

;;Font size
;;(set-face-attribute 'default nil :height 150)

;; Packages
(require 'package)
(setq package-enable-at-startup nil) ; disable package init

;; MELPA -> repo
(add-to-list 'package-archives
	     '("melpa-stable". "https://melpa.org/packages/"))

(package-initialize) ; init packages

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
  ;; :bind (("<f8>" . 'neotree-toggle)))

;; install ace-window
(use-package ace-window
  :ensure t)


;;(use-package ergoemacs-mode
;;  :ensure t
;;  :config
;;  (progn
;;    (setq ergoemacs-theme nil)
;;    (setq ergoemacs-keyboard-layout "us")
;;    (ergoemacs-mode 1)))

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
;;(global-set-key (kbd "\C-<right>") 'next-window)
;;(global-set-key (kbd "\C-<left>") 'previous-window)
;;(global-set-key (kbd "M-o") 'ace-window)

;;Theme set
;;(use-package zenburn-theme
;;  :ensure t)
;;(load-theme 'zenburn t)

;; (use-package gruvbox-theme
;;   :ensure t)
;; (load-theme 'gruvbox-dark-hard t)

(use-package dracula-theme
  :ensure t)
(load-theme 'dracula t)


;; MELPA stuff 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(zenburn-theme ace-window auto-complete which-key try use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
