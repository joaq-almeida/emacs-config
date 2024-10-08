;;======================================================================
;; Emacs configuration file written by Joaquim Almeida
;;
;; This file is hosted at https://github.com/joaq-almeida/emacs-config
;;
;; Most of the content available here was obtained/inspired by
;; Prof Walmes Zeviani ~/.emacs.d from Dept of Statistics from UFPR.
;; His repo: https://github.com/walmes/emacs
;;
;; Please, send questions, problems and/or
;; suggestions as an issue on GitHub project for this file.
;;======================================================================

(setq inhibit-startup-message t)                             ;; Remove welcome screen
(tool-bar-mode -1)                                           ;; Remove tool menu
;; (menu-bar-mode -1)                                        ;; Remove bar menu
(show-paren-mode t)                                          ;; Highlight matching pair
(setq auto-save-default nil)                                 ;; Disable #autosave#
(setq make-backup-files nil)                                 ;; Disable backup~
(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ;; Start frame with full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Start every frame maximazed
(scroll-bar-mode -1)                                         ;; Remove scrollbar mode
(global-display-line-numbers-mode t)                         ;; Display line numbers
;; (global-visual-line-mode t)                               ;; Do not wrap lines
;; (toggle-truncate-lines -1)                                ;; Do not truncate lines 
(require 'generic-x)                                         ;; A better highlight

;;======================================================================
;; Fonts config
;;======================================================================
(set-frame-font "DejaVu Sans Mono Bold" nil t)

;;======================================================================
;; Package config
;;======================================================================

(require 'package)
(setq package-enable-at-startup nil) ;; disable package init
 
;; MELPA repos
 (setq package-archives
      '(("melpa". "https://melpa.org/packages/")
	("melpa-stable". "https://stable.melpa.org/packages/")))

(package-initialize) 
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;======================================================================
;; Packages
;;======================================================================

;; Package for trying install packages
(use-package try
  :ensure t)

;; Major mode for several modes in one buffer
(use-package polymode
  :ensure t)

;; Mode for code completion
;; Needs to install from M-x
;; Otherwise it will not work.
(use-package auto-complete
  :ensure t)
(require 'auto-complete-config)
(ac-config-default)

;; Ido mode
(require 'ido)
(ido-mode t)

;; Parentheses
(use-package highlight-parentheses
  :ensure t
  :config
  (progn
    (highlight-parentheses-mode)
    (global-highlight-parentheses-mode)))

;; set a package helper for commands
(use-package which-key
  :ensure t
  :config
  (progn
    (which-key-setup-side-window-right-bottom)
    (which-key-mode)))

;; download icons
;; Needs to install from M-x
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; install and config neotree
(use-package neotree
  :ensure t
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;;======================================================================
;; R configs.
;;======================================================================

;; ESS
(use-package ess
  :ensure t)
(setq-default ess-dialect "R")
(setq-default inferior-R-args "--no-restore-history --no-save ")

;; Down below is a workaround to solve
;; the problem with fancy R comments in ESS mode.
;; https://github.com/emacs-ess/ESS/issues/1175
;; (setq ess-indent-with-fancy-comments nil)
(setf (cdr (assoc 'ess-indent-with-fancy-comments ess-own-style-list)) nil)

;; Script and console highlight
(setq ess-R-font-lock-keywords
      '((ess-R-fl-keyword:modifiers . t)
        (ess-R-fl-keyword:fun-defs . t)
        (ess-R-fl-keyword:keywords . t)
        (ess-R-fl-keyword:assign-ops . t)
        (ess-R-fl-keyword:constants . t)
        (ess-fl-keyword:fun-calls . t)
        (ess-fl-keyword:numbers . t)
        (ess-fl-keyword:operators . t)
        (ess-fl-keyword:delimiters . t)
        (ess-fl-keyword:= . t)
        (ess-R-fl-keyword:F&T . t)))
(setq inferior-R-font-lock-keywords
      '((ess-S-fl-keyword:prompt . t)
        (ess-R-fl-keyword:messages . t)
        (ess-R-fl-keyword:modifiers . t)
        (ess-R-fl-keyword:fun-defs . t)
        (ess-R-fl-keyword:keywords . t)
        (ess-R-fl-keyword:assign-ops . t)
        (ess-R-fl-keyword:constants . t)
        (ess-fl-keyword:matrix-labels . t)
        (ess-fl-keyword:fun-calls . t)
        (ess-fl-keyword:numbers . t)
        (ess-fl-keyword:operators . t)
        (ess-fl-keyword:delimiters . t)
        (ess-fl-keyword:= . t)
        (ess-R-fl-keyword:F&T . t)))

;;======================================================================
;; Poly and Markdown configs.
;;======================================================================

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(use-package poly-R
  :ensure t)
(require 'poly-R)

(add-to-list 'auto-mode-alist
             '("\\.[rR]md\\'" . poly-gfm+r-mode))
(setq markdown-code-block-braces t)

;;======================================================================
;; Elisp functions.
;;======================================================================

(defun open-shell-split-window ()
  "Open shell and split window."
  (interactive)
  (split-window)
  (eshell)
  (previous-buffer) ;; 1
  (other-window 1)
  (next-buffer)     ;; 2
  (other-window 1))

(defun new-code-chunk ()
  (interactive)
  (if (derived-mode-p 'ess-mode)
      (insert "```\n\n```{r}\n")
    (insert "```{r}\n\n```")
    (forward-line -1)))

(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))

;;======================================================================
;; key shortcuts.
;;======================================================================

(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<right>") 'shrink-window-horizontally)
(global-set-key (kbd "M-9") 'open-shell-split-window)
(global-unset-key "\C-z")
(global-set-key "\C-z" 'undo)
(global-set-key [f8] 'neotree-toggle)
(global-set-key [f9] 'neotree-dir)
(global-set-key (kbd "C-c i") 'new-code-chunk)
(global-set-key (kbd "C-c d") 'duplicate-line)

;;======================================================================
;; Themes load
;;======================================================================

(load-theme 'tango-dark t)
;; (use-package nord-theme
;;  :ensure t)
;; (load-theme 'nord t)

;;======================================================================
;; MELPA stuffs
;;======================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(quarto-mode poly-R markdown-mode ess neotree all-the-icons which-key highlight-parentheses auto-complete polymode try)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
