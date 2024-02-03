;;======================================================================
;; Configuration file to Emacs by Joaquim Almeida
;;
;; This file is hosted at https://github.com/joaq-almeida/emacs-config
;;
;; Most of all the content available here was obtained/inspired by
;; Prof Walmes Zeviani ~/.emacs.d from Dept of Statistics from UFPR.
;; His repo: https://github.com/walmes/emacs
;;
;; Please, send questions, problems and/or
;; suggestions as an issue on GitHub project of this file.
;;======================================================================

(setq inhibit-startup-message t)                             ;; Remove welcome screen
(tool-bar-mode -1)                                           ;; Remove tool menu
(menu-bar-mode -1)                                           ;; Remove bar menu
(show-paren-mode t)                                          ;; Highlight matching pair
(setq auto-save-default nil)                                 ;; Disable #autosave#
(setq make-backup-files nil)                                 ;; Disable backup~
(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ;; Start frame with full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Start every frame maximazed
(scroll-bar-mode -1)                                         ;; Remove scrollbar mode
(global-linum-mode t)                                        ;; Set line number global
(global-visual-line-mode t)                                  ;; Do not wrap lines
(toggle-truncate-lines -1)                                   ;; Do not truncate lines 
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
	("melpa-stable". "https://stable.melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

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
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; install and config neotree
(use-package neotree
  :ensure t
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; Install web-mode
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2
        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        ))

;;======================================================================
;; Markdown configs.
;;======================================================================

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;;======================================================================
;; Poly-Markdown configs.
;;======================================================================
 
(use-package poly-markdown
             :ensure t)
(use-package poly-R
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.[Rr]md" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))

;;======================================================================
;; R configs.
;;======================================================================

;; ESS
(use-package ess
  :ensure t)
(setq-default ess-dialect "R")
(setq-default inferior-R-args "--no-restore-history --no-save ")

;; calling custom funcs
;; (byte-compile-file "~/.emacs.d/funcs.el")
;; (require 'funcs "~/.emacs.d/funcs.el")

;; Down below is a workaround to solve
;; the damn problem with fancy R comments in ESS mode.
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
;; Org mode config.
;;======================================================================

(defun org-mode-setup
  (org-indent-mode)
  (variable-pitch-mode)
  (visual-line-mode 1))

(use-package org
  :ensure t
  :no-require
  ;; :straight nil
  :hook (org-mode . org-mode-setup))

;; active Org Babel for languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (R . t)
   (latex . t)
   (emacs-lisp . t)))

(setq org-confirm-babel-evaluate nil)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)   
(add-hook 'org-mode-hook 'org-display-inline-images)   
(push '("conf-unix" . conf-unix) org-src-lang-modes)

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

;;======================================================================
;; key shortcuts.
;;======================================================================

(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<right>") 'shrink-window-horizontally)
(global-set-key (kbd "M-9") 'open-shell-split-window)
(global-unset-key "\C-z")
(global-set-key "\C-z" 'undo)
(global-set-key [f8] 'neotree-toggle)
(global-set-key [f9] 'neotree-dir)

;;======================================================================
;; Themes load
;;======================================================================

;; Themes :))
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
   '(elpy ess poly-R poly-markdown web-mode neotree all-the-icons which-key highlight-parentheses try polymode use-package cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
