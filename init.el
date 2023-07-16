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
(global-visual-line-mode t)                                  ;; Do not break the lines

;;======================================================================
;; Package config
;;======================================================================

(require 'package)
(setq package-enable-at-startup nil) ; disable package init
  
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

(use-package try
  :ensure t)

;; Major mode for several modes in one buffer
(use-package polymode
  :ensure t)

;; ;; Little templates for programming languages :)
;; (use-package yasnippet
;;   :ensure t)
;; (use-package yasnippet-snippets
;;   :ensure t)
;; ;; Turning on this mode
;; (yas-global-mode t)

;; Ido mode
(require 'ido)
    (ido-mode t)

;; Install flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'typescript-mode-hook 'flycheck-mode))

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

;; Install down below using M-x
;; (use-package all-the-icons-install-fonts
;;   :ensure t)

;; install and config neotree
(use-package neotree
  :ensure t
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; install ace-window
(use-package ace-window
  :ensure t)

;; A better syntax highlight
(use-package tree-sitter
  :ensure t)
(use-package tree-sitter-langs
  :ensure t)
(use-package tree-sitter-indent
  :ensure t)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; Langs of tree-sitter
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))

;; Org mode
(use-package org
  :ensure t)

;; active Org Babel for languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (R . t)
   (emacs-lisp . nil)))

;; Install web-mode
(use-package web-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))

;; Install magit 
;; (use-package magit
;;    :ensure t)

;; Install Projectile
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))
(setq projectile-project-search-path '("~/projects/"))

;;======================================================================
;; R configs.
;;======================================================================

;; ESS
(use-package ess
  :ensure t)
(setq-default ess-dialect "R")
(setq-default inferior-R-args "--no-restore-history --no-save ")

;; Down below is workaround to solve
;; the damn problem with fancy R comments in ESS mode.
;; https://github.com/emacs-ess/ESS/issues/1175
;; (setq ess-indent-with-fancy-comments nil)
(setf (cdr (assoc 'ess-indent-with-fancy-comments ess-own-style-list)) nil)

(use-package poly-markdown
             :ensure t)
(use-package poly-R
             :ensure t)
(add-to-list 'auto-mode-alist '("\\.[Rr]md" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))

(use-package ess-view
  :ensure t)
(setq ess-view--spreadsheet-program "gnumeric")

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
;; Python configs.
;;======================================================================

;; elpy
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

;; Enable elpy
(elpy-enable)
(setq python-shell-completion-native-disabled-interpreters '("python3"))

;; Enable Flycheck for python
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;;======================================================================
;; Typescript configs.
;;======================================================================

;;(use-package typescript-mode
;;  :ensure t
;;  :config
;;  (setq typescript-indent-level 2)
;;  (add-hook 'typescript-mode #'subword-mode))

;;======================================================================
;; Scala configs.
;;======================================================================

;; (use-package scala-mode
;;   :ensure t)

;; Enable sbt mode for executing sbt commands
;; (use-package sbt-mode
;;   :commands sbt-start sbt-command
;;   :config
;;   ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
;;   ;; allows using SPACE when in the minibuffer
;;   (substitute-key-definition
;;    'minibuffer-complete-word
;;    'self-insert-command
;;    minibuffer-local-completion-map)
;;    ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
;;    (setq sbt:program-options '("-Dsbt.supershell=false")))

;;======================================================================
;; Groovy configs.
;;======================================================================

;; (use-package groovy-mode
;;   :ensure t)

;;======================================================================
;; C# configs.
;;======================================================================

;; OK, there is a kind of heresy here.
;; But I don't fucking mind
(use-package csharp-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode)))

(defun my-csharp-mode-hook ()
  ;; enable the stuff you want for C# here
  (electric-pair-mode 1)       ;; Emacs 24
  (electric-pair-local-mode 1) ;; Emacs 25
  )
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

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
;; (global-set-key (kbd "C-c l") #'org-store-link)
;; (global-set-key (kbd "C-c a") #'org-agenda)
;; (global-set-key (kbd "C-c c") #'org-capture)

;;======================================================================
;; Themes load
;;======================================================================

;; Themes :)
;; (use-package moe-theme
;;   :ensure t)
;; (load-theme 'moe-dark t)

;; (use-package ayu-theme
;;   :ensure t
;;   :config (load-theme 'ayu-dark t))

(load-theme 'tango-dark t)
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
   '(nord-theme all-the-icons-ibuffer all-the-icons-ivy-rich all-the-icons-dired scala-mode moe-theme flycheck elpy poly-R poly-markdown ess web-mode tree-sitter-langs tree-sitter ace-window neotree all-the-icons which-key markdown-mode highlight-parentheses company polymode try use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
