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
(when (not (package-installed-p 'helm))
  (package-install 'helm))

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
;; (use-package dracula-theme
;;   :ensure t)
;; (load-theme 'dracula t)

;; (use-package gotham-theme
;;   :ensure t)
;; (load-theme 'gotham t)

;; (use-package misterioso-theme
;;   :ensure t)
;; (load-theme 'misterioso t)

;; (use-package zenburn-theme
;;   :ensure t)
;; (load-theme 'zenburn t)

;; (use-package solarized-theme
;;   :ensure t)
;; (load-theme 'solarized-dark t)

;; (use-package gruber-darker-theme
;;   :ensure t)
;; (load-theme 'gruber-darker t)

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
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(beacon-color "#cc6666")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#93E0E3")
 '(cua-normal-cursor-color "#DCDCCC")
 '(cua-overwrite-cursor-color "#F0DFAF")
 '(cua-read-only-cursor-color "#7F9F7F")
 '(custom-safe-themes
   '("2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" "4a201d19d8f7864e930fbb67e5c2029b558d26a658be1313b19b8958fe451b55" "34be6a46f3026dbc0eed3ac8ccf60cba5d2a6ad71aa37ccf21fbd6859f9b4d25" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "b1a691bb67bd8bd85b76998caf2386c9a7b2ac98a116534071364ed6489b695d" "2ff9ac386eac4dffd77a33e93b0c8236bb376c5a5df62e36d4bfa821d56e4e20" "72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe" "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" "3d2e532b010eeb2f5e09c79f0b3a277bfc268ca91a59cdda7ffd056b868a03bc" "3a2e0c5597f6d74d99daa2b5bbbc2a653d02d6b88fcd73d3c84ebf25cde37b3f" "05626f77b0c8c197c7e4a31d9783c4ec6e351d9624aa28bc15e7f6d6a6ebd926" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "e33efadcb0406fc9b0f0fd78e3afbfdb2825d9a9796b8d6671fad6bf38427bd1" "f4cdc8dea941e3c7e92b907e62cdc03e0483a350b738a43c2e118ce6be9880a6" "830596655dc39879096d9b7772768de6042fb5a4293c6b90c98a9b98bce96e4a" default))
 '(fci-rule-color "#4F4F4F")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(frame-brackground-mode 'dark)
 '(highlight-changes-colors '("#DC8CC3" "#bbb0cb"))
 '(highlight-symbol-colors
   '("#681063eb5999" "#54db645164d1" "#6098535f5323" "#5c2959a95fa1" "#4edf55f24ea4" "#64de597a525e" "#530160d26158"))
 '(highlight-symbol-foreground-color "#FFFFEF")
 '(highlight-tail-colors
   '(("#4F4F4F" . 0)
     ("#488249" . 20)
     ("#5dacaf" . 30)
     ("#57a2a4" . 50)
     ("#b6a576" . 60)
     ("#ac7b5a" . 70)
     ("#aa5790" . 85)
     ("#4F4F4F" . 100)))
 '(hl-bg-colors
   '("#b6a576" "#ac7b5a" "#9f5c5c" "#aa5790" "#85749c" "#57a2a4" "#5dacaf" "#488249"))
 '(hl-fg-colors
   '("#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F"))
 '(hl-paren-colors '("#93E0E3" "#F0DFAF" "#8CD0D3" "#bbb0cb" "#7F9F7F"))
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(ispell-dictionary nil)
 '(lsp-ui-doc-border "#FFFFEF")
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#488249" "#95d291" "#57a2a4" "#93E0E3" "#DC8CC3" "#bbb0cb"))
 '(org-fontify-done-headline nil)
 '(org-fontify-todo-headline nil)
 '(package-selected-packages
   '(misterioso-theme zenburn-theme which-key use-package try subatomic256-theme subatomic-theme spacemacs-theme projectile neotree magit gruvbox-theme ergoemacs-mode dracula-theme color-theme-sanityinc-tomorrow auto-complete all-the-icons ace-window))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(pos-tip-background-color "#4F4F4F")
 '(pos-tip-foreground-color "#FFFFEF")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#7F9F7F" "#4F4F4F" 0.2))
 '(term-default-bg-color "#3F3F3F")
 '(term-default-fg-color "#DCDCCC")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#CC9393")
     (40 . "#df52b97da1ae")
     (60 . "#e83dcc9ba8b2")
     (80 . "#F0DFAF")
     (100 . "#cadbca379f51")
     (120 . "#b7fbbf7a973e")
     (140 . "#a52cb4cc8f40")
     (160 . "#9260aa2e8755")
     (180 . "#7F9F7F")
     (200 . "#87dbb4dca003")
     (220 . "#8b6fbfadb0a2")
     (240 . "#8e96ca9fc17d")
     (260 . "#914fd5b0d293")
     (280 . "#93E0E3")
     (300 . "#90c5da6cdd6f")
     (320 . "#8f5dd736da39")
     (340 . "#8df5d402d705")
     (360 . "#8CD0D3")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#3F3F3F" "#4F4F4F" "#9f5c5c" "#CC9393" "#488249" "#7F9F7F" "#b6a576" "#F0DFAF" "#57a2a4" "#8CD0D3" "#aa5790" "#DC8CC3" "#5dacaf" "#93E0E3" "#DCDCCC" "#6F6F6F"))
 '(window-divider-mode nil)
 '(xterm-color-names
   ["#4F4F4F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#fffff6"])
 '(xterm-color-names-bright
   ["#3F3F3F" "#DFAF8F" "#878777" "#6F6F6F" "#DCDCCC" "#bbb0cb" "#FFFFEF" "#FFFFFD"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
