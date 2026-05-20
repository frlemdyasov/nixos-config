(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("95ee4d370f4b66ff2287d8075f8fe5f58c4a9b9c1e65d663b15174f1a8c57717"
     default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t)  ; Don't show the splash screen

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                       ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                       ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
(package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package haskell-mode
  :mode "\\.hs\\'")

;; modus-theme color set
(use-package modus-themes
  :ensure t
  :config
  (setq	modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
	modus-themes-common-palette-overrides
	'((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
	  (bg-main "#f2f2f2")
	  (bg-dim "#deddda")
	  (bg-mode-line-active "#c0bfbc")
	  (bg-mode-line-inactive "#deddda")))
  (modus-themes-load-theme 'modus-operandi-tinted))

;; Dirvish file manager, an upgrade to dired
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  (setq dired-mouse-drag-files t)
  (setq mouse-drag-and-drop-region-cross-program t)
  :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-dwim)
   :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
   (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
   ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
   ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("f"   . dirvish-file-info-menu)    ; [f]ile info
   ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
   ("s"   . dirvish-quicksort)         ; [s]ort flie list
   ("r"   . dirvish-history-jump)      ; [r]ecent visited
   ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
   ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
   ("*"   . dirvish-mark-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)))

(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq version-control t)
(setq create-lockfiles nil)

;; Make dired copy/rename files to the other dired window
(setq dired-dwim-target t)

;; Preview a file from dired
(use-package dired-preview)

;; Smooth Scrolling Package
(use-package ultra-scroll
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))

;; Latex stuff
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(use-package auctex
  :defer t
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
	reftex-plug-ino-AUCTeX t)
  :hook (LaTeX-mode . reftex-mode))

(use-package bibtex
  :mode (("\\.bib\\'" . bibtex-mode)))

;; Install Dashboard Startup Screen
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)

  ;; Replace scratch buffer wth dashboard
  ;; (to get dashboard working with emacsclient)
  ;;(setq initial-buffer-choice 'dashboard-open)
  
  (setq dashboard-items '((bookmarks . 5)))
  (setq dashboard-banner-logo-title "\"Welcome to Emacs\" -Rico")
  (setq dashboard-startup-banner "/home/fedor/khaled-rico.png")
  (setq dashboard-center-content t)
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    ;;dashboard-insert-newline
                                    ;;dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline)))



;; Set startup window size
(when window-system
      (set-frame-position (selected-frame) 10 0)
      (set-frame-size (selected-frame) 90 53))

;; Mode Line Theme
;;(use-package powerline
;;  :init
;;  (powerline-default-theme))
;;

;; Enable LPS-Mode in emacs
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (go-mode . lsp)
	 (c-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(add-hook 'go-mode-hook #'lsp-deferred)

(use-package lsp-ui)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; syntax checker
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; debugger
(use-package dap-mode)

;; C language server
(use-package ccls)
