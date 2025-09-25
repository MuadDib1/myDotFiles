;; Disable native compilation on macOS 
(setq native-comp-deferred-compilation nil)
(setq comp-deferred-compilation nil)
(setq native-comp-jit-compilation nil)
(setq native-comp-async-report-warnings-errors nil)

;; Disable UI clutter
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

;; Visible bell
(setq visible-bell t)

;; Disable backups & lockfiles
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Line/column numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Font
(set-face-attribute 'default nil :family "SauceCodePro NFM" :height 160)
;; height 160 ≈ 16pt, adjust to your liking

;; Package setup
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Theme
(use-package gruvbox-theme
  :ensure t
  :config (load-theme 'gruvbox t))

;; Org mode
(use-package org)

;; Flycheck + js2-mode
(use-package flycheck
  :hook (after-init . global-flycheck-mode))

(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . (lambda ()
                      (setq js2-basic-offset 2))))

;; Optional:  Evil mode
;; (use-package evil
;;   :config (evil-mode 1))

;; End of init

