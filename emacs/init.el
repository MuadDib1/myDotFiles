;; (setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar

(setq make-backup-files nil)  ; disable emacs automatic backup~ file
(setq create-lockfiles nil)   ; disable lock files

;; Set up the visible bell
(setq visible-bell t)

(set-face-attribute 'default nil :font "SauceCodePro NFM-11")
(load-theme 'gruvbox t)

;; Set up Melpa
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

