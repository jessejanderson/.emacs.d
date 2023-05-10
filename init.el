;; Remove ugly startup message
(setq inhibit-startup-message t)

(scroll-bar-mode -1)      ; Disable visible scrollbar
(tool-bar-mode -1)        ; Disable the toolbar
(tooltip-mode -1)         ; Disable tooltips
(set-fringe-mode 10)      ; Give some breathing room

(menu-bar-mode -1)        ; Disabled the menu bar

;; Set up the visible bell
(setq visible-bell t)

(set-face-attribute 'default nil :font "MonoLisa" :height 120)

(require 'evil)
(evil-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
