;; Remove ugly startup message
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disabled the menu bar

;; Set up the visible bell
(setq visible-bell t)

(set-face-attribute 'default nil :font "MonoLisa" :height 140)

(load-theme 'tango-dark)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpsa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Open a panel to show used shortcuts log
(use-package command-log-mode)

(use-package ivy
  :diminish                      ;keeps ivy out of the mode line
  :bind (("C-s" . swiper)        ;inline search similar to vim `/`
         :map ivy-minibuffer-map
         ;("TAB" . ivy-alt-done)    
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; 
;; 
;; Evil stuff after this
;; (because I neeeed it)
;;
;;

(require 'evil)
(evil-mode 1)

;; Add easy commenting shortcut
(evil-global-set-key 'normal (kbd "M-;") 'comment-line)
