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
;; (use-package command-log-mode)

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

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 36)))

;; Switch themes with M-x counsel-load-theme
(use-package doom-themes
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Improved functions search
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history)))

;; Improved helpers
(use-package helpful
  ;; :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Add line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

;; Add Vim-style redo shortcut: Ctrl-r
(evil-set-undo-system 'undo-tree)
(require 'undo-tree)
(setq evil-undo-system 'undo-tree)
(global-undo-tree-mode t)
(add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)


;;  
;;  
;; Auto-set stuff below
;;  
;;  

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(helpful ivy-rich which-key rainbow-delimiters doom-themes all-the-icons doom-modeline-now-playing doom-modeline counsel use-package undo-tree ivy evil command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

