;; NOTES:
;; 
;; Ctrl-h v for describe-variable
;; Cmd-shift ; for evaluating a hook
;; 
;; :init happens before packages are loaded
;; :config happens after packages are loaded
;;
;; M-x check-parens to find parenthesis issues
;;
;; Ways to close buffers
;; - C-g
;; - Esc
;; - q
;; 
;; QUESTIONS:
;; I'm not sure I totally get what "hooks" are in Emacs
;;
;; I need to try out magit spinoff:
;;
;; It sounds like if I'm working on main and make some commits
;; and then realize I should be on a new feature branch (oops)
;; I can just make a spinoff branch, which will move all the newer
;; commits from main that aren't on github to the new branch I create.
;; 
;; If so, that would be a huge time save! I do that all the time.

(defvar jj/default-font-size 140)

;; Remove ugly startup message
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disabled the menu bar

;; Set up the visible bell
(setq visible-bell t)

(set-face-attribute 'default nil :font "MonoLisa" :height jj/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "MonoLisa")
(set-face-attribute 'variable-pitch nil :font "SF Pro")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:inherit (shadow default) :height 1 :italic nil))))
 '(line-number-current-line ((t (:inherit line-number :italic nil)))))



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

;; Add line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

;; Make sure to run this on a new machine to get the fonts:
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

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
  ;; :config
  ;; (setq which-key-idle-delay 0)
  )

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


;; Set up Space leader key
(use-package general
  :config
  ;; (general-evil-setup t)
  (general-create-definer jj/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (jj/leader-keys
    "b"   '(:ignore t :which-key "buffer")
    "bb"  '(buffer-menu :which-key "buffer menu")
    "bm"  '((lambda () (interactive) (switch-to-buffer "*Messages*")) :which-key "messages")
    "bk"  '(kill-this-buffer :which-key "kill active buffer")
    "f"   '(:ignore t :which-key "file")
    "fe" '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :which-key "emacs config")
    "fs"  '(save-buffer :which-key "save active buffer")
    "t"   '(:ignore t :which-key "toggles")
    "tt"  '(counsel-load-theme :which-key "choose theme")
    "w"   '(:ignore t :which-key "window")
    "w/"  '(split-window-right :which-key "split vertical")
    "wc"  '(delete-window :which-key "close window")
    "wh"  '(evil-window-left :which-key "select left")
    "wl"  '(evil-window-right :which-key "select right")
    ))

;; crashes if I don't have these?
(setq evil-want-keybinding nil)
(require 'evil)

(use-package evil
  :init
  (setq evil-want-integration t)
  ;; (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-i-jump nil)
  :hook (evil-mode . jj/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(jj/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Code")
    (setq projectile-project-search-path '("~/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

;; redefine ESC key in projectile-command-map to just close the map
;; (define-key projectile-command-map (kbd "<escape>") 'keyboard-escape-quit)

(jj/leader-keys
  "p" '(projectile-command-map :which-key "projectile"))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(jj/leader-keys
  "g" '(magit-status :which-key "magit"))

(use-package forge)

(defun jj/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  ;; (auto-fill-mode 0)
  ;; (setq evil-auto-indent nil)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . jj/org-mode-setup)
  :config
  (setq org-ellipsis " ▼"
	org-hide-emphasis-markers nil))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;;  Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(require 'org-indent)

(defun jj/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . jj/org-mode-visual-fill))

;; (defun jj/evil-hook ()
;;   (dolist (mode '(custom-mode
;; 		  eshell-mode
;; 		  git-rebase-mode
;; 		  erc-mode
;; 		  circe-server-mode
;; 		  circe-chat-mode
;; 		  circe-query-mode
;; 		  sauron-mode
;; 		  term-mode))
;;    (add-to-list 'evil-emacs-state-modes mode)))
		  
;; (setq evil-want-keybinding nil)

;; Add easy commenting shortcut
(evil-global-set-key 'normal (kbd "M-;") 'comment-line)

;; Add Vim-style redo shortcut: Ctrl-r
(evil-set-undo-system 'undo-tree)
(require 'undo-tree)
(setq evil-undo-system 'undo-tree)
(global-undo-tree-mode t)
(add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)

;; Prevent Backtrace from taking over the buffer on an error
(setq debug-on-error nil)

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
   '(visual-fill-column visual-fill org-bullets magit counsel-projectile projectile hydra evil-collection general helpful ivy-rich which-key rainbow-delimiters doom-themes all-the-icons doom-modeline-now-playing doom-modeline counsel use-package undo-tree ivy evil command-log-mode))
 '(safe-local-variable-values '((projectile-project-run-cmd . "mix phx.server"))))

