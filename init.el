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


;; Add line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

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

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
              term-mode-hook
              shell-mode-hook
              treemacs-mode-hook
              eshell-mode-hook))
(add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "MonoLisa" :height jj/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "MonoLisa")

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "SF Pro Display")


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:inherit (shadow default) :height 1 :italic nil))))
 '(line-number-current-line ((t (:inherit line-number :italic nil)))))

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
  ;; (load-theme 'smyx t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  ;; :config
  ;; (setq which-key-idle-delay 0)
  )

;; Set up Space leader key
(use-package general
  :config
  ;; (general-evil-setup t)
  (general-create-definer jj/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(jj/leader-keys
  "b"  '(:ignore t :which-key "buffer")
  "bb" '(buffer-menu :which-key "buffer menu")
  "bc" '(kill-this-buffer :which-key "kill active buffer")
  "bm" '((lambda () (interactive) (switch-to-buffer "*Messages*")) :which-key "messages"))

(jj/leader-keys
  "f"  '(:ignore t :which-key "file")
  "fe" '((lambda () (interactive) (find-file "~/.emacs.d/README.org")) :which-key "emacs config")
  "fr" '(rename-file :which-key "rename file")
  "fs" '(save-buffer :which-key "save active buffer")
)

(jj/leader-keys
  "o"  '(:ignore t :which-key "org-mode")
  "oa" '(org-agenda :which-key "agenda")
  "oc" '(org-capture :which-key "capture")
  "on" '((lambda () (interactive) (org-capture nil "n")) :which-key "add now")
  "ot" '((lambda () (interactive) (org-capture nil "t")) :which-key "add todo"))

(jj/leader-keys
  "p" '(projectile-command-map :which-key "projectile"))

(jj/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme")
  "ts" '(hydra-text-scale/body :which-key "scale text")
)

(jj/leader-keys
  "w"  '(:ignore t :which-key "window")
  "w/" '(split-window-right :which-key "split vertical")
  "wc" '(delete-window :which-key "close window")
  "wh" '(evil-window-left :which-key "select left")
  "wj" '(evil-window-down :which-key "select down")
  "wk" '(evil-window-up :which-key "select up")
  "wl" '(evil-window-right :which-key "select right")
  )

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

;; Add easy commenting shortcut
(evil-global-set-key 'normal (kbd "M-;") 'comment-line)

;; Add Vim-style redo shortcut: Ctrl-r
(evil-set-undo-system 'undo-tree)
(require 'undo-tree)
(setq evil-undo-system 'undo-tree)
(global-undo-tree-mode t)
(add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("ex" . "src elixir"))
(add-to-list 'org-structure-template-alist '("js" . "src javascript"))

(defun jj/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  ;; (auto-fill-mode 0)
  ;; (setq evil-auto-indent nil)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . jj/org-mode-setup)
  :config
  (setq org-ellipsis " ▼")
  ;; (setq org-hide-emphasis-markers nil)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files
        '("~/.emacs.d/org/now.org"))
  )

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

(setq
 org-capture-templates
 '(
   ("n" "What I'm working on now" entry (file+olp+datetree "~/.emacs.d/org/now.org")
    "* %T %?\n%l\n%i" :tree-type week)
   ("t" "Create a TODO for today" entry (file+olp+datetree "~/.emacs.d/org/now.org")
    "* TODO %?" :tree-type week)))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(setq org-confirm-babel-evaluate nil)

;; Automatically tangle our emacs.org config file when we save it
(defun jj/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/README.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'jj/org-babel-tangle-config)))

(defun jj/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . jj/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (setq lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

;; Search for a symbol within your project
(use-package lsp-ivy)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; Makes the autocomplete menu look a little nicer
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(jj/leader-keys
  "g" '(magit-status :which-key "magit"))

;; Github Issues/PRs/Etc in Magit
;; NOTE: Currently has an issue with sqlite, so disabling for now
;; (use-package forge)

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

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Prevent Backtrace from taking over the buffer on an error
(setq debug-on-error nil)
