(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Always install packages from source
;; Removes the need for `:straight t` in every use-package expression
;; NOTE: might remove this later and opt for more control
(setq straight-use-package-by-default t)

;; Initialize package sources
;; (require 'package)

;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                          ("org" . "https://orgmode.org/elpa/")
;;                          ("elpa" . "https://elpa.gnu.org/packages/")))

;; (package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; ;; Initialize use-package on non-Linux platforms
;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

;; (require 'use-package)
;; (setq use-package-always-ensure t)


;; For diagnosing any issues with what packages load
;; (setq use-package-verbose t)

;; Oh hey, this probably doesn't work with straight.
;; Let's keep it around for now just-in-case.

;; (use-package auto-package-update
;;   :custom
;;   (auto-package-update-interval 7)
;;   (auto-package-update-prompt-before-update t)
;;   (auto-package-update-hide-results t)
;;   :config
;;   (auto-package-update-maybe)
;;   (auto-package-update-at-time "09:00"))

;; The default is 800kb. Measured in bytes.
(setq gc-cons-threshold 100000000)

(defun jj/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'jj/display-startup-time)

(use-package no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(defvar jj/default-font-size 140)

;; Remove ugly startup message
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disabled the menu bar

;; make emacs borderless
;; (add-to-list 'default-frame-alist '(undecorated . t))

;; Give emacs window a decent starting size
(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 120))

;; Set up the visible bell
(setq visible-bell t)

;; Add line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Set frame transparency
;; (set-frame-parameter (selected-frame) 'alpha jj/frame-transparency)
;; (add-to-list 'default-frame-alist `(alpha . ,jj/frame-transparency))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist `(fullscreen . maximized))

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

;; Make sure to run this on a new machine to get the fonts:
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 36)))

(load "~/.emacs.d/themes/eighty-four-theme.el")

;; Switch themes with M-x counsel-load-theme
(use-package doom-themes
  :config
  ;; (load-theme 'doom-dracula t)
  ;; (load-theme 'smyx t)
  (load-theme 'eighty-four t)
  (doom-themes-visual-bell-config) (doom-themes-org-config))

(use-package winum
  :straight t
  :init
  (setq winum-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
          (define-key map (kbd "M-1") 'winum-select-window-1)
          (define-key map (kbd "M-2") 'winum-select-window-2)
          (define-key map (kbd "M-3") 'winum-select-window-3)
          (define-key map (kbd "M-4") 'winum-select-window-4)
          (define-key map (kbd "M-5") 'winum-select-window-5)
          (define-key map (kbd "M-6") 'winum-select-window-6)
          (define-key map (kbd "M-7") 'winum-select-window-7)
          (define-key map (kbd "M-8") 'winum-select-window-8)
          (define-key map (kbd "M-9") 'winum-select-window-9)
          map))
  :config
  (winum-mode))

;; dim inactive windows
(use-package dimmer
  :config
  (setq dimmer-fraction 0.5)
  (dimmer-configure-helm)
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-configure-which-key)
  (dimmer-mode)
  )

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2)
  )

;; Set up Space leader key
(use-package general
  :config
  ;; (general-evil-setup t)
  (general-create-definer jj/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(defun jj/alternate-buffer ()
  "Switch back and forth between current and last buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(jj/leader-keys
  "ESC"  '(keyboard-escape-quit :which-key "quit")
  "TAB" '(jj/alternate-buffer :which-key "previous buffer")
  "SPC"  '(counsel-M-x :which-key "M-x")
  "q"    '(save-buffers-kill-terminal :which-key "quit emacs")
  "Q"    '(kill-emacs :which-key "quit emacs")
  )

(jj/leader-keys
  "b"  '(:ignore t :which-key "buffer")
  "bb" '(buffer-menu :which-key "buffer menu")
  "bd" '(kill-this-buffer :which-key "kill active buffer")
  "bm" '((lambda () (interactive) (switch-to-buffer "*Messages*")) :which-key "messages")
  )

;; Used copilot to suggest which additional commands I might want,
;; but primary usage will be `SPC c c` I think.
(jj/leader-keys
  "c"  '(:ignore t :which-key "copilot")
  "cc" '(copilot-mode :which-key "copilot mode")

  "ca" '(copilot-activate :which-key "activate")
  "cd" '(copilot-dictate :which-key "dictate")
  "cs" '(copilot-say :which-key "say")
  "ct" '(copilot-try-expand :which-key "try expand")
  "cw" '(copilot-words :which-key "words")
  )

(jj/leader-keys
  "f"  '(:ignore t :which-key "file")
  "fe"  '(:ignore t :which-key "editor")
  "fed" '((lambda () (interactive) (find-file "~/.emacs.d/README.org")) :which-key "emacs config")
  "ff" '(counsel-find-file :which-key "find file")
  "fj" '(dired-jump :which-key "jump to file")
  "fr" '(rename-file :which-key "rename file")
  "fs" '(save-buffer :which-key "save active buffer")
  )

(jj/leader-keys
  "g" '(magit-status :which-key "magit"))

(jj/leader-keys
  "o"  '(:ignore t :which-key "org-mode")
  "oa" '(org-agenda :which-key "agenda")
  "oc" '(org-capture :which-key "capture")
  "od" '(org-todo :which-key "toggle todo/done")
  "on" '((lambda () (interactive) (org-capture nil "n")) :which-key "add now")
  "ot" '((lambda () (interactive) (org-capture nil "t")) :which-key "add todo")
  )

(jj/leader-keys
  "p" '(projectile-command-map :which-key "projectile"))

(jj/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme")
  "ts" '(hydra-text-scale/body :which-key "scale text")
  )

(jj/leader-keys
  "w"  '(:ignore t :which-key "window")
  "w TAB" '(evil-window-prev :which-key "previous window")
  "w-" '(split-window-below :which-key "horizontal split")
  "w/" '(split-window-right :which-key "vertical split")
  "w=" '(balance-windows :which-key "balance windows")
  "wH" '(evil-window-move-far-left :which-key "move to left")
  "wJ" '(evil-window-move-very-bottom :which-key "move to bottom")
  "wK" '(evil-window-move-very-top :which-key "move to top")
  "wL" '(evil-window-move-far-right :which-key "move to right")
  "wd" '(delete-window :which-key "close window")
  "wh" '(evil-window-left :which-key "select left")
  "wj" '(evil-window-down :which-key "select down")
  "wk" '(evil-window-up :which-key "select up")
  "wl" '(evil-window-right :which-key "select right")
  ;; "wt" '(toggle-window-dedicated :which-key "toggle window dedication")
  "wm" '(delete-other-windows :which-key "maximize")
  "wo" '(other-window :which-key "other window")
  )

;; crashes if I don't have these?
;; (setq evil-want-keybinding nil)
;; (setq evil-want-C-u-scroll t)
;; (require 'evil)

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  ;; (setq evil-want-C-d-scroll t)
  (setq evil-undo-system 'undo-redo)
  ;; (setq evil-want-C-i-jump nil)
  ;; :hook (evil-mode . jj/evil-hook)
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

;; Add Vim-style redo shortcut: Ctrl-r
;; (evil-set-undo-system 'undo-tree)
;; (require 'undo-tree)
;; (setq evil-undo-system 'undo-tree)
;; (global-undo-tree-mode t)
;; (add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package ivy
  :diminish                      ;keeps ivy out of the mode line
  :bind (("C-s" . swiper)        ;inline search similar to vim `/`
         :map ivy-minibuffer-map
         ;; ("TAB" . ivy-alt-done)
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
  :after ivy
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
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(with-eval-after-load 'org
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("ex" . "src elixir"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
  )

(use-package org :straight (:type built-in))

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

(defun jj/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; Wraps long lines
(use-package visual-fill-column
  :hook (org-mode . jj/org-mode-visual-fill))

(setq
 org-capture-templates
 '(
   ("n" "What I'm working on now" entry (file+olp+datetree "~/.emacs.d/org/now.org")
    "* %T %?\n%l\n%i" :tree-type week)
   ("t" "Create a TODO for today" entry (file+olp+datetree "~/.emacs.d/org/now.org")
    "* TODO %?" :tree-type week)))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t))))

(setq org-confirm-babel-evaluate nil)

;; Automatically tangle our emacs.org config file when we save it
(defun jj/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/README.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'jj/org-babel-tangle-config)))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines)
  :bind ("M-;" . evilnc-comment-or-uncomment-lines)
  )

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)
;; you can utilize :map :hook and :config to customize copilot

(add-hook 'prog-mode-hook 'copilot-mode)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(defun jj/elixir-format-buffer ()
  (interactive)
  (lsp-format-buffer))

(use-package elixir-mode
  :mode ("\\.ex\\'" "\\.heex\\'")
  :init
  ;; (add-to-list 'auto-mode-alist '("\\.heex\\'" . elixir-mode))
  :hook (elixir-mode . (lambda () (add-hook 'before-save-hook
                                            'jj/elixir-format-buffer
                                            nil
                                            t)))
  :config
  (setq lsp-elixir-suggest-specs nil)
  )

(defun jj/lsp-mode-setup-completion ()
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless)))

(add-hook 'lsp-mode-hook 'jj/lsp-mode-setup-completion)

(defface elixir-bracket-face
  '((t (:foreground "#fcfcfc")))
  "Face for < and > syntax.")

;; Conditionally add bracket colors when using eighty-four theme
(defun elixir-add-bracket-face ()
  (when (member 'eighty-four custom-enabled-themes)
    (font-lock-add-keywords
     nil ;; current buffer
     '(
       ("\\(<%=\\)[^%]*\\(%>\\)" (1 'elixir-bracket-face) (2 'elixir-bracket-face))
       ("\\(<%\\)[^%]*\\(%>\\)" (1 'elixir-bracket-face) (2 'elixir-bracket-face))
       ("\\(<\\.\\)[^>]*\\(/?>\\)" (1 'elixir-bracket-face) (2 'elixir-bracket-face))
       ("\\(</\\.\\)[^>]*\\(>\\)" (1 'elixir-bracket-face) (2 'elixir-bracket-face))
       ("\\(</\\)[^>]*\\(>\\)" (1 'elixir-bracket-face) (2 'elixir-bracket-face))
       ("\\(<\\)[^>]*\\(/?>\\)" (1 'elixir-bracket-face) (2 'elixir-bracket-face))
       ("\\(</\\)[^>]*\\(>\\)" (1 'elixir-bracket-face) (2 'elixir-bracket-face))
       )
     t ;; append
     )))

(add-hook 'elixir-mode-hook 'elixir-add-bracket-face)
(add-hook 'load-theme-after-hook 'elixir-add-bracket-face)

(define-derived-mode heex-mode elixir-mode "Heex"
  "Major mode for Elixir's Heex templates."
  (setq font-lock-defaults '((font-lock-keywords) nil nil))

  (font-lock-add-keywords
   nil ;; current buffer
   '(
     ("\\(<[^>]+>\\)\\([^<]*\\)\\(<\\/[^>]+>\\)" (2 'elixir-inner-text-face))
     )
   t ;; append
   ))

;; Associate .heex files with heex-mode
(add-to-list 'auto-mode-alist '("\\.heex\\'" . heex-mode))

;; init.el
(defface elixir-equal-face nil "")
(defface elixir-keyword-face nil "")
(defface elixir-attr-face nil "")
(defface elixir-number-face nil "")
(defface elixir-at-variable-face nil "")

(defun heex-add-custom-faces ()
  (when (member 'eighty-four custom-enabled-themes)
    (font-lock-add-keywords
     nil ;; current buffer
     '(
       ("\\(!=\\|=\\)" (1 'elixir-equal-face))
       ("\\(<%[^>]*\\)\\(if\\|do\\|else\\|end\\)[^>]*\\(%>\\)" (2 'elixir-keyword-face))
       ;; ("\\(<[^>]+\\)\\(:\\w+=\\)" (2 'elixir-attr-face))
       ;; ("\\b\\w+\\s*=\\s*\\{?@?:?\\w+\\}?" (0 'elixir-attr-face))
       ("\\b\\([0-9]+\\)\\b" (1 'elixir-number-face))
       ;; ("\\(<%=\\|<%\\)\\([^>]*@\\w+[^>]\\)\\(%>\\)" (2 'elixir-at-variable-face))
       )
     t ;; append
     )))

(add-hook 'heex-mode-hook 'heex-add-custom-faces)
(add-hook 'load-theme-after-hook 'heex-add-custom-faces)

(add-hook 'elixir-mode-hook
          (lambda ()
            (setq font-lock-defaults '((elixir-font-lock-keywords) nil nil))))

(defface elixir-inner-text-face
  '((t (:foreground "#fcfcfc")))
  "Face for the text inside < and > syntax.")

(defun elixir-add-inner-text-face ()
  (when (member 'eighty-four custom-enabled-themes)
    (font-lock-add-keywords
     nil ;; current buffer
     '(
       ("\\(<[^>]+>\\)\\(\\w+\\)\\(<\\/[^>]+>\\)" (2 'elixir-inner-text-face))
       )
     t ;; prepend
     )))

(add-hook 'elixir-mode-hook 'elixir-add-inner-text-face)
(add-hook 'load-theme-after-hook 'elixir-add-inner-text-face)

;; (defun elixir-add-capital-letter-face ()
;;   (when (member 'eighty-four custom-enabled-themes)
;;     (font-lock-add-keywords
;;      nil ;; current buffer
;;      '(
;;        ("\\(<[^>]+>\\)\\([A-Z][^<]*\\)\\(<\\/[^>]+>\\)" (2 'elixir-inner-text-face))
;;        )
;;      t ;; append
;;      )))

;; (add-hook 'elixir-mode-hook 'elixir-add-capital-letter-face)
;; (add-hook 'load-theme-after-hook 'elixir-add-capital-letter-face)

(use-package typescript-mode
  :mode "\\.ts\\'"
  ;; :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; This is kind of ugly and I don't know if I really need it.
;; Let's hide it for now

;; (defun jj/lsp-mode-setup ()
;;   (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;   (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (
         (elixir-mode . lsp-deferred)
         (elixir-ts-mode . lsp)
         (heex-ts-mode . lsp)
         (js-mode . lsp-deferred)
         (lsp-mode . jj/lsp-mode-setup)
         (typescript-mode . lsp-deferred)
         )
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  ;; Extra hack to work with my custom heex mode
  (add-to-list 'lsp-language-id-configuration '(heex-mode . "elixir"))
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (setq lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

;; Search for a symbol within your project
(use-package lsp-ivy
  :after lsp)

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
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Github Issues/PRs/Etc in Magit
;; NOTE: Currently has an issue with sqlite, so disabling for now
;; (use-package forge
;;   :after magit)

(use-package projectile
  :straight t
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
  :straight t
  ;; :after projectile
  :config (counsel-projectile-mode))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "zsh")
  ;; (setq explicit-zsh-args '())
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  )

;; Disabling this because it mutes my colors for some reason
;; (use-package eterm-256color
;;   :hook (term-mode . eterm-256color-mode))

;; set term background to match colors in eighty-four theme
;; (defun set-term-background ()
;;   (when (eq major-mode 'term-mode)
;;     (face-remap-add-relative 'default :foreground "#173347")))

;; (add-hook 'buffer-list-update-hook 'set-term-background)

;; set term foreground to match colors in eighty-four theme
(defun set-vterm-foreground ()
  (when (eq major-mode 'vterm-mode)
    (face-remap-add-relative 'default :foreground "#f1f1f1")))

(add-hook 'buffer-list-update-hook 'set-vterm-foreground)

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-al --group-directories-first")

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer)
  )

;; Use single dired buffer
(use-package dired-single
  :commands (dired dired-jump)
  :after dired)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(require 'dired-aux)

;; Toggle hiding dotfiles
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; (use-package dired-open
;;   :after dired
;;   :config
;;   (setq dired-open-extensions `(
;;                                 ;; {ext} . {app}
;;                                 ;; ("png" . "feh")
;;                                 ;; ("mkv" . "mpv")
;;                                 )))

;; Prevent Backtrace from taking over the buffer on an error
(setq debug-on-error nil)

(defun jj/org-indent-source-blocks ()
  "Indent all source blocks in the current org-mode buffer."
  (require 'org-indent)

  (when (eq major-mode 'org-mode)
    (org-element-map (org-element-parse-buffer) 'src-block
      (lambda (src-block)
        (let ((begin (org-element-property :begin src-block))
              (end (org-element-property :end src-block)))
          (save-excursion
            (goto-char begin)
            (org-indent-block)))))))

(add-hook 'before-save-hook #'jj/org-indent-source-blocks)

;; Make gc pauses faster by decreasing the threshold
;; (setq gc-cons-threshold (* 2 1000 1000))
