;; Based on "1984" theme for VS Code
;; https://github.com/juanmnl/vs-1984

;; Requires a bunch of hacks in my init.el file to work

;; Not intended for distribution.
;; (it probably won't work for you anyway)


(deftheme eighty-four "A nice dark theme.")


(let (
(unset "#ff0000")
(unsetbg "#330000")
(bg "#0d0f31")
(bgDark "#070825")
(bgLight "#1b1c27")
(bgLighter "#242533")
(bgLightest "#2c313a")
(textBlue "#46bdff")
(textGray "#3a3c5e")
(textPurple "#df81fc")
(textPurpleLight "#96a1ff")
(textPink "#ff16b0")
(cursor "#b3f361")
(textWhite "#fcfcfc")
(selection "#454761")
(comment "#525863")
(lineNumbers "#3b4d66")
(lineNumbersActive "#d7dae0")
(delA "#fcee54")
(delB "#46bdff")
(delC "#b3f361")
(delD "#ff16b0")
(delE "#d5d8da")

(yellow "#ffff00")
(green "#00ff00")
(greenbg "#003300")
)

(custom-theme-set-faces
'eighty-four


;; BASIC FACES
`(cursor ((t (:background ,cursor ))))
`(default ((t (:background ,bg :foreground ,textBlue))))
`(fringe ((t (:background ,bgDark :foreground ,comment ))))
;; highlight line mode
;;`(hl-line ((t (:background ,unset :foreground ,unsetbg ))))
;; `(minibuffer-prompt ((t (:background ,greenbg :foreground ,green ))))
`(minibuffer-prompt ((t (:background ,bgDark :foreground ,textPurpleLight ))))
`(mode-line-inactive ((t (:background ,bgDark :foreground ,textGray ))))
`(mode-line ((t (:background ,bgDark :foreground ,textBlue ))))
`(region ((t (:background ,selection ))))
;; `(secondary-selection ((t (:background ,greenbg :foreground ,green ))))
`(vertical-border ((t (:background ,bg :foreground ,bgDark ))))

;; FONT LOCK FACES
`(font-lock-builtin-face ((t (:foreground ,textPurpleLight ))))
`(font-lock-comment-delimiter-face ((t (:foreground ,comment ))))
`(font-lock-comment-face ((t (:foreground ,comment ))))
`(font-lock-constant-face ((t (:foreground ,textPurple ))))
`(font-lock-doc-face ((t (:foreground ,yellow ))))
`(font-lock-doc-markup-face ((t (:foreground ,yellow ))))
`(font-lock-doc-string-face ((t (:foreground ,yellow ))))
`(font-lock-faces ((t (:foreground ,yellow ))))
`(font-lock-fic-face ((t (:foreground ,yellow ))))
`(font-lock-function-name-face ((t (:foreground ,textWhite ))))
`(font-lock-keyword-face ((t (:foreground ,textBlue ))))
`(font-lock-negation-char-face ((t (:foreground ,yellow ))))
`(font-lock-preprocessor-char-face ((t (:foreground ,yellow ))))
`(font-lock-preprocessor-face ((t (:foreground ,textPurpleLight ))))
`(font-lock-reference-face ((t (:foreground ,yellow ))))
`(font-lock-regexp-grouping-backslash ((t (:foreground ,yellow ))))
`(font-lock-regexp-grouping-construct ((t (:foreground ,yellow ))))
`(font-lock-string-face ((t (:foreground ,textPurple ))))
`(font-lock-syntax-face ((t (:background ,yellow :foreground ,unset ))))
;; Why does this highlight only the first words of sentences?
`(font-lock-type-face ((t (:foreground ,textWhite ))))
`(font-lock-variable-name-face ((t (:foreground ,textPurpleLight ))))
`(font-lock-warning-face ((t (:foreground ,unset ))))


;; rainbow-delimiters-mode
`(rainbow-delimiters-depth-1-face ((t (:foreground ,delA ))))
`(rainbow-delimiters-depth-2-face ((t (:foreground ,delB ))))
`(rainbow-delimiters-depth-3-face ((t (:foreground ,delC ))))
`(rainbow-delimiters-depth-4-face ((t (:foreground ,delD ))))
`(rainbow-delimiters-depth-5-face ((t (:foreground ,delE ))))
`(rainbow-delimiters-depth-6-face ((t (:foreground ,delA ))))
`(rainbow-delimiters-depth-7-face ((t (:foreground ,delB ))))
`(rainbow-delimiters-depth-8-face ((t (:foreground ,delC ))))
`(rainbow-delimiters-depth-9-face ((t (:foreground ,delD ))))
`(rainbow-delimiters-depth-10-face ((t (:foreground ,delE ))))
`(rainbow-delimiters-depth-11-face ((t (:foreground ,delA ))))
`(rainbow-delimiters-depth-12-face ((t (:foreground ,delB ))))


;; headerline
`(header-line ((t (:background ,bgDark :foreground ,textWhite))))
`(header-line-highlight ((t (:background ,bgDark :foreground ,textWhite))))
`(lsp-headerline-breadcrumb-path-error-face ((t (:background ,bgDark :foreground ,textWhite))))
`(lsp-headerline-breadcrumb-symbols-face ((t (:background ,bgDark :foreground ,textWhite))))
`(lsp-headerline-breadcrumb-path-face ((t (:background ,bgDark :foreground ,textWhite))))
`(lsp-headerline-breadcrumb-unknown-project-prefix-face ((t (:background ,bgDark :foreground ,textWhite))))
`(lsp-headerline-breadcrumb-separator-face ((t (:background ,bgDark :foreground ,textWhite))))


;; display-line-number-mode
`(line-number ((t (:foreground ,lineNumbers ))))
`(line-number-current-line ((t (:foreground ,lineNumbersActive ))))


;; THIRD PARTY PACKAGE FACES


;; doom-modeline-mode
`(doom-modeline-bar ((t (:background ,unsetbg :foreground ,unset ))))
`(doom-modeline-inactive-bar ((t (:background ,unsetbg :foreground ,unset ))))
`(doom-themes-visual-bell ((t (:background ,textPink :foreground ,textWhite ))))


;; web-mode
`(web-mode-html-tag-face ((t (:foreground ,yellow ))))  ; color for tags
`(web-mode-html-tag-bracket-face ((t (:foreground ,yellow ))))  ; color for brackets
`(web-mode-html-attr-name-face ((t (:foreground ,yellow ))))  ; color for attributes
`(web-mode-html-attr-equal-face ((t (:foreground ,yellow ))))  ; color for equals sign
`(web-mode-html-attr-value-face ((t (:foreground ,yellow ))))  ; color for attribute values


;; elixir-mode
;; `(elixir-atom-face ((t (:foreground ,textPurpleLight ))))  ; color for tags
;; `(elixir-attribute-face ((t (:foreground ,textPurpleLight ))))  ; color for tags
;; `(elixir-number-face ((t (:foreground ,textPink ))))  ; color for tags

;; Deprecated?
;; `(elixir-add-bracket-face ((t (:background ,unsetbg :foreground ,unset ))))  ; color for tags
;; `(elixir-bracket-face ((t (:background ,unsetbg :foreground ,unset ))))  ; color for tags

;; `(elixir-mode-html-tag-face ((t (:background ,unsetbg :foreground ,unset ))))  ; color for tags
;; `(elixir-ignored-var-face ((t (:background ,unsetbg :foreground ,unset ))))  ; color for tags
;; `(elixir-operator-face ((t (:background ,unsetbg :foreground ,unset ))))  ; color for tags

;; heex-mode
`(elixir-equal-face ((t (:foreground ,textPink))))
`(elixir-keyword-face ((t (:foreground ,textPink))))
`(elixir-attr-face ((t (:foreground ,textPurpleLight))))
`(elixir-number-face ((t (:foreground ,textPink))))
`(elixir-at-variable-face ((t (:foreground ,textPurpleLight))))

;; company-mode
`(company-tooltip ((t (:background ,bgDark :foreground ,textPurpleLight ))))


;; org-mode
`(org-block ((t (:background ,bgLight :extend t))))
`(org-block-begin-line ((t (:height 0.8 :background ,bgLighter :foreground ,comment :extend t))))
`(org-block-end-line ((t (:height 0.8 :background ,bgLighter :foreground ,comment :extend t))))


;; term + vterm (cobalt2-based)
`(term-color-blue ((t (:background "#1477da", :foreground "#1477da"))))
`(term-color-yellow ((t (:background "#ffe600", :foreground "#ffe600"))))
`(term-color-red ((t (:background "#ff2600", :foreground "#ff2600"))))
`(term-color-green ((t (:background "#3cdf2b", :foreground "#3cdf2b"))))
`(term-color-cyan ((t (:background "#00c5c7", :foreground "#00c5c7"))))
`(term-color-magenta ((t (:background "#ff2b6f", :foreground "#ff2b6f"))))
`(term-color-white ((t (:background "#c7c7c7", :foreground "#c7c7c7"))))
`(term-color-black ((t (:background "#000000", :foreground "#000000"))))

`(vterm-color-default ((t (:background "#f1f1f1", :foreground "#f1f1f1"))))

;; Added these via copilot - do they work?
`(vterm-color-blue ((t (:background "#1477da", :foreground "#1477da"))))
`(vterm-color-yellow ((t (:background "#ffe600", :foreground "#ffe600"))))
`(vterm-color-red ((t (:background "#ff2600", :foreground "#ff2600"))))
`(vterm-color-green ((t (:background "#3cdf2b", :foreground "#3cdf2b"))))
`(vterm-color-cyan ((t (:background "#00c5c7", :foreground "#00c5c7"))))
`(vterm-color-magenta ((t (:background "#ff2b6f", :foreground "#ff2b6f"))))
`(vterm-color-white ((t (:background "#c7c7c7", :foreground "#c7c7c7"))))
`(vterm-color-black ((t (:background "#000000", :foreground "#000000"))))
`(vterm-color-default-bg ((t (:background "#f1f1f1", :foreground "#f1f1f1"))))
`(vterm-color-blue-bg ((t (:background "#1477da", :foreground "#1477da"))))
`(vterm-color-yellow-bg ((t (:background "#ffe600", :foreground "#ffe600"))))
`(vterm-color-red-bg ((t (:background "#ff2600", :foreground "#ff2600"))))
`(vterm-color-green-bg ((t (:background "#3cdf2b", :foreground "#3cdf2b"))))
`(vterm-color-cyan-bg ((t (:background "#00c5c7", :foreground "#00c5c7"))))
`(vterm-color-magenta-bg ((t (:background "#ff2b6f", :foreground "#ff2b6f"))))
`(vterm-color-white-bg ((t (:background "#c7c7c7", :foreground "#c7c7c7"))))
`(vterm-color-black-bg ((t (:background "#000000", :foreground "#000000"))))


;; end block
))


;; Hack for angle bracket coloring
;; (defface elixir-embedded-code-face
;;   '((t (:foreground "#0cfcfc")))
;;   "Face for <%= %> syntax.")

;; (defun elixir-add-embedded-code-face ()
;;   (font-lock-add-keywords
;;    nil ;; current buffer
;;    '(("<%=[^%]*%>" . 'elixir-embedded-code-face))
;;    t ;; append
;;    ))

;; (add-hook 'elixir-mode-hook 'elixir-add-embedded-code-face)

(custom-theme-set-variables
  'eighty-four
  '(linum-format " %3i "))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


;;;###autoload
(defun make-apps-theme()
  "Apply the Eighty Four theme."
  (interactive)
  (load-theme 'eighty-four t))


(provide-theme 'eighty-four)
