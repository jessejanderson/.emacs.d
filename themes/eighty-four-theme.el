;; If you are distributing this theme, please replace this comment
;; with the appropriate license attributing the original VS Code
;; theme author.

(deftheme eighty-four "A nice dark theme.")


(let (
(color0 "#0d0f31")
(color1 "#f1f1f1")
(color2 "#212345")
(color3 "#B3F361")
(color4 "#3a3c5e")
(color5 "#26284a")
(color6 "#1b1c39")
(color7 "#c9d2e3")
(color8 "#2f304d")
(color9 "#dde6f7")
(color10 "#070930")
(color11 "#585a7c")
(color12 "#fcfcfc")
(color13 "#525863")
(color14 "#96A1FF")
(color15 "#FF16B0")
(color16 "#3B4D66")
(color17 "#1c1e40")
(color18 "#ffffff")
(color19 "#181a3c")
(color20 "#46bdff")
(color21 "#df81fc")
)

(custom-theme-set-faces
'eighty-four


;; BASIC FACES
`(default ((t (:background ,color0 :foreground ,color1 ))))
`(hl-line ((t (:background ,color2 ))))
`(cursor ((t (:foreground ,color3 ))))
`(region ((t (:background ,color4 ))))
`(secondary-selection ((t (:background ,color5 ))))
`(fringe ((t (:background ,color0 ))))
`(mode-line-inactive ((t (:background ,color6 :foreground ,color7 ))))
`(mode-line ((t (:background ,color8 :foreground ,color9 ))))
`(minibuffer-prompt ((t (:background ,color10 ))))
`(vertical-border ((t (:foreground ,color11 ))))

;; FONT LOCK FACES
`(font-lock-builtin-face ((t (:foreground ,color12 ))))
`(font-lock-comment-face ((t (:foreground ,color13 :fontStyle :italic t ))))
`(font-lock-constant-face ((t (:foreground ,color14 ))))
`(font-lock-function-name-face ((t (:foreground ,color12 :fontStyle :bold t ))))
`(font-lock-keyword-face ((t (:foreground ,color15 :fontStyle :bold t ))))
`(font-lock-variable-name-face ((t (:foreground ,color14 ))))


;; linum-mode
`(linum ((t (:foreground ,color16 ))))
`(linum-relative-current-face ((t (:foreground ,color16 ))))


;; display-line-number-mode
`(line-number ((t (:foreground ,color16 ))))
`(line-number-current-line ((t (:foreground ,color16 ))))


;; THIRD PARTY PACKAGE FACES


;; doom-modeline-mode
`(doom-modeline-bar ((t (:background ,color8 :foreground ,color9 ))))
`(doom-modeline-inactive-bar ((t (:background ,color6 :foreground ,color7 ))))


;; web-mode
`(web-mode-html-tag-face ((t (:foreground ,color15 ))))
`(web-mode-html-tag-bracket-face ((t (:foreground ,color15 ))))



;; company-mode
`(company-tooltip ((t (:background ,color17 :foreground ,color18 ))))


;; org-mode
`(org-block ((t (:background ,color19 :foreground ,color12 ))))
`(org-block-begin-line ((t (:foreground ,color13 ))))))


(custom-theme-set-variables
  'eighty-four
  '(linum-format " %3i "))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


;;;###autoload
(defun make-apps-theme()
  "Apply the 1984 theme."
  (interactive)
  (load-theme 'eighty-four t))


(provide-theme 'eighty-four)


;; Local Variables:
;; no-byte-compile: t
;; End:


;; Generated using https://github.com/nice/themeforge
;; Feel free to remove the above URL and this line.
