;;
;;
;;


(require 'color-theme)

(defun color-theme-jb-night ()
  (interactive)
  ;note for testing: i can redef a var using C-M-x
  (setq jbcl_background "gray20")
  (setq jbcl_foreground "gray80")
  (setq jbcl_cursor "magenta")

  (setq jbcl_acute "yellow")
  (setq jbcl_acutebg "red2")
  (setq jbcl_highlight "cyan")
  (setq jbcl_highlightbg "tomato3")
  (setq jbcl_inactive "gray40")
  (setq jbcl_inactivebg "gray7")

  (setq jbcl_comment "red2")
  (setq jbcl_commentbg "gray8")
  (setq jbcl_docstring "SteelBlue2")
  (setq jbcl_docstringbg jbcl_commentbg)
  (setq jbcl_string "chartreuse3")
  (setq jbcl_keyword "DarkTurquoise") ;things like: const, extern, static, return, new, this, if, for
  (setq jbcl_type "DarkTurquoise") ;things like: class, int, void, classnames,
  (setq jbcl_constant "LightGoldenrod3") ;things like: NULL, true, but sometimes other stuff like the name of a namespace:: or class:: etc. and private, public etc. 
  (setq jbcl_variable "goldenrod1") ;names of variables, but only in the place they are defined. useless...
  (setq jbcl_function "DarkGoldenrod3") ;the names of functions and methods... when recognized correctly
  (setq jbcl_builtin jbcl_keyword) ;seems to not be used in c/c++, but in elisp
  (setq jbcl_preprocessor "SlateBlue1") 
  (setq jbcl_operator "SeaGreen3") ;seems to not be used in c/c++
  (color-theme-jb-apply)
)

(defun color-theme-jb ()
  (interactive)
  (color-theme-jb-night)
)

(defun color-theme-jb-day ()
  (interactive)

  (setq jbcl_background "gray90")
  (setq jbcl_foreground "gray5")
  (setq jbcl_cursor "magenta")

  (setq jbcl_acute "red2")
  (setq jbcl_acutebg "orange1")
  (setq jbcl_highlight "cyan")
  (setq jbcl_highlightbg "tomato3")
  (setq jbcl_inactive "gray40")
  (setq jbcl_inactivebg "gray7")

  (setq jbcl_comment "red2")
  (setq jbcl_commentbg "gray80")
  (setq jbcl_docstring "MediumBlue")
  (setq jbcl_docstringbg jbcl_commentbg)
  (setq jbcl_string "ForestGreen")
  (setq jbcl_keyword "cyan4") ;things like: const, extern, static, return, new, this, if, for
  (setq jbcl_type "cyan4") ;things like: class, int, void, classnames,
  (setq jbcl_constant jbcl_foreground) ;things like: NULL, true, but sometimes other stuff like the name of a namespace:: or class:: etc. and private, public etc. 
  (setq jbcl_variable "DarkGoldenrod") ;names of variables, but only in the place they are defined. useless...
  (setq jbcl_function "goldenrod3") ;the names of functions and methods... when recognized correctly
  (setq jbcl_builtin jbcl_keyword) ;seems to not be used in c/c++, but in elisp
  (setq jbcl_preprocessor "DarkViolet") 
  (setq jbcl_operator "SeaGreen3") ;seems to not be used in c/c++
  (color-theme-jb-apply)  
)

(defun color-theme-jb-apply ()
  (interactive)
  (color-theme-install
   `(color-theme-jb

;;general colors
;     (default ((t 
;               (:stipple nil 
;                :background "gray20" 
;                :foreground "gray80" 
;                :inverse-video nil 
;                :box nil 
;                :strike-through nil 
;                :overline nil 
;                :underline nil 
;                :slant normal 
;                :weight normal 
;                :height 94 
;                :width semi-condensed 
;                :family "jmk-neep"
;               )))
;     )

     ((background-color . ,jbcl_background)
      (background-mode . dark)		
      (foreground-color . ,jbcl_foreground)
      (cursor-color . ,jbcl_cursor))

;; CUA 
    ((cua-global-mark-cursor-color . "cyan")
      (cua-normal-cursor-color . "black")
      (cua-overwrite-cursor-color . "chartreuse")
      (cua-read-only-cursor-color . "darkgreen"))
;     (cursor ((t (:background "magenta"))))

     (paren ((t (nil))))
     (paren-blink-off ((t (:foreground ,jbcl_foreground))))
     (paren-dim-face ((t (nil))))
     (paren-face ((t (:background ,jbcl_background))))
     (paren-face-match ((t (:background "turquoise"))))
     (paren-face-mismatch ((t (:background "purple" :foreground "white"))))
     (paren-face-no-match ((t (:background "yellow" :foreground "gray30"))))
     (paren-match ((t (:background "darkseagreen2"))))
     (paren-mismatch ((t (:background "RosyBrown" :foreground "gray30"))))
     (paren-mismatch-face ((t ( :background "white" :foreground "red" ))))
     (paren-no-match-face ((t ( :background "white" :foreground "red" ))))

     (show-paren-match ((t ( :background ,jbcl_acutebg :foreground ,jbcl_acute ))))
     (show-paren-mismatch ((t (:background "purple" :foreground "cyan"))))

     
;; MODE LINE
     (mode-line ((t (:background ,jbcl_background :foreground ,jbcl_acute :box (:line-width -1 :color ,jbcl_highlightbg)))))
     (mode-line-buffer-id ((t ( :foreground ,jbcl_foreground ))))
     (mode-line-highlight ((t (:background "blueviolet" :foreground "cyan"))))
     (mode-line-inactive ((t (:background ,jbcl_inactivebg :foreground ,jbcl_inactive))))
;; ibuffer isearch iswitch (maybe colors included in modeline stuff?
     (ibuffer-marked-face ((t (:foreground "gray85"))))

     (isearch ((t (:background ,jbcl_acutebg :foreground ,jbcl_acute))))
     (isearch-lazy-highlight ((t (nil))))
     (isearch-secondary ((t (:background "darkred" :foreground "yellow"))))
     (iswitchb-current-match ((t (nil))))
     (iswitchb-invalid-regexp ((t (nil))))
     (iswitchb-single-match ((t (nil))))
     (iswitchb-virtual-matches ((t (nil))))


;; syntax highlighting colors
     (font-lock-builtin-face ((t (:bold t :foreground ,jbcl_builtin))))
     (font-lock-comment-delimiter-face ((t (:background ,jbcl_commentbg :foreground ,jbcl_comment))))
     (font-lock-comment-face ((t (:background ,jbcl_commentbg :foreground ,jbcl_comment))))
     (font-lock-constant-face ((t (:foreground ,jbcl_constant))))
     (font-lock-doc-face ((t (:foreground ,jbcl_docstring))))
     (font-lock-doc-string-face ((t (:background ,jbcl_docstringbg :foreground ,jbcl_docstring))))
     (font-lock-emphasized-face ((t (:foreground "firebrick"))))
     (font-lock-exit-face ((t (:foreground "blue"))))
     (font-lock-function-name-face ((t (:foreground ,jbcl_function))))
     (font-lock-keyword-face ((t (:bold t :foreground ,jbcl_keyword )))) 
     (font-lock-operator-face ((t (:foregrond ,jbcl_operator))))
     (font-lock-other-emphasized-face ((t (nil))))
     (font-lock-other-type-face ((t (nil))))
     (font-lock-preprocessor-face ((t (:foreground ,jbcl_preprocessor))))
     (font-lock-pseudo-keyword-face ((t (:foreground "blue"))))
     (font-lock-reference-face ((t (:foreground "cyan"))))
     (font-lock-string-face ((t (:foreground ,jbcl_string))))
     (font-lock-type-def-face ((t (nil))))
     (font-lock-type-face ((t (:foreground ,jbcl_type :underline nil))))
     (font-lock-variable-name-face ((t (:foreground ,jbcl_variable))))
     (font-lock-warning-face ((t (:underline "red"))))
     (font-lock-regexp-grouping-backslash ((t (:foreground "gray10"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "yellow"))))

;; SPEEDBAR ?
;     (cscsd-speedbar-main-section ((t ( :foreground "cyan" ))))
;     (cscsd-speedbar-subsection ((t (:foreground "orange" :underline t))))


;; ORG MODE
     (org-agenda-structure ((t (nil))))
     (org-agenda-date ((t (:foreground "white" :background "gray30" :underline "red"))))
     (org-agenda-date-weekend ((t (:foreground "white" :background "gray30" :underline "blue"))))
     (org-archived ((t (nil))))
     (org-code ((t (:foreground "gray70" :background "gray23"))))
     (org-column ((t (nil))))
     (org-date ((t (:background "forestgreen" :foreground "yellow"))))
     (org-deadline-announce ((t (:foreground "red"))))
     (org-done ((t (:background "gray15" :foreground "gray45"))))
     (org-drawer ((t (nil))))
     (org-ellipsis ((t (nil))))
     (org-formula ((t (:foreground "chocolate1"))))
     (org-headline-done ((t (:foreground "gray45"))))
     (org-hide ((t (:foreground "gray27"))))
     (org-level-1 ((t ( :foreground "white" :underline "red"))))
     (org-level-2 ((t ( :foreground "yellow" ))))
     (org-level-3 ((t (:foreground "paleturquoise"))))
     (org-level-4 ((t (:foreground "deepskyblue"))))
     (org-level-5 ((t (:foreground "dodgerblue"))))
     (org-level-6 ((t (:foreground "royalblue"))))
     (org-level-7 ((t (:foreground "gray50"))))
     (org-level-8 ((t (:foreground "gray35"))))
     (org-link ((t (:foreground "yellowgreen" :underline "yellow"))))
     (org-property-value ((t (:foreground "greenyellow"))))
     (org-scheduled-previously ((t (:foreground "red"))))
     (org-scheduled-today ((t (:foreground "LightSkyBlue"))))
     (org-sexp-date ((t (nil))))
     (org-special-keyword ((t (:foreground "yellow" :background "gray30"))))
     (org-table ((t (:foreground "yellow" :background "gray15"))))
     (org-meta-line ((t (:foreground "skyblue" :background "gray25"))))
     (org-tag ((t (:background "seagreen" :foreground "cyan"))))
     (org-target ((t (nil))))
     (org-time-grid ((t (:foreground "LightGoldenrod"))))
     (org-todo ((t (:background "red" :foreground "yellow"))))
     (org-upcoming-deadline ((t (nil))))
     (org-warning ((t ( :foreground "Red1" ))))

     (outline-1 ((t (:italic t  :foreground "LightSkyBlue" :slant italic ))))
     (outline-2 ((t (:italic t  :foreground "LightGoldenrod" :slant italic ))))
     (outline-3 ((t ( :foreground "Cyan" ))))
     (outline-4 ((t ( :foreground "LightSteelBlue" ))))
     (outline-5 ((t (:italic t :foreground "medium aquamarine" :slant italic))))
     (outline-6 ((t ( :foreground "Aquamarine" ))))
     (outline-7 ((t ( :foreground "PaleGreen" ))))
     (outline-8 ((t (:italic t :foreground "aquamarine" :slant italic))))
)))

(provide 'color-theme-jb)
;;; color-theme-jb.el ends here
