
;stuff to do to the emacs windows, like hiding the toolbar and other visual stuff

(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 1)

;; display visited file's path in frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))

(add-to-list 'load-path "~/.emacs.d/lisp/color-theme-6.6.0/")
(require 'color-theme)
;(require 'zenburn-theme)
(eval-after-load "color-theme"
  '(progn
	 (color-theme-initialize)
     (color-theme-gunmetal) ;first load the gunmetal theme as a good general basis
     (color-theme-jb-night) ;then overwrite the important stuff with my color theme
))


;;(require 'sublimity)
;;(require 'sublimity-scroll)
;;(require 'sublimity-map)
;;(setq sublimity-scroll-weight 1
;;      sublimity-scroll-drift-length 0)
;;(sublimity-mode 1)
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))
(setq scroll-step 1)

;; prevent emacs from splitting windows automatically on high-res
;; screens. reuse the default vertical split instead
(setq split-height-threshold 2200)
(setq split-width-threshold 600)

(column-number-mode 1)
(setq tooltip-mode nil)
