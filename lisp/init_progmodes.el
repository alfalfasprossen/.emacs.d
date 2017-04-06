;;;; CUSTOMMODES init
(add-to-list 'load-path "~/.emacs.d/lisp/progmode/")

;;; ----------------------------------------------------------------------------
;;; --- GENERAL STUFF ---
;;; ----------------------------------------------------------------------------
;; adapt to different indentation styles
;(require 'dtrt-indent)
;(dtrt-indent-mode 1)

(setq-default tab-width 4)

;; smart parens, automatic paren completion etc.
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; Enter key executes newline-and-indent
;; this needs to be set for some modes explicitly
(defun set-newline-and-indent ()
  "Map the return key with `newline-and-indent'"
  (local-set-key (kbd "RET") 'newline-and-indent))

;; load modes automatically
(autoload 'cg-mode "cg-mode" "Cg editing mode." t)
(autoload 'mel-mode "mel-mode" "Mel editing mode." t)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(autoload 'mma-mode "mma.el" "Mathematica package file mode" t)
(autoload 'rsl-mode "rsl-mode" "RenderMan Shading Language editing mode" t)
(autoload 'rib-mode "rib-mode" "RenderMan Interface Bytestream editing mode" t)

;; use other python mode
(setq py-install-directory "~/.emacs.d/lisp/python-mode/")
(add-to-list 'load-path py-install-directory)
(require 'python-mode)

;; Load specific file extensions with a appropriate mode
(setq auto-mode-alist
     (append '(("\\.cs$" . csharp-mode)
               ("\\.cg$" . cg-mode)
               ("\\.hlsl$" . cg-mode)
               ("\\.fxh?$" . cg-mode)
               ("\\.sl$" . rsl-mode)
               ("\\.rib$" . rib-mode)
               ("\\.ma$" . mel-mode)
               ("\\.mel$" . mel-mode)
               ("\\.lua$" . lua-mode)
               ("\\.hs$" . haskell-mode)
               ("\\.lhs$" . literate-haskell-mode)
               ;("\\.m$" . mma-mode)
               ("\\.org$" . org-mode)
               ("\\.py$" . python-mode)
	       ("\\.h$" . c++-mode)
	       ("\\.inl$" . c++-mode)
	       ("\\.js$" . js2-mode)
	       ("\\.html$" . web-mode))
             auto-mode-alist))

;; automatic camelCase or snake_case when typing word-word
;(require 'electric-case)
;;(global-visual-line-mode t)
(require 'highlight-indentation) ;; indentation highlight

;;; --- nxml mode outline folding ---
(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))
(add-hook 'nxml-mode-hook
	  (lambda ()
	    (hs-minor-mode)
	    (hideshowvis-enable)))


;;; ----------------------------------------------------------------------------
;;; --- Maxscript mode for 3DsMax ---
(add-to-list 'load-path "~/.emacs.d/lisp/maxscript-mode/")
(autoload 'maxscript-mode "maxscript-mode" "maxscript-mode" t)
(setq auto-mode-alist (append '(("\.ms$" . maxscript-mode)) auto-mode-alist))

(add-hook
 'maxscript-mode-hook
 (lambda ()
   (require 'send-to-max)
   (local-set-key [S-return] 'maxscript-send-line-or-region)
   (local-set-key (kbd "C-e") 'maxscript-send-file)
   (local-set-key (kbd "C-c C-c") 'maxscript-send-buffer)
   (local-set-key (kbd "C-c C-d") 'maxscript-clear-output)
   (fci-mode)
   (whitespace-mode)
   (require 'smart-dash)
   (smart-dash-mode)))
(add-hook 'maxscript-mode-hook 'set-newline-and-indent)
;; max-python stuff
(add-hook
 'python-mode-hook
 (lambda ()
   (require 'send-to-max)
   (local-set-key [S-return] 'maxscript-send-line-or-region-py)
   (local-set-key (kbd "C-e") 'maxscript-send-file)))



;;; --- END Maxscript setup ---
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; --- Maya MEL and Python integration ---
;; etom stuff mel
(add-hook
 'mel-mode-hook
 (lambda ()
   (require 'etom)
   (setq etom-default-host "localhost")
   (setq etom-default-port 2222)
   (local-set-key (kbd "C-c C-r") 'etom-send-region)
   (local-set-key (kbd "C-c C-c") 'etom-send-buffer)
   (local-set-key (kbd "C-c C-l") 'etom-send-buffer)
   (local-set-key (kbd "C-c C-z") 'etom-show-buffer)))
;; etom stuff python
(add-hook
 'python-mode-hook
 (lambda ()
   (require 'etom)
   (setq etom-default-host "localhost")
   (setq etom-default-port 2222)
   (local-set-key (kbd "C-c C-m C-r") 'etom-send-region-py)
   (local-set-key (kbd "C-c C-m C-c") 'etom-send-buffer-py)
   (local-set-key (kbd "C-c C-m C-l") 'etom-send-buffer-py)
   (local-set-key (kbd "C-c C-m C-z") 'etom-show-buffer)))

;;; --- END Maya setup ---
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; --- AC and COMPANY setup ---
;; use ac or company mode for c-modes?
(defconst cmode-use-ac nil)

;; - AUTO COMPLETE
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(require 'popup)
(setq ac-auto-start 1)

;(add-to-list 'ac-sources 'ac-source-yasnippet))
;;(global-auto-complete-mode 0) ; disable AC globally
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/ac-dict")

;; disable ac in c-modes completely, when using company
;; (unless cmode-use-ac
;;   (defadvice auto-complete-mode (around disable-auto-complete-for-c++)
;;     (unless (or (eq major-mode 'c++-mode)
;;     		(eq major-mode 'c-mode)
;;     		(eq major-mode 'objc-mode))
;;       ad-do-it))
;;   (ad-activate 'auto-complete-mode))
;;(setq ac-quick-help-delay 0.3)

;; - COMPANY (alternative to AC)
(require 'company)
(setq company-dabbrev-ignore-case t)
;(require 'company-quickhelp)
;(company-quickhelp-mode)
;; (unless cmode-use-ac
;;   (add-hook 'c-mode-common-hook
;; 	    (lambda ()
;; 	      )))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (highlight-parentheses-mode t)
	    (hideshowvis-enable)))

;;; --- END ac and company setup
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; --- C++ / C-MODES ---
(setq c-basic-offset 4)
(setq c-indent-level 4)
(c-set-offset 'substatement-open 0)
(c-set-offset 'inline-open 0)

(smart-tabs-add-language-support c++ c++-mode-hook
  ((c-indent-line . c-basic-offset)
   (c-indent-region . c-basic-offset)))
(smart-tabs-insinuate 'c++ 'c)

(add-hook 'c-mode-common-hook ; for c++, c and obj-c
	  (lambda ()
		(c-set-offset 'substatement-open 0)
	    (setq indent-tabs-mode t) ; indent with tabs, not spaces
	    (setq tab-width 4) ; a tab is 4 spaces wide
	    (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
	    (hideshowvis-enable)
	    (fci-mode)
	    (whitespace-mode)
		(add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
	    (setq truncate-lines t)))
(add-hook 'csharp-mode-hook
	  (lambda ()
	    (setq c-basic-offset 4)))

;;; --- END C++ / C-modes setup ---
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; --- PYTHON ---
;;(load-file "~/.emacs.d/lisp/emacs-for-python/epy-init.el")
;;(epy-setup-checker "pyflakes %f")
;;(global-hl-line-mode t) ;; line highligh
;;(set-face-background 'hl-line "black")

;; Python flymake configuration

;; (when (load "flymake" t)
;;   (defun flymake-pycheckers-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; 		       'flymake-create-temp-inplace))
;; 	   (local-file (file-relative-name
;; 			temp-file
;; 			(file-name-directory buffer-file-name))))
;;       (list "pycheckers.bat"  (list local-file))))

;;   (add-to-list 'flymake-allowed-file-name-masks
;; 	       '("\\.py\\'" flymake-pycheckers-init)))

;; jedi python mode stuff
(add-hook 'python-mode-hook
	  (lambda ()
	    (jedi:setup)
	    (highlight-indentation-mode t)
	    (highlight-parentheses-mode t)
	    (hideshowvis-enable)
	    ;;(define-key python-mode-map (kbd "M->") 'python-indent-shift-right)
	    ;;(define-key python-mode-map (kbd "M-<") 'python-indent-shift-left)
	    (define-key python-mode-map (kbd "M->") 'py-shift-right)
	    (define-key python-mode-map (kbd "M-<") 'py-shift-left)
	    (fci-mode t)
	    (whitespace-mode t)
	    (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
	    ;;(flymake-mode)
	    ;;(flymake-python-pyflakes-load)
	    (flycheck-mode)
	    (visual-line-mode nil)
	    (require 'smart-dash)
	    (smart-dash-mode)))
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional

(add-hook 'python-mode-hook 'set-newline-and-indent)

;; Ignoring electric indentation
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode"
  (if (equal major-mode 'python-mode)
      `no-indent'
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

;;; --- END Python setup ---
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; --- Javascript setup ---
(add-hook 'js2-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (highlight-parentheses-mode t)
	    (hideshowvis-enable)
	    (fci-mode t)
	    (whitespace-mode t)
	    (highlight-indentation-mode t)
	    (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
	    (flycheck-mode t)
	    ;;(setq 'font-lock-doc-face jbcl_docstring)
	    ))

(add-hook 'css-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (highlight-indentation-mode t)
	    (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))

(add-hook 'html-mode-hook
	  (lambda ()
	    (setq tab-width 2)
	    (setq indent-tabs-mode nil)
	    (highlight-indentation-mode t)
	    (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))

(defvar web-mode-html-offset) ;; highlight indent mode issue see below hook
(add-hook 'web-mode-hook
	  (lambda ()
	    (rainbow-mode t)
	    (setq-default tab-width 2)
	    (setq-default indent-tabs-mode nil)
      (highlight-indentation-mode t)
	    (add-hook 'before-save-hook
		      'delete-trailing-whitespace nil t)))
	    ;; (setq tab-width 2)
	    ;; (setq indent-tabs-mode nil)
	    ;; (highlight-indentation-mode t) ;https://github.com/antonj/Highlight-Indentation-for-Emacs/pull/27
;;; --- END Javascript setup ---
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; --- CEDET AND C-MODES INTELLISENSE SETUP ---

;; cedet and ecb project management stuff
;;(load-file "~/.emacs.d/lisp/cedet-1.1/common/cedet.el") ;outdated
;;(load-file "~/.emacs.d/lisp/cedet-bzr/cedet-devel-load.el")
;; enable ede project management features
;;(global-ede-mode 1)


;; enable code folding (only works with semantic enabled)
;(load-library "contrib/semantic-tag-folding.el")
;(defun do-after-decorate () (semantic-tag-folding-mode t) )
;(add-hook 'semantic-decoration-mode-hook 'do-after-decorate)
;(add-hook 'c-mode-common-hook 'do-after-decorate)


;; --- rtags setup ---
(when (eq system-type 'darwin) ; only if on mac
  (require 'cl-lib)
  (add-to-list 'load-path "~/rtags/src")
  (require 'rtags)
  (rtags-enable-standard-keybindings c-mode-base-map)
  (setq rtags-completions-enabled t)
  (setq rtags-spellcheck-enabled nil)
  (rtags-diagnostics); enable interactive rtags feedback

  ;; - rtags auto-complete -
  (when cmode-use-ac
    (print "C using auto complete")
    (require 'rtags-ac)
    (require 'auto-complete-clang)
    ;(add-hook 'c-mode-common-hook
    (add-hook 'c++-mode-hook
	      (lambda ()
		(company-mode 0)
		(auto-complete-mode 1)
		;(rtags-diagnostics) ; enable interactive rtags feedback
		;(local-set-key (kbd "C-M-i") 'ac-complete-rtags)
		;(setq completion-at-point-functions '(ac-complete-rtags))
		;(setq ac-sources '(ac-source-rtags)) ; only rtags
		(setq ac-sources '(ac-source-clang))
		(rtags-ac-init))))

  ;; - rtags company -
  (unless cmode-use-ac
    (print "C using company")
    (require 'company-rtags)
    (add-hook 'c-mode-common-hook
	      (lambda ()
		;(rtags-diagnostics) ; enable interactive rtags feedback
		(auto-complete-mode 0)
		(company-mode 1)
		(local-set-key (kbd "C-M-i") 'company-complete)
		;(setq company-backends '(company-rtags)) ; only rtags
		(setq company-backends '(company-clang))
		)))

); end only if on mac

;;; --- END cedet and intellisense setup ---
;;; ----------------------------------------------------------------------------


;; perforce mode (use p4.el from gareth-rees github)
(require 'p4)


;; Markdown mode
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(custom-set-variables
 '(livedown:autostart nil) ; automatically open preview when opening markdown files
 '(livedown:open t)        ; automatically open the browser window
 '(livedown:port 1337))    ; port for livedown server

(add-to-list 'load-path "~/.emacs.d/lisp/emacs-livedown/")
(require 'livedown nil "soft")

(global-magit-file-mode t)



;; ediff copy both changes to result buffer
;; http://stackoverflow.com/questions/9656311
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)


(defun indent-with-tabs ()
  (setq indent-tabs-mode t))

(defun indent-with-spaces ()
  (setq indent-tabs-mode nil))
