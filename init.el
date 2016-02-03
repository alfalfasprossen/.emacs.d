
(add-to-list 'load-path "~/.emacs.d/lisp/")
;(load "init_windows") ; windows specific

;; make sure package installed packages are found before I use them
;; inside my own init files
(setq package-enable-at-startup nil)
(load "init_elpa.el")
(load "init_packages.el")

;; initialize my keyboard settings before trying something stupid
(load "init_keyboard") ; so if something fails, i still have MY KEYBOARD SETTINGS!!!

(when (eq system-type 'darwin) ;; mac specific settings
  (load "init_mac")) ; mac specific
(load "init_settings") ; lots of general settings
(load "init_window") ; visual appearance
(when (eq system-type 'windows-nt) ;; windows specific
  (load "init_windows"))
(load "init_progmodes") ;


;; -- stuff that needs to be sorted into other files and temporary testing --
;;----------------------------------------------------------------------------
;; -
;; -


(defun --copy-face (new-face face)
  "Define NEW-FACE from existing FACE."
  (copy-face face new-face)
  (eval `(defvar ,new-face nil))
  (set new-face new-face))

(--copy-face 'font-lock-label-face  ; labels, case, public, private, proteced, namespace-tags
         'font-lock-keyword-face)
(--copy-face 'font-lock-doc-markup-face ; comment markups such as Javadoc-tags
         'font-lock-doc-face)
(--copy-face 'font-lock-doc-string-face ; comment markups
         'font-lock-comment-face)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(add-hook
 'c++-mode-hook
 '(lambda()
    ;; We could place some regexes into `c-mode-common-hook', but note that their evaluation order
    ;; matters.
    (font-lock-add-keywords
     nil '(;; complete some fundamental keywords
           ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
           ;; namespace names and tags - these are rendered as constants by cc-mode
           ("\\<\\(\\w+::\\)" . font-lock-function-name-face)
           ;;  new C++11 keywords
           ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
           ("\\<\\(char16_t\\|char32_t\\)\\>" . font-lock-keyword-face)
           ;; PREPROCESSOR_CONSTANT, PREPROCESSORCONSTANT
           ("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
           ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face)
           ;; hexadecimal numbers
           ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
           ;; integer/float/scientific numbers
           ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
           ;; c++11 string literals
           ;;       L"wide string"
           ;;       L"wide string with UNICODE codepoint: \u2018"
           ;;       u8"UTF-8 string", u"UTF-16 string", U"UTF-32 string"
           ("\\<\\([LuU8]+\\)\".*?\"" 1 font-lock-keyword-face)
           ;;       R"(user-defined literal)"
           ;;       R"( a "quot'd" string )"
           ;;       R"delimiter(The String Data" )delimiter"
           ;;       R"delimiter((a-z))delimiter" is equivalent to "(a-z)"
           ("\\(\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\)" 1 font-lock-keyword-face t) ; start delimiter
           (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\(.*?\\))[^\\s-\\\\()]\\{0,16\\}\"" 1 font-lock-string-face t)  ; actual string
           (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(.*?\\()[^\\s-\\\\()]\\{0,16\\}\"\\)" 1 font-lock-keyword-face t) ; end delimiter

           ;; user-defined types (rather project-specific)
           ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(type\\|ptr\\)\\>" . font-lock-type-face)
           ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
           ))
    ) t)

;;(global-git-gutter-mode t)
(git-gutter:linum-setup)
;;(custom-set-variables
 ;;'(git-gutter:update-interval 2))

(setq scroll-preserve-screen-position t)
(global-auto-highlight-symbol-mode)

(unless (fboundp 'cua-replace-region)
  (defun cua-replace-region ()
    "Replace the active region with the character you type."
    (interactive)
    (let ((not-empty (and cua-delete-selection (cua-delete-region))))
      (unless (eq this-original-command this-command)
        (let ((overwrite-mode
               (and overwrite-mode
                    not-empty
                    (not (eq this-original-command 'self-insert-command)))))
          (cua--fallback))))))

(require 'cl)
(defun kill-buffers-regexp (regexp)
  "Kill buffers matching REGEXP without asking for confirmation."
  (interactive "sKill buffers matching this regular expression: ")
  (cl-flet ((kill-buffer-ask (buffer) (kill-buffer buffer)))
    (kill-matching-buffers regexp)))

;; Kill "unused" buffers over night
(require 'midnight)
;; never kill org mode or lisp buffers
(setq clean-buffer-list-kill-never-regexps
      '(".+\.org"
	".+\.el"))
;; kill other buffers only if they weren't used for 14 days, default would be 3 days
;; (switching between python and c++ projects sometimes takes longer)
(setq clean-buffer-list-delay-general 14)

;; Rename the file on disc as well as the buffer
(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
 (let ((name (buffer-name))
	(filename (buffer-file-name)))
 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (if (get-buffer new-name)
	 (message "A buffer named '%s' already exists!" new-name)
	(progn 	 (rename-file name new-name 1) 	 (rename-buffer new-name) 	 (set-visited-file-name new-name) 	 (set-buffer-modified-p nil)))))) ;;

;; move the file of the used buffer to another directory
(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	 (if (string-match dir "\\(?:/\\|\\\\)$")
	 (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))

 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (progn 	(copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name newname) 	(set-buffer-modified-p nil) 	t))))

;;; python intelly sense stuff
;; pymacs
;(add-to-list 'load-path "~/.emacs.d/lisp/pymacs")
;(autoload 'pymacs-apply "pymacs")
;(autoload 'pymacs-call "pymacs")
;(autoload 'pymacs-eval "pymacs" nil t)
;(autoload 'pymacs-exec "pymacs" nil t)
;(autoload 'pymacs-load "pymacs" nil t)
;(autoload 'pymacs-autoload "pymacs")
;;;(eval-after-load "pymacs"
;;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
;
;; flymake for python with pyflakes
;(when (load "flymake" t)
;     (defun flymake-pyflakes-init ()
;       (let* ((temp-file (flymake-init-create-temp-buffer-copy
;                          'flymake-create-temp-inplace))
;      (local-file (file-relative-name
;               temp-file
;               (file-name-directory buffer-file-name))))
;         (list "pyflakes" (list local-file))))
;
;     (add-to-list 'flymake-allowed-file-name-masks
;          '("\\.py\\'" flymake-pyflakes-init)))
;
;(add-hook 'find-file-hook 'flymake-find-file-hook)
;
;(defun my-flymake-show-help ()
;  (when (get-char-property (point) 'flymake-overlay)
;   (let ((help (get-char-property (point) 'help-echo)))
;    (if help (message "%s" help)))))
;
;(add-hook 'post-command-hook 'my-flymake-show-help)
;
;; auto complete for rope
;(ac-ropemacs-initialize)
;(add-hook 'python-mode-hook
;      (lambda ()
;    (add-to-list 'ac-sources 'ac-source-ropemacs)))




;(add-to-list 'load-path "~/.emacs.d/lisp/yasnippet")
;(require 'yasnippet)
;(yas-global-mode 1)


; -
; -
;;----------------------------------------------------------------------------


;; Variables configured via the interactive 'customize' interface
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(load "init_keyboard") ; and now make sure no keys have been overriden by some packages
(ido-mode 1)
(setq ido-enable-flex-matching 1)




;; disable git as it slows down emacs extremely
(setq vc-handled-backends ())
