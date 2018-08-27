
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(setq user-home-directory (expand-file-name "~"))
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix)
(setq user-emacs-directory (expand-file-name ".emacs.d" user-home-directory))

(require 'org)

(org-babel-tangle-file
 (expand-file-name "settings.org" user-emacs-directory)
 (expand-file-name ".spacemacs" user-home-directory))
;;(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(setq spacemacs-start-directory (expand-file-name "spacemacs/" user-emacs-directory))
(load-file (concat spacemacs-start-directory "init.el"))
