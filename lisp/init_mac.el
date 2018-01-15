;; mac specific inits

;; mac specific key bindings n stuff

(setq mac-option-modifier 'none) ;make sure alt is used for isocode
(setq mac-command-modifier 'meta)
(setq ns-right-command-modifier 'control) ;; make right command act as control
(global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
(global-set-key (kbd "M-`") 'ns-next-frame)
;;(global-set-key (kbd "M-h") 'ns-do-hide-emacs)
;;(eval-after-load 'nxml-mode
;;    '(define-key nxml-mode-map (kbd "M-h") nil))
;;(global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports
;;(global-set-key (kbd "M-c") 'ns-copy-including-secondary)
;;(global-set-key (kbd "M-v") 'ns-paste-secondary)
(global-set-key (kbd "M-c") 'cua-copy-region)
(global-set-key (kbd "M-v") 'cua-paste)
(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))

(setq exec-path (append exec-path '("/usr/texbin"))) ; latex stuff

;; mac specific path finding stuff
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell 
      (replace-regexp-in-string "[[:space:]\n]*$" "" 
        (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))


;; don't open new window on drag-drop file
(setq ns-pop-up-frames nil)

;; don't show popup dialogs as they tend to crash emacs on OSX currently
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice message-box (around prevent-dialog activate) 
  "Prevent message-box from activating a dialog" 
  (apply #'message 
	 (ad-get-args 0))) 
