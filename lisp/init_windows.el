;; windows OS specific inits

;; suppress unsafeness error on windows
(require 'server)
(when (eq system-type 'win32)
  (defun server-ensure-safe-dir (dir) "Noop" t) ; suppress ".emacs.d/server is unsafe" error 
)
(unless (server-running-p)
  (server-start))

(set-default-font "Lucida Console 12")

(setq explicit-shell-file-name
      "C:/Program Files/Git/bin/sh.exe")
(setq shell-file-name explicit-shell-file-name)
(add-to-list 'exec-path "C:/Program Files/Git/bin/")


(defun my-shell-setup ()
  "For Cygwin bash under Emacs 20"
  (setq comint-scroll-show-maximum-output 'this)
  (make-variable-buffer-local 'comint-completion-addsuffix))
  (setq comint-completion-addsuffix t)
  ;; (setq comint-process-echoes t) ;; reported that this is no longer needed
  (setq comint-eol-on-send t)
  (setq w32-quote-process-args ?\")

(add-hook 'shell-mode-hook 'my-shell-setup)
