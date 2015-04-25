;; windows OS specific inits

;; suppress unsafeness error on windows
(require 'server)
(when (eq system-type 'win32)
  (defun server-ensure-safe-dir (dir) "Noop" t) ; suppress ".emacs.d/server is unsafe" error 
)
(unless (server-running-p)
  (server-start))
