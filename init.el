(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(org-babel-load-file
 (expand-file-name "settings.org"
				   user-emacs-directory))
