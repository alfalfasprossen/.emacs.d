; keyboard and text-operation config

(load "init_ergoemacs")



(defun reindent-whole-buffer-python ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun reindent-whole-buffer-cc ()
  "indent whole buffer"
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)))


(defun smart-beginning-of-line ()
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         ;(beginning-of-line)
	 (beginning-of-visual-line))))
(global-set-key [home] 'smart-beginning-of-line)
(global-set-key (kbd "M-h") 'smart-beginning-of-line)

;; when moving one word forward, move to the beginning of the word, not the
;; end of it
(defun forward-word-to-beginning (&optional n)
  (interactive "p")
  (let (myword)
    (setq myword
      (if (and transient-mark-mode mark-active)
        (buffer-substring-no-properties (region-beginning) (region-end))
        (thing-at-point 'symbol)))
    (if (not (eq myword nil))
      (progn
	(subword-forward)))
      (progn
	(subword-forward)
	(subword-backward))))

;;(global-set-key (kbd "M-o") 'forward-word-to-beginning)
;;(global-set-key (kbd "M-o") 'forward-word)

;; when moving wordwise, always stop at the next word boundary, beginning or end
(defun intelligently-subword-forward ()
  (interactive)
  (if (looking-at "[[:alpha:]]") ; if inside or before a word
      (subword-forward) ; go to the end of that word
    (progn (subword-forward) ; else go to the beginning of the next word
	   (subword-backward))))
(global-set-key (kbd "M-o") 'intelligently-subword-forward)

(defun intelligently-subword-backward ()
  (interactive)
  (if (looking-back "[[:alpha:]]") ; if inside or behind a word
      (subword-backward) ; go to the beginning of that word
    (progn (subword-backward) ; else go to the end of the previous word
	   (subword-forward))))
(global-set-key (kbd "M-u") 'intelligently-subword-backward)

(global-set-key (kbd "C-x C-f") 'find-file-at-point)
(global-set-key [(meta ?m)] 'highlight-symbol-at-point)
(global-set-key [(meta shift ?m)] 'highlight-symbol-next)

(global-set-key (kbd "C-8") 'whitespace-mode)
(global-set-key (kbd "C-9") 'sr-speedbar-toggle)


(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(global-set-key (kbd "M-4") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-)") 'delete-other-windows-vertically)
(global-set-key (kbd "M-#") 'delete-other-windows-vertically)
(global-set-key (kbd "M-1") 'delete-other-windows)

;;(global-set-key (kbd "M-w") 'subword-mark)
(global-set-key (kbd "C-t") 'subword-transpose)

;; (require 'drag-stuff)
;; (drag-stuff-global-mode)

(require 'expand-region)
(global-unset-key (kbd "M-w"))
(global-set-key (kbd "M-w") 'er/expand-region)

(cua-mode t)
(global-set-key (kbd "M-c") 'cua-copy-region)
(global-set-key (kbd "M-x") 'cua-cut-region)
(global-set-key (kbd "M-v") 'cua-paste)

(require 'smartparens-config)
(global-set-key (kbd "C-M-o")'sp-forward-sexp)
(global-set-key (kbd "C-M-u")'sp-backward-sexp)

(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-'") 'comment-or-uncomment-region-or-line)

(global-set-key (kbd "C-l") 'toggle-truncate-lines)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-c p h") 'helm-projectile)
(global-set-key (kbd "C-c p g") 'helm-projectile-find-file-dwim)
(global-set-key (kbd "M-[") 'string-inflection-all-cycle)
