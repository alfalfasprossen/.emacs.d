; -*- coding: utf-8 -*-

;;; Keyboard and text-operation configurations.
;;;
;;; Some functionality and basic keyboard layout taken from
;;; ergoemacs (http://ergoemacs.org)


(require 'redo "redo.elc" t) ; for redo shortcut


;; (cua-mode 1)
(delete-selection-mode 1)


(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (message "Current line is copied.")
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))


(defadvice kill-region (before slick-copy activate compile)
  "When called interactively with no active region, cut the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))


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


(defun move-cursor-next-pane ()
  "Move cursor to the next pane."
  (interactive)
  (other-window 1)
  )


(defun move-cursor-previous-pane ()
  "Move cursor to the previous pane."
  (interactive)
  (other-window -1)
  )


(defvar recently-closed-buffers (cons nil nil) "A list of recently closed buffers. The max number to track is controlled by the variable recently-closed-buffers-max.")
(defvar recently-closed-buffers-max 10 "The maximum length for recently-closed-buffers.")

(defun close-current-buffer ()
"Close the current buffer.

Similar to (kill-buffer (current-buffer)) with the following addition:

• prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
• make sure the buffer shown after closing is a user buffer.
• if the buffer is a file, add the path to the list recently-closed-buffers.

A emacs buffer is one who's name starts with *.
Else it is a user buffer."
 (interactive)
 (let (emacsBuff-p isEmacsBufferAfter)
   (if (string-match "^*" (buffer-name))
       (setq emacsBuff-p t)
     (setq emacsBuff-p nil))

   ;; offer to save buffers that are non-empty and modified, even for non-file visiting buffer. (because kill-buffer does not offer to save buffers that are not associated with files)
   (when (and (buffer-modified-p)
              (not emacsBuff-p)
              (not (string-equal major-mode "dired-mode"))
              (if (equal (buffer-file-name) nil) 
                  (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                t
                )
              )
     (if (y-or-n-p
            (concat "Buffer " (buffer-name) " modified; Do you want to save?"))
       (save-buffer)
       (set-buffer-modified-p nil)))

   ;; save to a list of closed buffer
   (when (not (equal buffer-file-name nil))
     (setq recently-closed-buffers
           (cons (cons (buffer-name) (buffer-file-name)) recently-closed-buffers))
     (when (> (length recently-closed-buffers) recently-closed-buffers-max)
           (setq recently-closed-buffers (butlast recently-closed-buffers 1))
           )
     )

   ;; close
   (kill-buffer (current-buffer))

   )
 )


(defun open-last-closed ()
  "Open the last closed file."
  (interactive)
  (find-file (cdr (pop recently-closed-buffers)) ) )


(defun new-empty-buffer ()
  "Opens a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))


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


(defun forward-word-to-beginning (&optional n)
"When moving one word forward, move to the beginning of the word, not 
the end of it."
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


(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))


;; Initialize completions by just hitting Tab instead of M-Tab
(setq tab-always-indent 'complete)

;;; Define my custom keymap. Generally inspired by ergoemacs, but more
;;; limited to general cursor movement and editing, while keeping
;;; most of the more complex bindings at their defaults.
(defvar my-keymap (make-sparse-keymap)
  "My custom keymap.")

;;; --------------------------------------------------
;;; CURSOR MOVEMENTS

;; Single char cursor movement
(define-key my-keymap (kbd "M-h") 'backward-char)
(define-key my-keymap (kbd "M-l") 'forward-char)
(define-key my-keymap (kbd "M-k") 'previous-line)
(define-key my-keymap (kbd "M-j") 'next-line)

;; Move by word
(define-key my-keymap (kbd "M-u") 'backward-word)
(define-key my-keymap (kbd "M-o") 'forward-word-to-beginning)

(define-key my-keymap (kbd "C-M-j") 'backward-list)  ;; needs rethinking
(define-key my-keymap (kbd "C-M-l") 'forward-list)  ;; needs rethinking

;; Move to beginning/ending of line
(define-key my-keymap [home] 'smart-beginning-of-line)
(define-key my-keymap (kbd "M-H") 'smart-beginning-of-line)
(define-key my-keymap (kbd "M-L") 'end-of-line)

;; Move to beginning/ending of file
;; (define-key my-keymap (kbd "M-J") 'beginning-of-buffer)  ;; no need for a convenient mapping,
;; used for scroll-up instead.
;; (define-key my-keymap (kbd "M-L") 'end-of-buffer)  ;; no need for a convenient mapping, should be next-word

(define-key my-keymap (kbd "M-K") 'scroll-down)
(define-key my-keymap (kbd "M-J") 'scroll-up)

;; C-i is always tab
;; C-m is always return
;; C-o is currently open file which is not needed
;; M-p and C-l (default) are recenter, one of them could be reused
;; C-h is always help, so binding C bindings to hjkl is not generally an option
;; C-k is kill line to end C-j could be kill line to beginning
;; C-j is currently open-line which is in it's default behaviour useless
;;   improved functions could be mapped to C-o C-S-o or M-o
;; C-M bindings should be avoided because they don't work on windows
;;   or i will have to switch to a non-international layout
;; M-h and M-H were used to move to beginning-end of line
;;   not sure if there are defaults for this. (C-e and C-a)?
;;   When using M-h for backward char, this needs to be relocated though.
;; M-f and M-b are defaults for forward and backward word. b is a bad char
;;   for this, and movement is generally on the right hand, moving by word
;;   should be located on the right, maybe u and o are ok for this.
;; M-f is also remapped to forward delete char
;; M-y as C-y is cua-paste. Which is already covered by M-v / C-v
;;   where M-V is moving through the kill-ring. That is generally
;;   tedious and often it is clear that an entry that is not the
;;   most recent is desired. A direkt keybinding to helm-show-killring
;;   would be nicer for these occasions. Overwriting M-v (and C-v)
;;   is probably bad because one is so used to that behaviour.
;;   M-V / C-V or M-y could be used for that though.

;;; --------------------------------------------------
;;; MAJOR EDITING COMMANDS

;; Delete previous/next char.
(define-key my-keymap (kbd "M-d") 'delete-backward-char)
(define-key my-keymap (kbd "M-f") 'delete-char)

; Delete previous/next word.
(define-key my-keymap (kbd "M-e") 'backward-kill-word)
(define-key my-keymap (kbd "M-r") 'kill-word)

; Copy Cut Paste, Paste previous
;; (define-key my-keymap (kbd "M-x") 'kill-region)
(define-key my-keymap (kbd "M-c") 'kill-ring-save)
(define-key my-keymap (kbd "M-v") 'yank)
(define-key my-keymap (kbd "M-V") 'yank-pop)

;; undo and redo
(define-key my-keymap (kbd "C-Z") 'redo)
(define-key my-keymap (kbd "C-z") 'undo)

;;; --------------------------------------------------
;;; WINDOWS AND FRAMES

(define-key my-keymap (kbd "M-s") 'move-cursor-next-pane)
(define-key my-keymap (kbd "M-S") 'move-cursor-previous-pane)
(define-key my-keymap (kbd "M-4") 'split-window-vertically)
(define-key my-keymap (kbd "M-3") 'split-window-horizontally)
(define-key my-keymap (kbd "M-0") 'delete-window)
(define-key my-keymap (kbd "M-1") 'delete-other-windows)

(define-key my-keymap (kbd "M-~") 'switch-to-previous-frame)
(define-key my-keymap (kbd "M-`") 'switch-to-next-frame)

(define-key my-keymap (kbd "M-p") 'recenter-top-bottom)

;;; --------------------------------------------------
;;; STANDARD SHORTCUTS (CUA - like)

(define-key my-keymap (kbd "C-o") 'helm-find-files)
(define-key my-keymap (kbd "C-S-t") 'open-last-closed)
(define-key my-keymap (kbd "C-w") 'close-current-buffer)
(define-key my-keymap (kbd "<delete>") 'delete-char)
(define-key my-keymap (kbd "C-a") 'mark-whole-buffer)

;;; --------------------------------------------------
;;; OTHER COMMANDS

(define-key my-keymap (kbd "M-x") 'helm-M-x)
(define-key my-keymap (kbd "M-o") 'occur)
(define-key my-keymap (kbd "C-x C-l") 'toggle-truncate-lines)
(define-key my-keymap (kbd "C-x C-n") 'new-empty-buffer)

;; (global-set-key (kbd "C-x C-f") 'find-file-at-point)
(define-key my-keymap (kbd "M-m") 'highlight-symbol-at-point)
(define-key my-keymap (kbd "M-M") 'highlight-symbol-next)
(define-key my-keymap (kbd "M-C-M") 'highlight-symbol-prev)  ;; Might not work because C-m is always return

(define-key my-keymap (kbd "M-w") 'er/expand-region)

;; (global-set-key (kbd "C-M-o") 'sp-forward-sexp)
;; (global-set-key (kbd "C-M-u") 'sp-backward-sexp)

(define-key my-keymap (kbd "M-;") 'comment-or-uncomment-region-or-line)
(define-key my-keymap (kbd "M-[") 'string-inflection-all-cycle)

(define-key my-keymap (kbd "C-x C-f") 'helm-find-files)
(define-key my-keymap (kbd "C-x b") 'helm-mini)
(define-key my-keymap (kbd "C-c p h") 'helm-projectile)
(define-key my-keymap (kbd "C-c p g") 'helm-projectile-find-file-dwim)

(define-key my-keymap (kbd "C-8") 'whitespace-mode)
(define-key my-keymap (kbd "C-9") 'sr-speedbar-toggle)

;;; ------------------------------------------------------------------
;;; Mode for my keymap

(define-minor-mode my-keys-mode
  "Minor mode for my keybindings"
  nil
  :lighter "MKB"
  :global t
  :keymap my-keymap

  nil
  )

(my-keys-mode 1)


;;; ------------------------------------------------------------------
;;; Modify other modes

;; When exiting i-search with C-<return>, place cursor at the front
;; of the match.
(define-key isearch-mode-map [(control return)]
  #'isearch-exit-other-end)
(defun isearch-exit-other-end ()
  "Exit isearch, at the opposite end of the string."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))
