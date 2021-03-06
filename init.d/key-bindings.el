;; Timestamp: <>
;; * CUSTOM KEYBINDINGS *
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs built-in
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none)
  (setq mac-option-modifier 'meta)   ;; make option as meta key!
  (setq mac-command-modifier 'super) ;; make command key do super
  (setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
  )

(bind-key "M-g" 'goto-line)
(bind-key "M-`" 'other-frame)
;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)

;; From Pragmatic Emacs a more concise way to kill the buffer.
;;;(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "H-k") 'kill-this-buffer)

(global-set-key (kbd "H-c") 'comment-region)
(global-set-key (kbd "H-u") 'uncomment-region)

(global-set-key (kbd "C-c C-k") 'eval-buffer) ;; mimics CIDER binding

;(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provided by packages

;; Hyper!
(global-set-key (kbd "H-]") 'er/expand-region)
(global-set-key (kbd "H-[") 'er/contract-region)

;(global-set-key (kbd "H-g") 'magit-status)
;(global-set-key (kbd "C-M-s-g") 'magit-status)
;(global-set-key (kbd "H-b") 'magit-blame)
;(global-set-key (kbd "C-M-s-b") 'magit-blame)

(global-set-key (kbd "H-s") 'helm-projectile-ag) ; search in project on steroids
;(global-set-key (kbd "C-M-s-s") 'helm-projectile-ag) ; until I figure out how to have my ergodox send hyper



;; Alternative to C-c p, but keeping the original binding as well
(define-key projectile-mode-map (kbd "H-p") 'projectile-command-map)

;; aliases
(global-set-key (kbd "M-t") 'helm-projectile-find-file) ;; default: C-c p f
(global-set-key (kbd "C-M-<backspace>") 'sp-backward-kill-sexp)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)

;; Not having this work everywhere is a major source of frustration
(global-set-key (kbd "C-M-k") #'sp-kill-sexp)
(global-set-key (kbd "C-M-w") #'sp-copy-sexp)

(global-set-key (kbd "s-'") #'hs-toggle-hiding)
(global-set-key (kbd "M-:") #'eval-expression)

(global-set-key (kbd "H-y") (lambda () (interactive) (insert (pop kill-ring-yank-pointer))))
(global-set-key (kbd "H-Y") (lambda ()
                              (interactive)
                              (insert (pop kill-ring-yank-pointer))
                              (insert "\n") (recenter 20)))
(global-set-key (kbd "H-j") (lambda () (interactive) (insert "\n") (recenter 20)))
;; (global-set-key (kbd "C-x C-f") #'ido-find-file)

;;;; Global keybindings ;;;;

(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)

(global-set-key (kbd "s-z") 'undo)

(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-w") 'delete-window)
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)

(global-set-key (kbd "C-c d") #'duplicate-thing)


(defun zz/goto-match-paren (arg)
  "Go to the matching paren/bracket, otherwise (or if ARG is not nil) insert %.
  vi style of % jumping to matching brace."
  (interactive "p")
  (if (not (memq last-command '(set-mark
                                cua-set-mark
                                zz/goto-match-paren
                                down-list
                                up-list
                                end-of-defun
                                beginning-of-defun
                                backward-sexp
                                forward-sexp
                                backward-up-list
                                forward-paragraph
                                backward-paragraph
                                end-of-buffer
                                beginning-of-buffer
                                backward-word
                                forward-word
                                mwheel-scroll
                                backward-word
                                forward-word
                                mouse-start-secondary
                                mouse-yank-secondary
                                mouse-secondary-save-then-kill
                                move-end-of-line
                                move-beginning-of-line
                                backward-char
                                forward-char
                                scroll-up
                                scroll-down
                                scroll-left
                                scroll-right
                                mouse-set-point
                                next-buffer
                                previous-buffer
                                previous-line
                                next-line
                                back-to-indentation
                                )))
      (self-insert-command (or arg 1))
    (cond ((looking-at "\\s\(") (sp-forward-sexp) (backward-char 1))
          ((looking-at "\\s\)") (forward-char 1) (sp-backward-sexp))
          (t (self-insert-command (or arg 1))))))

(bind-key "%" 'zz/goto-match-paren)

(provide 'key-bindings)
