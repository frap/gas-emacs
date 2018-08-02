;; String manipulation routines for emacs lisp
(use-package s
  :ensure t)

;; Hydras are the most awesome thing in the world. Check out the project page (https://github.com/abo-abo/hydra) for some great examples.
(use-package hydra
  :ensure t)

; Code Folding
(use-package hideshow
  :ensure t
  :bind (("C->" . my-toggle-hideshow-all)
         ("C-<" . hs-hide-level)
         ("C-;" . hs-toggle-hiding))
  :config
  ;; Hide the comments too when you do a 'hs-hide-all'
  (setq hs-hide-comments nil)
  ;; Set whether isearch opens folded comments, code, or both
  ;; where x is code, comments, t (both), or nil (neither)
  (setq hs-isearch-open 'x)
  ;; Add more here


  (setq hs-set-up-overlay
        (defun my-display-code-line-counts (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (overlay-put ov 'display
                         (propertize
                          (format " ... <%d>"
                                  (count-lines (overlay-start ov)
                                               (overlay-end ov)))
                          'face 'font-lock-type-face)))))

  (defvar my-hs-hide nil "Current state of hideshow for toggling all.")
       ;;;###autoload
  (defun my-toggle-hideshow-all () "Toggle hideshow all."
         (interactive)
         (setq my-hs-hide (not my-hs-hide))
         (if my-hs-hide
             (hs-hide-all)
           (hs-show-all)))

  (add-hook 'prog-mode-hook (lambda ()
                              (hs-minor-mode 1)
                              ))
  (add-hook 'clojure-mode-hook (lambda ()
                              (hs-minor-mode 1)
                              ))
  )

; VERSION CONTROL
;; Magit is the only thing you need when it comes to Version Control (Git)
(use-package magit
  :ensure t
  :bind (("C-c m" . magit-status)))


(defun live-coding ()
  (interactive)
  (set-face-attribute 'default nil :font "Hack-18")
  (add-hook 'prog-mode-hook 'command-log-mode)
  ;;(add-hook 'prog-mode-hook (lambda () (focus-mode 1)))
  )

(defun normal-coding ()
  (interactive)
  (set-face-attribute 'default nil :font "Hack-14")
  (add-hook 'prog-mode-hook 'command-log-mode)
  ;;(add-hook 'prog-mode-hook (lambda () (focus-mode 1)))
  )


(defun paredit-delete-indentation (&optional arg)
  "Handle joining lines that end in a comment."
  (interactive "*P")
  (let (comt)
    (save-excursion
      (move-beginning-of-line (if arg 1 0))
      (when (skip-syntax-forward "^<" (point-at-eol))
        (setq comt (delete-and-extract-region (point) (point-at-eol)))))
    (delete-indentation arg)
    (when comt
      (save-excursion
        (move-end-of-line 1)
        (insert " ")
        (insert comt)))))


(defun paredit-remove-newlines ()
  "Removes extras whitespace and newlines from the current point
    to the next parenthesis."
  (interactive)
  (let ((up-to (point))
        (from (re-search-forward "[])}]")))
    (backward-char)
    (while (> (point) up-to)
      (paredit-delete-indentation))))

(use-package paredit
  :ensure t
  :delight " {}"
  :init
  (dolist (m (list 'emacs-lisp-mode-hook 'lisp-interaction-mode-hook 'eval-expression-minibuffer-setup-hook 'ielm-mode-hook))
    (add-hook m 'enable-paredit-mode))
  :bind ( ("M-^" . paredit-delete-indentation)
          ("C-^" . paredit-remove-newlines)
;         ("C-c d" . paredit-forward-down)
;         ("C-M-f" . clojure-forward-logical-sexp)
;         ("C-M-b" . clojure-backward-logical-sexp)
;         ("C-M-{" . paredit-wrap-curly)
 ;        ("C-M-(" . paredit-wrap-round)
 ;        ("C-M-w" . sp-copy-sexp)
           )
  )

 ;; Useful key sequences for positioning cursor on particular s-expressions:

 ;;  - C-M- a d :: Move to beginning of function and inside the
 ;;      declaration. Good start to just about any other positioning.
 ;; - C-M- d f d :: At beginning of function, moves to first s-expression.

(provide 'setup-programming)
