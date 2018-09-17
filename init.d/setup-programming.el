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


;(use-package paredit
;  :ensure t
 ; :delight " {}"
;  :init

 ; (dolist (m (list 'emacs-lisp-mode-hook 'lisp-interaction-mode-hook 'eval-expression-minibuffer-setup-hook 'ielm-mode-hook 'clojurescript-mode))
 ;   (add-hook m 'enable-paredit-mode))
 ;:bind
;  ( ("M-^" . paredit-delete-indentation)
                                        ; ("C-^" . paredit-remove-newlines)
                                        ;         ("C-c d" . paredit-forward-down)
                                        ;         ("C-M-f" . clojure-forward-logical-sexp)
                                        ;         ("C-M-b" . clojure-backward-logical-sexp)
                                        ;         ("C-M-{" . paredit-wrap-curly)
                                        ;        ("C-M-(" . paredit-wrap-round)
                                        ;        ("C-M-w" . sp-copy-sexp)

 ;  )
;  )

 ;; Useful key sequences for positioning cursor on particular s-expressions:

 ;;  - C-M- a d :: Move to beginning of function and inside the
 ;;      declaration. Good start to just about any other positioning.
 ;; - C-M- d f d :: At beginning of function, moves to first s-expression.

;; replacement for delete-indentation
 (defun smartparens-delete-indentation (&optional arg)
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

; While =M-SPC= (especially =M-0 M-SPC=) is good for cleaning up extra
;  white space on a single line, let's use this function to get rid of
;  it all.
 (defun smartparens-remove-newlines ()
      "Removes extras whitespace and newlines from the current point
    to the next parenthesis."
      (interactive)
      (let ((up-to (point))
            (from (re-search-forward "[])}]")))
         (backward-char)
         (while (> (point) up-to)
           (smartparens-delete-indentation))))

(use-package smartparens
  ;; :delight " {}"
  :bind (:map smartparens-mode-map
              ("M-^" . smartparens-delete-indentation)
              ("C-^" . smartparens-remove-newlines)
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-u" . sp-backward-up-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-p" . sp-backward-down-sexp)
              ("C-M-n" . sp-up-sexp)
              ("M-s" . sp-splice-sexp)
              ("C-M-<up>" . sp-splice-sexp-killing-backward)
              ("C-M-<down>" . sp-splice-sexp-killing-forward)
              ("C-M-r" . sp-splice-sexp-killing-around)
              ("C-)" . sp-forward-slurp-sexp)
              ("C-<right>" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-<left>" . sp-forward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-M-<left>" . sp-backward-slurp-sexp)
              ("C-{" . sp-backward-barf-sexp)
              ("C-M-<right>" . sp-backward-barf-sexp)
              ("M-S" . sp-split-sexp))
  :hook
  (after-init . smartparens-global-strict-mode)
  :config
  (require 'smartparens-config)
  ;; Org-mode config
  (sp-with-modes 'org-mode
    (sp-local-pair "'" nil :unless '(sp-point-after-word-p))
    (sp-local-pair "*" "*" :actions '(insert wrap) :unless
                   '(sp-point-after-word-p sp-point-at-bol-p)
                   :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
    (sp-local-pair "_" "_" :unless '(sp-point-after-word-p))
    (sp-local-pair "/" "/" :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "~" "~" :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "=" "=" :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "«" "»"))

  (defun sp--org-skip-asterisk (ms mb me)
    (or (and (= (line-beginning-position) mb)
             (eq 32 (char-after (1+ mb))))
        (and (= (1+ (line-beginning-position)) me)
             (eq 32 (char-after me))))))


(use-package rainbow-delimiters
    :ensure t
    :config
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    :delight ("rainbow-mode" "🌈"))
; keep things indented correctly
(use-package aggressive-indent
      :ensure t)
; expand parenthesis ??
  (add-hook 'prog-mode-hook 'electric-pair-mode)
;   Always show matching parenthesis.
 (show-paren-mode 1)
     (setq show-paren-delay 0)


(provide 'setup-programming)
