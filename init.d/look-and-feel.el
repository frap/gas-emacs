;;(set-default-font "InputSerifNarrow-12")
;;(when (member "Input" (font-family-list)) (set-frame-font "InputSerifNarrow-12" t t))
                                        ;(when (member "Menlo" (font-family-list)) (set-frame-font "Menlo-14" t t))
(when (window-system)
  (set-frame-font "Hack-13" t t));; was "Fira Code"
                                        ;(when (member "Inconsolata" (font-family-list)) (set-frame-font "Inconsolata-17" t t))
;;(set-frame-font "Anonymous Pro-14")

                                        ;(set-frame-parameter nil 'left-fringe 18)

;; Emoji! ð
;;(set-fontset-font t 'unicode "Symbola" nil 'prepend)
(set-fontset-font t 'unicode "Emoji One Color" nil 'prepend)

(use-package farmhouse-theme
  :ensure t
  :disabled
  :init
  (load-theme 'farmhouse-dark t))

(use-package kaolin-themes
  :ensure t
 ; :disabled
  :init
  (load-theme 'kaolin-aurora t)
  :config
;  (use-package color-identifiers-mode)
)

(use-package panda-theme
  :ensure t
  :disabled
  :config
  (load-theme 'panda t))

(use-package dracula-theme
  :disabled
  :ensure t
  :config
  (load-theme 'dracula t))

(use-package color-theme-sanityinc-solarized
  :ensure t
  :disabled
  :config
  (load-theme 'sanityinc-solarized-light)
  )

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :disabled
  :config
  (load-theme 'sanityinc-tomorrow-blue))


;; Set theme & font size
;(add-to-list 'custom-theme-load-path "~/.emacs.d/lib/monokai-theme")
;(load-theme 'monokai t)



;; (add-hook 'prog-mode-hook
;;   (lambda ()
;;     ;; Show line number
;;     ;; (linum-mode 1)
;;     (hl-line-mode 1)))
(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode)
  (let ((alist '((33 . ".\\(?:\\(?:==\\)\\|[!=]\\)")
                 (35 . ".\\(?:[(?[_{]\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|\\+\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (58 . ".\\(?:[:=]\\)")
                 (59 . ".\\(?:;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:[:=?]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:[=@~-]\\)"))))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))

(setq fast-but-imprecise-scrolling t)

;; Buffer settings
(setq default-indicate-empty-lines t)
(setq require-final-newline t)
(setq show-trailing-whitespace t)


;; Every time a window is started, make sure it get maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; (use-package outshine
;;   :ensure t
;;   :config
;;   (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
;;   (add-hook 'prog-mode-hook 'outline-minor-mode)
;; )

;; (defun -add-font-lock-kwds (FONT-LOCK-ALIST)
;;   (font-lock-add-keywords
;;    nil (--map (-let (((rgx uni-point) it))
;;                 `(,rgx (0 (progn
;;                             (compose-region (match-beginning 1) (match-end 1)
;;                                             ,(concat "\t" (list uni-point)))
;;                             nil))))
;;               FONT-LOCK-ALIST)))

;; (defmacro add-font-locks (FONT-LOCK-HOOKS-ALIST)
;;   `(--each ,FONT-LOCK-HOOKS-ALIST
;;      (-let (((font-locks . mode-hooks) it))
;;        (--each mode-hooks
;;          (add-hook it (-partial '-add-font-lock-kwds
;;                                 (symbol-value font-locks)))))))

;; (defconst emacs-outlines-font-lock-alist
;;   ;; Outlines
;;   '(("\\(^;;;\\) "          ?■)
;;     ("\\(^;;;;\\) "         ?○)
;;     ("\\(^;;;;;\\) "        ?✸)
;;     ("\\(^;;;;;;\\) "       ?✿)))

;; (defconst lisp-outlines-font-lock-alist
;;   ;; Outlines
;;   '(("\\(^;; \\*\\) "          ?■)
;;     ("\\(^;; \\*\\*\\) "       ?○)
;;     ("\\(^;; \\*\\*\\*\\) "    ?✸)
;;     ("\\(^;; \\*\\*\\*\\*\\) " ?✿)))

;; (defconst python-outlines-font-lock-alist
;;   '(("\\(^# \\*\\) "          ?■)
;;     ("\\(^# \\*\\*\\) "       ?○)
;;     ("\\(^# \\*\\*\\*\\) "    ?✸)
;;     ("\\(^# \\*\\*\\*\\*\\) " ?✿)))

;; (add-font-locks
;;  '((emacs-outlines-font-lock-alist emacs-lisp-mode-hook)
;;    (lisp-outlines-font-lock-alist clojure-mode-hook hy-mode-hook)
;;    (python-outlines-font-lock-alist python-mode-hook)))

(provide 'look-and-feel)
