;;; Modes ;;;;

(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)

(use-package paredit
  :ensure t
  :delight "﹙﹚"
  :init
  (dolist (m (list 'emacs-lisp-mode-hook 'lisp-interaction-mode-hook 'eval-expression-minibuffer-setup-hook 'ielm-mode-hook))
    (add-hook m 'enable-paredit-mode))
  :bind (("C-c d" . paredit-forward-down)
         ("C-M-f" . clojure-forward-logical-sexp)
         ("C-M-b" . clojure-backward-logical-sexp)
         ("C-M-{" . paredit-wrap-curly)
         ("C-M-(" . paredit-wrap-round)
         ("C-M-w" . sp-copy-sexp))
)

(use-package elisp-mode
  :delight (emacs-lisp-mode "εl" :major)
  :bind (("C-C C-r" . eval-region))
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
)

;; Flash sexps when evaluating them
 (use-package eval-sexp-fu :ensure t)

(use-package highlight-parentheses
  :ensure t
  :delight highlight-parentheses-mode
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda()
              (highlight-parentheses-mode)
              )))


(use-package rainbow-delimiters
    :ensure t
    :config
    (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    :delight ("rainbow-mode" ""))

(global-highlight-parentheses-mode)

;(add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)

(provide 'setup-elisp)
