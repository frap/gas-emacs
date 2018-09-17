;;; Modes ;;;;

(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

(use-package elisp-mode
  :delight (emacs-lisp-mode "ξλ " :major)
  :bind (("C-C C-r" . eval-region))
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
)

;; Flash sexps when evaluating them
 (use-package eval-sexp-fu :ensure t)

(use-package highlight-parentheses
  :ensure t
  :delight "🔦";highlight-parentheses-mode
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda()
              (highlight-parentheses-mode)
              )))


(global-highlight-parentheses-mode)

(add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)

(provide 'setup-elisp)
