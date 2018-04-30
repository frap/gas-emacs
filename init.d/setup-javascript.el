(use-package js2-mode
  :ensure t
  :delight "☕🌶"
  :config
  (add-to-list 'auto-mode-alist '("\\.es6$" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . js2-mode)))

(provide 'setup-javascript)
