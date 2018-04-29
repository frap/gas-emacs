
(use-package clojure-snippets
  :ensure t)


(use-package clojure-mode
  :ensure t
  :init
  (defconst clojure--prettify-symbols-alist
    '(("fn"   . ?λ)
      ("__"   . ?⁈)))

  :config
  (add-hook 'clojure-mode-hook 'global-prettify-symbols-mode)
  :bind (("C-c d f" . cider-code)
         ("C-c d g" . cider-grimoire)
         ("C-c d w" . cider-grimoire-web)
         ("C-c d c" . clojure-cheatsheet)
;         ("C-c d d" . dash-at-point)
))



;(use-package clj-refactor
;  :ensure t
;  :init
;  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
;  :config
  ;; Configure the Clojure Refactoring prefix:
 ; (cljr-add-keybindings-with-prefix "C-c .")
 ; :diminish clj-refactor-mode)

(provide 'init-clojure)
