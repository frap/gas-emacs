;; Packages that pertain to specific modes or languages, and that don't have
;; their own setup-*.el

;; Discoverability

(use-package company
  :ensure t
  :init
  (setq company-dabbrev-ignore-case t
        company-show-numbers t)
  (add-hook 'after-init-hook 'global-company-mode)
  :config
;;       (add-to-list 'company-backends 'company-math-symbols-unicode)
  :bind ("C-:" . company-complete)  ; In case I don't want to wait
  :delight company-mode)


;(use-package sql-interactive-mode
;  :init
;  (setq sql-prompt-regexp "^[_[:alnum:]]*[=][#>] ")
;  (setq sql-prompt-cont-regexp "^[_[:alnum:]]*[-][#>] "))

;(use-package sql-indent :ensure t)

(use-package yaml-mode :ensure t)
(use-package feature-mode :ensure t)

;(use-package elixir-mode :ensure t)

(use-package restclient
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rest$" . restclient-mode)))



(use-package terraform-mode
   :defer t
   :init
    ; (progn
    ;   (require 'company-terraform)
    ;   (company-terraform-init)
    ;  )
   :config (setq terraform-indent-level 2)
)

(provide 'setup-mode-packages)
