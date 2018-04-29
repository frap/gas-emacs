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

; AUTO COMPLETION
(use-package company
;  :init (global-company-mode)
  :config
  (progn
    (defun indent-or-complete ()
      (interactive)
      (if (looking-at "\\_>")
          (company-complete-common)
        (indent-according-to-mode)))

    (global-set-key "\t" 'indent-or-complete)))


(use-package command-log-mode
  :ensure t)

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

(provide 'setup-programming)
