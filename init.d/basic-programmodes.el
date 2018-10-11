;; Timestamp: <>

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
  :bind (("C-c m" . magit-status))
  :config

  ;; Ignore recent commit
  (setq magit-status-sections-hook
        '(magit-insert-status-headers
          magit-insert-merge-log
          magit-insert-rebase-sequence
          magit-insert-am-sequence
          magit-insert-sequencer-sequence
          magit-insert-bisect-output
          magit-insert-bisect-rest
          magit-insert-bisect-log
          magit-insert-untracked-files
          magit-insert-unstaged-changes
          magit-insert-staged-changes
          magit-insert-stashes
          magit-insert-unpulled-from-upstream
          magit-insert-unpulled-from-pushremote
          magit-insert-unpushed-to-upstream
          magit-insert-unpushed-to-pushremote))


  ;; Update visualization
  (setq pretty-magit-alist nil
        pretty-magit-prompt nil)

  (defmacro pretty-magit (WORD ICON PROPS &optional NO-PROMPT?)
    "Replace sanitized WORD with ICON, PROPS and by default add to prompts."
    `(prog1
         (add-to-list 'pretty-magit-alist
                      (list (rx bow (group ,WORD (eval (if ,NO-PROMPT? "" ":"))))
                            ,ICON ',PROPS))
       (unless ,NO-PROMPT?
         (add-to-list 'pretty-magit-prompt (concat ,WORD ": ")))))

  ;; Operations
  (pretty-magit "add"   ? (:foreground "#375E97" :height 1.2) pretty-magit-prompt)
  (pretty-magit "fix"   ? (:foreground "#FB6542" :height 1.2) pretty-magit-prompt)
  (pretty-magit "clean" ? (:foreground "#FFBB00" :height 1.2) pretty-magit-prompt)
  (pretty-magit "doc."  ? (:foreground "#3F681C" :height 1.2) pretty-magit-prompt)

  ;; Meta information
  (pretty-magit "master"  ? (:box nil :height 1.2) t)
  (pretty-magit "origin"  ? (:box nil :height 1.2) t)
  (pretty-magit "upstream"  ? (:box nil :height 1.2) t)

  (defun add-magit-faces ()
    "Add face properties and compose symbols for buffer from pretty-magit."
    (interactive)
    (with-silent-modifications
      (--each pretty-magit-alist
        (-let (((rgx icon props) it))
          (save-excursion
            (goto-char (point-min))
            (while (search-forward-regexp rgx nil t)
              (compose-region
               (match-beginning 1) (match-end 1) icon)
              (when props
                (add-face-text-property
                 (match-beginning 1) (match-end 1) props))))))))

  (advice-add 'magit-status :after 'add-magit-faces)
  (advice-add 'magit-refresh-buffer :after 'add-magit-faces)


  ;; Opening repo externally
  (defun parse-url (url)
    "convert a git remote location as a HTTP URL"
    (if (string-match "^http" url)
        url
      (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                                "https://\\2/\\3"
                                url)))
  (defun magit-open-repo ()
    "open remote repo URL"
    (interactive)
    (let ((url (magit-get "remote" "origin" "url")))
      (progn
        (browse-url (parse-url url))
        (message "opening repo %s" url))))

  (add-hook 'magit-mode-hook
            (lambda ()
              (local-set-key (kbd "o") 'magit-open-repo))))



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


;; Timestamp: <>

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

(use-package fish-mode
  :mode "\\.fish\\'"
  :interpreter "fish")


(use-package nix-mode)

(provide 'basic-programmodes)
