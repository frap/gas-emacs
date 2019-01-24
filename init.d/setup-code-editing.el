;; Timestamp: <>
(use-package eldoc
  :delight
  :hook
  (prog-mode       . turn-on-eldoc-mode)
  (cider-repl-mode . turn-on-eldoc-mode))

(use-package ediff
  :config
  (autoload 'diff-mode "diff-mode" "Diff major mode" t)
  (setq diff-switches "-u"
        ediff-auto-refine-limit (* 2 14000)
        ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function
        (lambda (&optional arg)
          (if (> (frame-width) 160)
              (split-window-horizontally arg)
            (split-window-vertically arg)))))

(use-package company
  :ensure t
  :init
  (setq company-dabbrev-ignore-case t
        company-show-numbers t
        company-idle-delay 0.5
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t)
  :config
  (define-key company-mode-map [remap hippie-expand] 'company-complete)
  (define-key company-active-map [remap hippie-expand] 'company-complete)
  :hook
  (after-init . global-company-mode)
;;       (add-to-list 'company-backends 'company-math-symbols-unicode)
;;  :bind ("C-:" . company-complete)  ; In case I don't want to wait
  :delight company-mode)

;; Expand snippets
(use-package yasnippet
  :defer 5
  :ensure t
  :delight "γ"
 :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))

;; Use only own snippets, do not use bundled ones
;;(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(use-package yasnippet-snippets
  :ensure t
  )

;; Jump to end of snippet definition
(define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)

;; Inter-field navigation
(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
        (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

(define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
(define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)

(defun autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

;; No dropdowns please, yas
(setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))

;; No need to be so verbose
(setq yas-verbosity 1)

;; Wrap around region
(setq yas-wrap-around-region t)

(use-package autoinsert
  :init
  ;; Don't want to be prompted before insertion:
  (setq auto-insert-query nil)

  (setq auto-insert-directory (locate-user-emacs-file "templates"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)

  :config
  (define-auto-insert "\\.el$" ["default-lisp.el" autoinsert-yas-expand])
  (define-auto-insert "\\.sh$" ["default-sh.sh" autoinsert-yas-expand])
  (define-auto-insert "/bin/"  ["default-sh.sh" autoinsert-yas-expand])
  (define-auto-insert "\\.mk$" ["default-makefile.mk" autoinsert-yas-expand])
  (define-auto-insert "Makefile" ["default-makefile.mk" autoinsert-yas-expand])
  (define-auto-insert "\\.html?$" ["default-html.html" autoinsert-yas-expand])
  )

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
  :delight "{}"
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

;; when you hit Ctrl-:, all occurrences of the symbol under the cursor (or the current selection) are highlighted, and any changes you make on one of them will be automatically applied to all others.
(use-package iedit
  :config (set-face-background 'iedit-occurrence "Magenta"))

; keep things indented correctly
(use-package aggressive-indent
  :ensure t
  :delight aggressive-indent-mode
  :config
  (global-aggressive-indent-mode)
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'web-mode))

; expand parenthesis ??
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'column-number-mode)    ; show column numbers
(add-hook 'prog-mode-hook 'eldoc-mode)            ; always use eldoc

;   Always show matching parenthesis.
(show-paren-mode 1)
(setq show-paren-delay 0)


(provide 'setup-code-editing)
