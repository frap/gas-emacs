;; Packages that pertain to specific modes or languages, and that don't have
;; their own setup-*.el
;; Timestamp: <>

;; linum-mode has performance issues ith large files
(when (>= emacs-major-version 26)
  (use-package display-line-numbers
    :disabled
    :defer nil
    :ensure t
    :config
    (global-display-line-numbers-mode)))

;; show traiiling whitesdapce in red
(customize-set-variable 'show-trailing-whitespace t)
;; dont use hard-tabs
(customize-set-variable 'indent-tabs-mode nil)


(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package flyspell
  :ensure t
  :config

  ;; Set programms
  (setq-default ispell-program-name "aspell")
  (setq-default ispell-list-command "--list")

  ;; Refresh flyspell after directory change
  (defun flyspell-buffer-after-pdict-save (&rest _)
    (flyspell-buffer))
  (advice-add 'ispell-pdict-save :after #'flyspell-buffer-after-pdict-save)

  ;; Popup
  (defun flyspell-emacs-popup-textual (event poss word)
    "A textual flyspell popup menu."
    (require 'popup)
    (let* ((corrects (if flyspell-sort-corrections
                         (sort (car (cdr (cdr poss))) 'string<)
                       (car (cdr (cdr poss)))))
           (cor-menu (if (consp corrects)
                         (mapcar (lambda (correct)
                                   (list correct correct))
                                 corrects)
                       '()))
           (affix (car (cdr (cdr (cdr poss)))))
           show-affix-info
           (base-menu  (let ((save (if (and (consp affix) show-affix-info)
                                       (list
                                        (list (concat "Save affix: " (car affix))
                                              'save)
                                        '("Accept (session)" session)
                                        '("Accept (buffer)" buffer))
                                     '(("Save word" save)
                                       ("Accept (session)" session)
                                       ("Accept (buffer)" buffer)))))
                         (if (consp cor-menu)
                             (append cor-menu (cons "" save))
                           save)))
           (menu (mapcar
                  (lambda (arg) (if (consp arg) (car arg) arg))
                  base-menu)))
      (cadr (assoc (popup-menu* menu :scroll-bar t) base-menu))))


  (defun flyspell-emacs-popup-choose (org-fun event poss word)
    (if (window-system)
        (funcall org-fun event poss word)
      (flyspell-emacs-popup-textual event poss word)))

  (eval-after-load "flyspell"
    '(progn
       (advice-add 'flyspell-emacs-popup :around #'flyspell-emacs-popup-choose)))
  )

(use-package fic-mode
  :commands fic-mode
  :ensure t
  :init (add-hook 'prog-mode-hook 'fic-mode)
  :config

  (defun fic-view-listing ()
    "Use occur to list related FIXME keywords"
    (interactive)
    (occur "\\<\\(FIXME\\|WRITEME\\|WRITEME!\\|TODO\\|BUG\\):?"))
  )

;; Unify the buffer name style
(eval-after-load "uniquify"
  '(progn
     (setq uniquify-buffer-name-style 'forward)))

(provide 'setup-wordsmithing)
