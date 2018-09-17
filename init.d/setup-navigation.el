;; use ace-window
 (use-package ace-window
      :ensure t
      :init
        (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?o))
        (global-set-key (kbd "C-x o") 'ace-window)
      :delight ace-window-mode)

;; I like =IDO= for switching buffers since I typically know what I'm after
(global-set-key (kbd "<f8>") 'ido-switch-buffer)
(global-set-key (kbd "S-<f8>") 'ibuffer)

;; better jumping using avy-goto-word-1 bound to C-c j
(use-package avy
  :ensure t
  :init (setq avy-background t))
; bind doesnt work on use-package still ?
(global-set-key (kbd "s-h") 'avy-goto-char-timer)
(global-set-key (kbd "s-j") 'avy-goto-char-timer)
(global-set-key (kbd "s-H") 'avy-pop-mark)
(global-set-key (kbd "s-J") 'avy-pop-mark)
(global-set-key (kbd "A-h") 'avy-goto-char-timer)
(global-set-key (kbd "A-j") 'avy-goto-char-timer)
(global-set-key (kbd "A-H") 'avy-pop-mark)
(global-set-key (kbd "A-J") 'avy-pop-mark)

; IDO (Intercatively do things)
 (use-package ido
       :ensure t
       :init  (setq ido-enable-flex-matching t
                    ido-ignore-extensions t
                    ido-use-virtual-buffers t
                    ido-everywhere t)
       :config
       (ido-mode 1)
       (ido-everywhere 1)
       (add-to-list 'completion-ignored-extensions ".pyc"))

;; add to ido the flx package
 (use-package flx-ido
        :ensure t
        :init (setq ido-enable-flex-matching t
                    ido-use-faces nil)
        :config (flx-ido-mode 1))

;; and ido vertically
 (use-package ido-vertical-mode
       :ensure t
       :init               ; I like up and down arrow keys:
       (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
       :config
       (ido-vertical-mode 1))

; sorts ido file by mtime
(defun ido-sort-mtime ()
  "Reorder the IDO file list to sort from most recently modified."
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (ignore-errors
                  (time-less-p
                   (sixth (file-attributes (concat ido-current-directory b)))
                   (sixth (file-attributes (concat ido-current-directory a))))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (and (char-equal (string-to-char x) ?.) x))
              ido-temp-list))))

(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)


;; tramp and root files
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (let* ((file-name (buffer-file-name))
           (file-root (if (string-match "/ssh:\\([^:]+\\):\\(.*\\)" file-name)
                          (concat "/ssh:"  (match-string 1 file-name)
                                  "|sudo:" (match-string 1 file-name)
                                  ":"      (match-string 2 file-name))
                        (concat "/sudo:localhost:" file-name))))
      (find-alternate-file file-root))))

; smex built using IDO to do similar with M-x commands
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex)
  ("M-X" . smex-major-mode-commands))

;; helm project can be intrusive so use its features with C-x c
(use-package helm
       :ensure t
       :init
       (use-package helm-config))

;The [[helm-swoop]] project has a nice DWIM replacement for isearch-forward-symbol-at-point (bound to M-s .):
(use-package helm-swoop
       :ensure t
       :init
       ;; If this value is t, split window inside the current window
       (setq helm-swoop-split-with-multiple-windows t
             ;; If you prefer fuzzy matching
             helm-swoop-use-fuzzy-match t)
       :bind
       (("M-s s" . helm-swoop)  ;; overbind M-i ?
        ("M-s S" . helm-multi-swoop)))

(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)


;; Silver searcher (ag)
(use-package ag
  :ensure    t
  :commands  ag
  :init      (setq ag-highlight-search t)
  :config    (add-to-list 'ag-arguments "--word-regexp"))

;; use beacon mode for cursor highlighting
(use-package beacon
  :ensure t
  :config
  (beacon-mode t))

(provide 'setup-navigation)
