;; * SETUP PACKAGES *

;; General packages that pertain to Emacs in General, not to any specific
;; language or mode.

;; Functional macros a la underscore.js
(use-package dash
  :ensure t
  :config (dash-enable-font-lock))

;; Better string manipulation functions
(use-package s :ensure t)

;; better file path manipulation functions
(use-package f :ensure t)

;; Make fill column visible
(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-color "#111122"))

;; Expand snippets
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all))

;; Intelligent sexp editing. M-x customize-group smartparens
(use-package smartparens
  :ensure t
  :config (require 'smartparens-config))

;; Project-aware operations
;; All projectile mappings are under C-c p
(use-package projectile
  :ensure t
  :delight projectile-mode
  :config
  (projectile-global-mode))

;; IDO-like navigation
;; (use-package helm :ensure t
;;   :config
;;   (helm-mode 1)
;;   (use-package helm-ag :ensure t)
;;   (use-package helm-projectile :ensure t))

(use-package helm-projectile :ensure t
  :delight helm-mode)

(use-package ido :ensure t
  :config
  (ido-mode t)
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-default-buffer-method 'selected-window)
  (define-key ido-file-completion-map (kbd "<left>") #'ido-delete-backward-updir))

(alist-get 'left (cddr ido-completion-map))

;; Highlight escape sequences in strings
(use-package highlight-escape-sequences
  :ensure t
  :config
  (hes-mode)
  (put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face))

;; Better behaviour for popup windows
(use-package popwin
  :ensure t
  :config
  (setq display-buffer-function 'popwin:display-buffer)
  (push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
  (push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config))


;; Smart region expansion
(use-package expand-region
  :ensure t
  :config
  ;; Show expand-region command used
  (setq er--show-expansion-message t))

;; Writable grep buffers
(use-package wgrep :ensure t)

;; Represent undo-history as an actual tree (visualize with C-x u)
(use-package undo-tree
  :ensure t
  :delight " μ"
  :config
  (setq undo-tree-mode-lighter "")
  (global-undo-tree-mode)
  (defalias 'redo 'undo-tree-redo)
  :bind (("C-z" . undo)     ; Zap to character isn't helpful
         ("C-S-z" . redo)))

;; Duplicate line or region
(use-package duplicate-thing :ensure t)

(use-package which-key
  :ensure t
  :defer 10
  :delight which-key-mode
  :config

  ;; Replacements for how KEY is replaced when which-key displays
  ;;   KEY → FUNCTION
  ;; Eg: After "C-c", display "right → winner-redo" as "▶ → winner-redo"
  (setq which-key-key-replacement-alist
        '(("<\\([[:alnum:]-]+\\)>" . "\\1")
          ("left"                  . "◀")
          ("right"                 . "▶")
          ("up"                    . "▲")
          ("down"                  . "▼")
          ("delete"                . "DEL") ; delete key
          ("\\`DEL\\'"             . "BS") ; backspace key
          ("next"                  . "PgDn")
          ("prior"                 . "PgUp"))

        ;; List of "special" keys for which a KEY is displayed as just
        ;; K but with "inverted video" face... not sure I like this.
        which-key-special-keys '("RET" "DEL" ; delete key
                                 "ESC" "BS" ; backspace key
                                 "SPC" "TAB")

        ;; Replacements for how part or whole of FUNCTION is replaced:
        which-key-description-replacement-alist
        '(("Prefix Command" . "prefix")
          ("\\`calc-"       . "") ; Hide "calc-" prefixes when listing M-x calc keys
          ("\\`projectile-" . "𝓟/")
          ("\\`org-babel-"  . "ob/"))

        ;; Underlines commands to emphasize some functions:
        which-key-highlighted-command-list
        '("\\(rectangle-\\)\\|\\(-rectangle\\)"
          "\\`org-"))

  ;; Change what string to display for a given *complete* key binding
  ;; Eg: After "C-x", display "8 → +unicode" instead of "8 → +prefix"
  (which-key-add-key-based-replacements
    "C-x 8"   "unicode"
    "C-c T"   "toggles-"
    "C-c p s" "projectile-search"
    "C-c p 4" "projectile-other-buffer-"
    "C-x a"   "abbrev/expand"
    "C-x r"   "rect/reg"
    "C-c /"   "engine-mode-map"
    "C-c C-v" "org-babel")

  (which-key-mode 1))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

(use-package magit
  :ensure t
  :bind (("H-g" . magit-status)))


(use-package string-edit :ensure t)

;; Highlight Symbol
;; (use-package highlight-symbol
;;   :ensure t
;;   :init
;;   (progn
;;     (remove-hook 'prog-mode-hook
;;       (lambda ()
;; (highlight-symbol-mode)
;; (highlight-symbol-nav-mode)))
;;     (setq highlight-symbol-idle-delay 0.25)))

(use-package ace-jump-mode :ensure t
  :bind (("H-'" . ace-jump-mode)
         ("C-M-s-\"" . ace-jump-mode)
         ("H-," . ace-jump-mode-pop-mark)
         ("C-M-s-<" . ace-jump-mode-pop-mark))
  :config (ace-jump-mode-enable-mark-sync))

(use-package mastodon :ensure t
  :init
  (setq mastodon-instance-url
        "https://toot.cat"
        ;;"https://mastodon.social"
        ))

(use-package emojify :ensure t
  :config
  (global-emojify-mode))

(use-package edn :ensure t)

;(use-package dired+ :ensure t)

;(use-package so-long
;  :config
;  (so-long-enable))

(provide 'setup-packages)
