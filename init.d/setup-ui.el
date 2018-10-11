;;(set-default-font "InputSerifNarrow-12")
;;(when (member "Input" (font-family-list)) (set-frame-font "InputSerifNarrow-12" t t))
                                        ;(when (member "Menlo" (font-family-list)) (set-frame-font "Menlo-14" t t))
;(when (window-system)
;  (set-frame-font "Hack-13" t t));; was "Fira Code"
                                        ;(when (member "Inconsolata" (font-family-list)) (set-frame-font "Inconsolata-17" t t))
;;(set-frame-font "Anonymous Pro-14")

                                        ;(set-frame-parameter nil 'left-fringe 18)
(defun set-icon-fonts (CODE-FONT-ALIST)
  "Utility to associate many unicode points with specified fonts."
  (--each CODE-FONT-ALIST
    (-let (((font . codes) it))
      (--each codes
        (set-fontset-font t `(,it . ,it) font)))))

(defun load-default-fonts ()

  (set-fontset-font "fontset-default" 'unicode "Emoji One Color")
  (set-face-font 'default "Hack-13")

  ;; The icons you see are not the correct icons until this is evaluated!
  (set-icon-fonts
   '(("fontawesome"
      ;;                         
      #xf07c #xf0c9 #xf0c4 #xf0cb #xf017 #xf101)

     ("all-the-icons"
      ;;    
      #xe907 #xe928)

     ("github-octicons"
      ;;                        
      #xf091 #xf059 #xf076 #xf075 #xf016 #xf00a)

     ("Symbola"
      ;; 𝕊    ⨂      ∅      ⟻    ⟼     ⊙      𝕋       𝔽
      #x1d54a #x2a02 #x2205 #x27fb #x27fc #x2299 #x1d54b #x1d53d
      ;; 𝔹    𝔇       𝔗
      #x1d539 #x1d507 #x1d517))))


(defun load-fonts (frame)
  (select-frame frame)
  (load-default-fonts))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-fonts)
  (load-default-fonts))
;; Emoji! ð
;;(set-fontset-font t 'unicode "Symbola" nil 'prepend)
(set-fontset-font t 'unicode "Emoji One Color" nil 'prepend)

(use-package gruvbox-theme
;  :disabled
  :init
  (load-theme 'gruvbox t))

(use-package farmhouse-theme
  :ensure t
  :disabled
  :init
  (load-theme 'farmhouse-dark t))

(use-package kaolin-themes
  :ensure t
 ; :disabled
  :init
  (load-theme 'kaolin-aurora t)
  :config
;  (use-package color-identifiers-mode)
)

(use-package panda-theme
  :ensure t
  :disabled
  :config
  (load-theme 'panda t))

(use-package dracula-theme
  :disabled
  :ensure t
  :config
  (load-theme 'dracula t))

(use-package color-theme-sanityinc-solarized
  :ensure t
  :disabled
  :config
  (load-theme 'sanityinc-solarized-light)
  )

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :disabled
  :config
  (load-theme 'sanityinc-tomorrow-blue))


(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode)
  (let ((alist '((33 . ".\\(?:\\(?:==\\)\\|[!=]\\)")
                 (35 . ".\\(?:[(?[_{]\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|\\+\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (58 . ".\\(?:[:=]\\)")
                 (59 . ".\\(?:;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:[:=?]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:[=@~-]\\)"))))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))

(setq fast-but-imprecise-scrolling t)

;; Buffer settings
(setq default-indicate-empty-lines t)
(setq require-final-newline t)
(setq show-trailing-whitespace t)



;; smart-line mode for configurable abbrev of directories
(use-package smart-mode-line
  :defer 2
  :config
  (sml/setup)
  :custom
  (sml/theme 'dark)
  (sml/replacer-regexp-list
   '(("^~/\\.emacs\\.d/elpa/"                            ":ELPA:")
     ("^~/\\.emacs\\.d/"                                 ":ED:")
     ("^/sudo:.*:"                                       ":SU:")
     ("^~/Documents/"                                    ":Doc:")
     ("^:\\([^:]*\\):Documento?s/"                       ":\\1/Doc:")
     ("^~/Dropbox/"                                      ":DB:")
     ("^:DB:GTD"                                         ":GTD:")
     ("^:DB:Technical/"                                  ":Tech:")
     ("^:P:Dev/"                                         ":Dev:")
     )))


;; Every time a window is started, make sure it get maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package emojify
  :ensure t
  :config

  (use-package company-emoji
    :ensure t)

  (setq emojify-user-emojis
        '(("(heart)" . (("name" . "Heart")
                        ("image" . "~/.emacs.d/emojis/emojione-v2.2.6-22/2665.png")
                        ("style" . "github")))))

  ;; If emojify is already loaded refresh emoji data
  (when (featurep 'emojify)
    (emojify-set-emoji-data)))



(use-package flycheck-status-emoji
  :ensure t
  :after emojify)

(provide 'setup-ui)
