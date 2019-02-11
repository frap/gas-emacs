;; =======
;; VISUALS
;; Enable transparent title bar on macOS
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; {light, dark}
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; Font
(when (member "Hack" (font-family-list))
  (set-face-attribute 'default nil :font "Hack 13"))

                                        ;(if (member "Monaco" (font-family-list))
                                        ;(set-face-attribute 'default nil :font "Monaco 16"))
(setq-default line-spacing 2)


;; Nice and simple default light theme.
;;(load-theme 'tsdh-light)
(load-theme 'wombat)

;; Pretty icons
(use-package all-the-icons)
;; MUST DO M-x all-the-icons-install-fonts after

;(when (window-system)
;  (set-frame-font "Hack-13" t t));; was "Fira Code"
                                        ;(when (member "Inconsolata" (font-family-list)) (set-frame-font "Inconsolata-17" t t))
                                        ;(set-frame-parameter nil 'left-fringe 18)

(use-package gruvbox-theme
  :disabled
  :init
  (load-theme 'gruvbox t))

(use-package farmhouse-theme
  :disabled
  :init
  (load-theme 'farmhouse-dark t))

(use-package kaolin-themes
  :disabled
  :init
  (load-theme 'kaolin-aurora t)
  :config
;  (use-package color-identifiers-mode)
)

(use-package panda-
  :disabled
  :config
  (load-theme 'panda t))

(use-package dracula-theme
  :disabled
  :config
  (load-theme 'dracula t))

(use-package color-theme-sanityinc-solarized
  :disabled
  :config
  (load-theme 'sanityinc-solarized-light)
  )

(use-package color-theme-sanityinc-tomorrow
  :disabled
  :config
  (load-theme 'sanityinc-tomorrow-blue))



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
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))


(provide 'setup-ui)
