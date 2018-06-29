(require 'package)
(setq package-archives
      '(
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org"          . "http://orgmode.org/elpa/"))) ;; no https :(
;;(package-initialize)

(setq package-user-dir
      (expand-file-name (concat "elpa-" (substring emacs-version 0 (string-match "\\." emacs-version 3)))
			user-emacs-directory))

(unless (file-exists-p (expand-file-name "archives/melpa" package-user-dir)) (package-refresh-contents))

(unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))

(require 'use-package)

(if (equal "atearoot" user-login-name)
    (setq user-mail-address "agasson@ateasystems.com")
    (setq user-mail-address "agasson@red-elvis.net"))

;(when (memq window-system '(mac ns x))
;  (exec-path-from-shell-initialize))

(use-package diminish
  :ensure t)
(use-package delight
  :ensure t)

(use-package ess-site
  :load-path "init.d/"
  :commands R)


;; Completely hide visual-line-mode and change auto-fill-mode to " AF".
(use-package emacs
  :delight
  (auto-fill-function " AF")
  (visual-line-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'better-defaults)

(require 'setup-emacs)
(require 'setup-packages)
(require 'setup-mode-packages)

(require 'setup-programming)
(require 'setup-elisp)
;(require 'setup-common-lisp)

(require 'setup-clojure)

(require 'setup-javascript)
(require 'setup-code-editing)
(require 'setup-html)

(require 'setup-org)

;(require 'setup-org-mode)

(require 'look-and-feel)
(require 'key-bindings)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random stuff below this line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntax highlighting for systemd config files
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.slice\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.path\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.netdev\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.network\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.link\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Configure helm-ag
;; Make sure to have Platinum Searcher installed: https://github.com/monochromegane/the_platinum_searcher

(custom-set-variables
 '(helm-ag-base-command "/usr/local/bin/pt -e --nocolor --nogroup"))
