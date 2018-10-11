;; Timestamp: <>

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Gas Emacs is powering up... Ten paciencia, Senor %s!" current-user)

(defconst gas/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))

(defun gas/emacs-subdirectory (d) (expand-file-name d gas/emacs-directory))

 (let* ((subdirs '("elisp" "backups"))
            (fulldirs (mapcar (lambda (d) (gas/emacs-subdirectory d)) subdirs)))
       (dolist (dir fulldirs)
         (when (not (file-exists-p dir))
           (message "Make directory: %s" dir)
           (make-directory dir))))

(add-to-list 'load-path (gas/emacs-subdirectory "elisp"))

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

;; no https :(
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(setq package-user-dir
      (expand-file-name (concat "elpa-" (substring emacs-version 0 (string-match "\\." emacs-version 3)))
			gas/emacs-directory))

;(unless (file-exists-p (expand-file-name "archives/melpa" package-user-dir)) (package-refresh-contents))

(unless (and (package-installed-p 'use-package)
             (package-installed-p 'delight))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'delight)
  (package-install 'cl)
  )

;(eval-when-compile
;  (require 'cl)
;  (require 'use-package)
;  (require 'delight)
 ; (require 'bind-key)
;  )

(setq use-package-always-ensure t) ;; use-package should always try to install
;; need :ensure nil for packages pre-loaded
(setq use-package-always-defer t) ;; only load packages when needed

;; prefer the .el file if newer
(customize-set-variable 'load-prefer-newer t)

(use-package auto-compile
  :defer nil
  :config (auto-compile-on-load-mode))

(if (equal "atearoot" user-login-name)
    (setq user-mail-address "agasson@ateasystems.com")
    (setq user-mail-address "agasson@red-elvis.net"))


(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package delight
  :ensure t
  :config
;  (progn
;  (eval-after-load "ClojureC" '(delight 'clojurec-mode  "☯cljc"))
;  (eval-after-load "ClojureScript" '(delight 'clojurescript-mode  "☯cljs"))
;)
)

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
(require 'setup-ui)

(require 'setup-navigation)

(require 'setup-packages)
(require 'setup-wordsmithing)

(require 'setup-code-editing)
(require 'setup-elisp)
;(require 'setup-common-lisp)
(require 'basic-programmodes)
(require 'setup-clojure)

(require 'setup-javascript)

(require 'setup-html)

;(require 'setup-org-mode)


(require 'key-bindings)

(require 'setup-org)

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

(message "Gas Emacs is ready to do thy bidding, Senor %s!" current-user)
