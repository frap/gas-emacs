(if window-system
    (progn
      (setq frame-title-format '(buffer-file-name "%f" ("%b")))
      (tooltip-mode -1)
      (mouse-wheel-mode t)
      (scroll-bar-mode -1))
  (menu-bar-mode -1))

;; need in case we use Aquamacs which overwrites
(defconst gas/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))

(defun gas/emacs-subdirectory (d) (expand-file-name d gas/emacs-directory))

;; Set path to dependencies

(let* ((subdirs '("elisp" "backups" "autosave" "init.d"))
       (fulldirs (mapcar (lambda (d) (gas/emacs-subdirectory d)) subdirs)))
  (dolist (dir fulldirs)
    (when (not (file-exists-p dir))
      (message "Make directory: %s" dir)
      (make-directory dir))))

(add-to-list 'load-path (gas/emacs-subdirectory "init.d"))
(add-to-list 'load-path (gas/emacs-subdirectory "elisp"))

(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))
;; Set up load path
(add-to-list 'load-path site-lisp-dir)


(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(if (equal "atearoot" user-login-name)
    (setq user-mail-address "agasson@ateasystems.com")
  (setq user-mail-address "agasson@red-elvis.net"))

(setq package-user-dir
      (expand-file-name (concat "elpa-"
                                (substring emacs-version 0
                                           (string-match "\\." emacs-version 3)))
			gas/emacs-directory))

;;; bootstrap `use-package'
;; Install use-package if necessary
(require 'package)
(setq package-enable-at-startup nil)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ;;(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'delight))

;; Enable use-package
(eval-when-compile
  (require 'use-package)
  (require 'delight)                    ;; if you use :delight
  (require 'bind-key))                ;; if you use any :bind variant

;; No need to out 'ensure' everywhere, since we don't use anything else to install packages.
(setq use-package-always-ensure t)
                                        ;(setq use-package-always-defer t) ;; only load packages when needed

;; prefer the .el file if newer
;;(customize-set-variable 'load-prefer-newer t)

(delight '((abbrev-mode " Abv" abbrev)
           (smart-tab-mode " \\t" smart-tab)
           (eldoc-mode nil "eldoc")
           (rainbow-mode)
           (overwrite-mode " Ov" t)
           (emacs-lisp-mode "⍷lisp" :major)))

(require 'better-defaults)

;; Store custom-file separately, don't freak out when it's not found
(setq custom-file (expand-file-name "custom.el" gas/emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Pass system shell environment to Emacs. This is important primarily for shell inside Emacs, but also things like Org mode export to Tex PDF don't work, since it relies on running external command pdflatex, which is loaded from PATH.
(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;(use-package delight
;;  :ensure t
;;  )

;; add load-path
(use-package ess-site
  :load-path "init.d/"
  :commands R)

;; Completely hide visual-line-mode and change auto-fill-mode to " AF".
(use-package emacs
  :delight
  (auto-fill-function " AF")
  (visual-line-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup extensions
(require 'setup-emacs)
(require 'setup-ui)

(eval-after-load 'ido '(require 'setup-ido))

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

(require 'key-bindings)

(require 'setup-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random stuff below this line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntax highlighting for systemd config files
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.slice\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.path\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.netdev\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.network\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.link\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.ldif\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(message "Gas Emacs is ready to do thy bidding, Senor %s!" current-user)
