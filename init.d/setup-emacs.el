;; Timestamp: <>
;(eval-when-compile (require 'cl-lib))
;(require 'cl)
;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("Gas " invocation-name " - " (:eval (if (buffer-file-name)
                                                    (abbreviate-file-name (buffer-file-name))
                                                  "%b"))))

;; functional
(use-package dash
  :ensure t
  :config (eval-after-load "dash" '(dash-enable-font-lock)))

;; string manipulation
(use-package s
  :ensure t)
;; File manipulation
(use-package f
  :ensure t)

;; Keep emacs customise settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
     (when (file-exists-p custom-file)
       (load custom-file))


;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(setq backup-by-copying t      ; don't clobber symlinks
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Do save #..# files
(setq auto-save-default t)

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name (concat user-emacs-directory "autosave")) t)))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Save a list of recent files visited. (open recent file with C-x f)
(use-package recentf
  :init
  (recentf-mode 1)

  :config

  ;; Increase limit
  (setq recentf-max-menu-items 100)

  ;; Emacs
  (add-to-list 'recentf-exclude (format "%s/Dev/emacs/\\(?!\\(gas.*\\)\\)" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/.*" (getenv "HOME")))

  ;; Some caches
  (add-to-list 'recentf-exclude (format "%s/\\.ido\\.last" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.recentf" (getenv "HOME")))

  ;; elfeed
  (add-to-list 'recentf-exclude (format "%s/\\.elfeed/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/Dropbox/emacs/elfeed/.*" (getenv "HOME")))

  ;; Org-mode organisation
  (add-to-list 'recentf-exclude (format "%s/Dropbox/GTD/organisation/.*" (getenv "HOME")))

  ;; Org/todo/calendars
  (add-to-list 'recentf-exclude ".*todo.org")
  (add-to-list 'recentf-exclude (format "%s/Dropbox/Calendars/.*" (getenv "HOME")))

  ;; Maildir
  (add-to-list 'recentf-exclude (format "%s/maildir.*" (getenv "HOME"))))

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Undo/redo window configuration with C-c <left>/<right>
;(winner-mode 1)

;; Suppress “ad-handle-definition: .. redefined” warnings during Emacs startup.
(customize-set-variable 'ad-redefinition-action 'accept)

;; Emacs server
(use-package edit-server
 :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(provide 'setup-emacs)
