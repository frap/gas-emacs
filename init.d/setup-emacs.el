(eval-when-compile (require 'cl-lib))
;(require 'cl)

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
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Undo/redo window configuration with C-c <left>/<right>
;(winner-mode 1)


;; Emacs server
(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

;; Highlight matching parentheses when moving over them
(show-paren-mode)

(provide 'setup-emacs)
