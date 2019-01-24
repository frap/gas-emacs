;; ===================
;; * BETTER DEFAULTS *

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(blink-cursor-mode -1)
(setq-default cursor-type '(bar . 2))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Smoother and nicer scrolling
(setq scroll-margin 10
   scroll-step 1
   next-line-add-newlines nil
   scroll-conservatively 10000
   scroll-preserve-screen-position 1)

;; Turn off mouse interface early in startup to avoid momentary display
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Warn only when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Move file to trash instead of removing.
(setq-default delete-by-moving-to-trash t)

;; Revert (update) buffers automatically when underlying files are changed externally.
(global-auto-revert-mode t)
(delight 'auto-revert-mode)

(setq
 inhibit-startup-message t         ; Don't show the startup message...
 inhibit-startup-screen t          ; ... or screen
 cursor-in-non-selected-windows t  ; Hide the cursor in inactive windows

 echo-keystrokes 0.1               ; Show keystrokes right away, don't show the message in the scratch buffer
 initial-scratch-message nil       ; Empty scratch buffer
 initial-major-mode 'org-mode      ; Org mode by default
 sentence-end-double-space nil     ; Sentences should end in one space, come on!
 confirm-kill-emacs 'y-or-n-p      ; y and n instead of yes and no when quitting
 help-window-select t              ; Select help window so it's easy to quit it with 'q'
)

(defalias 'yes-or-no-p 'y-or-n-p)      ; y and n instead of yes and no everywhere else
(delete-selection-mode 1)          ; Delete selected text when typing
(global-unset-key (kbd "s-p"))     ; Don't print
;; Answering just 'y' or 'n' will do
;(fset 'yes-or-no-p 'y-or-n-p)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; We need Emacs kill ring and system clipboard to be independent. Simpleclip is the solution to that.
(use-package simpleclip
  :config
  (simpleclip-mode 1))

;; Things you'd expect from macOS app.
(global-set-key (kbd "s-s") 'save-buffer)             ;; save
(global-set-key (kbd "s-S") 'write-file)              ;; save as
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs) ;; quit
(global-set-key (kbd "s-a") 'mark-whole-buffer)       ;; select all
;; (global-set-key (kbd "s-z") 'undo)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top
(when (display-graphic-p)
   (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)
(set-default 'fill-column 80)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)
(delight 'subword-mode)

;; Don't break lines for me, please
;; (setq-default truncate-lines t)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; A better ediff
;(setq ediff-diff-options "-w")
;(setq ediff-split-window-function 'split-window-horizontally)
;(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; No electric indent -- automatically pair quotes
(setq electric-indent-mode nil)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun magnars/create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'magnars/create-non-existent-directory)

;; disable fringes... not sure yet if I want this
;;(fringe-mode 0)
(fringe-mode nil) ;; reset to default

;; Keep org-mode timestamps in English, e.g. [2016-11-05 Sat 10:03]
(setq system-time-locale "C")

;; Improves rendering of long lines.
;; Not for everyone. If you plan on working with Arabic or other bidi languages don't use this.
(setq-default bidi-display-reordering nil)

(provide 'better-defaults)
