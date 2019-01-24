;; ============
;; TEXT EDITING

;; Expand-region allows to gradually expand selection inside words, sentences, expressions, etc.
(use-package expand-region
  :config
  (global-set-key (kbd "s-'") 'er/expand-region)         ;; Cmd+' (apostrophe) to expand
  (global-set-key (kbd "s-S-'") 'er/contract-region))    ;; Cmd+" (same, but with shift) to contract

;; highlight search
(use-package anzu
  :ensure t
  :delight
  :config
  (global-anzu-mode t))

;; Move-text lines around with meta-up/down.
(use-package move-text
  :config
  (move-text-default-bindings))


;; Quickly insert new lines above or below the current line, with correct indentation.
(defun smart-open-line ()
  "Insert an empty line after the current line. Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun smart-open-line-above ()
  "Insert an empty line above the current line. Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "s-<return>") 'smart-open-line)            ;; Cmd+Return new line below
(global-set-key (kbd "s-S-<return>") 'smart-open-line-above)    ;; Cmd+Shift+Return new line above


;; Upcase and lowercase word or region, if selected.
;; To capitalize or un-capitalize word use Alt+c and Alt+l
(global-set-key (kbd "M-u") 'upcase-dwim)   ;; Alt+u upcase
(global-set-key (kbd "M-l") 'downcase-dwim) ;; Alt-l lowercase


;; Comment line or region.
(global-set-key (kbd "s-/") 'comment-line)


;; Visually find and replace text
(use-package visual-regexp
  :config
  (define-key global-map (kbd "M-s-f") 'vr/replace)
  (define-key global-map (kbd "s-r") 'vr/replace))  ;; Cmd+r find and replace


;; Multiple cursors. Similar to Sublime or VS Code.
(use-package multiple-cursors
  :config
  (setq mc/always-run-for-all 1)
  (global-set-key (kbd "s-d") 'mc/mark-next-like-this)        ;; Cmd+d select next occurrence of region
  (global-set-key (kbd "s-D") 'mc/mark-all-dwim)              ;; Cmd+Shift+d select all occurrences
  (global-set-key (kbd "M-s-d") 'mc/edit-beginnings-of-lines) ;; Alt+Cmd+d add cursor to each line in region
  (define-key mc/keymap (kbd "<return>") nil))

;; linum-mode has performance issues with large files
(when (>= emacs-major-version 26)
  (use-package display-line-numbers
 ;;   :disabled
    :defer nil
    :ensure t
    :config
    (global-display-line-numbers-mode)))

;; show traiiling whitespace in red
(customize-set-variable 'show-trailing-whitespace t)
;; dont use hard-tabs
(customize-set-variable 'indent-tabs-mode nil)


(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ===========================
;; SPELLCHECKING AND THESAURUS


;; Spellchecking requires an external command to be available. Install aspell on your Mac, then make it the default checker for Emacs' ispell. Note that personal dictionary is located at ~/.aspell.LANG.pws by default.
(setq ispell-program-name "aspell")


;; Popup window for spellchecking
(use-package flyspell-correct)
(use-package flyspell-correct-popup)


;; Enable spellcheck on the fly for all text modes. This includes org, latex and LaTeX.
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)


;; Enable right mouse click on macOS to see the list of suggestions.
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))


;; Spellcheck current word
(define-key flyspell-mode-map (kbd "s-\\") 'flyspell-correct-previous-word-generic) ;; Cmd+\ spellcheck word with popup
(define-key flyspell-mode-map (kbd "C-s-\\") 'ispell-word)                          ;; Ctrl+Cmd+\ spellcheck word using built UI


;; Search for synonyms
(use-package powerthesaurus
  :config
  (global-set-key (kbd "s-|") 'powerthesaurus-lookup-word-dwim)) ;; Cmd+Shift+\ search thesaurus


;; Word definition search
(use-package define-word
  :config
  (global-set-key (kbd "M-\\") 'define-word-at-point))


(use-package fic-mode
  :commands fic-mode
  :ensure t
  :init (add-hook 'prog-mode-hook 'fic-mode)
  :config

  (defun fic-view-listing ()
    "Use occur to list related FIXME keywords"
    (interactive)
    (occur "\\<\\(FIXME\\|WRITEME\\|WRITEME!\\|TODO\\|BUG\\):?"))
  )

;; Unify the buffer name style
(eval-after-load "uniquify"
  '(progn
     (setq uniquify-buffer-name-style 'forward)))

(provide 'setup-wordsmithing)
