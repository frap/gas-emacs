;; Timestamp: <>

;; ================
;; BASIC NAVIGATION


;; Move around with Cmd+i/j/k/l. This is not for everybody, and it takes away four very well placed
;; key combinations, but if you get used to using these keys instead of arrows, it will be worth it,
;; I promise.
(global-set-key (kbd "s-i") 'previous-line)
(global-set-key (kbd "s-k") 'next-line)
(global-set-key (kbd "s-j") 'left-char)
(global-set-key (kbd "s-l") 'right-char)


;; Kill line with CMD-Backspace. Note that thanks to Simpleclip, killing doesn't rewrite the system clipboard.
;; Kill one word with Alt+Backspace.
;; Kill forward word with Alt-Shift-Backspace.
(global-set-key (kbd "s-<backspace>") 'kill-whole-line)
(global-set-key (kbd "M-S-<backspace>") 'kill-word)


;; Use Cmd for movement and selection.
(global-set-key (kbd "s-<right>") (kbd "C-e"))        ;; End of line
(global-set-key (kbd "S-s-<right>") (kbd "C-S-e"))    ;; Select to end of line
(global-set-key (kbd "s-<left>") (kbd "M-m"))         ;; Beginning of line (first non-whitespace character)
(global-set-key (kbd "S-s-<left>") (kbd "M-S-m"))     ;; Select to beginning of line

(global-set-key (kbd "s-<up>") 'beginning-of-buffer)  ;; First line
(global-set-key (kbd "s-<down>") 'end-of-buffer)      ;; Last line


;; Thanks to Bozhidar Batsov
;; http://emacsredux.com/blog/2013/]05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)
(global-set-key (kbd "s-<left>") 'smarter-move-beginning-of-line)

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; Many commands in Emacs write the current position into mark ring.
;; These custom functions allow for quick movement backward and forward.
;; For example, if you were editing line 6, then did a search with Cmd+f, did something and want to come back,
;; press Cmd+, to go back to line 6. Cmd+. to go forward.
;; These keys are chosen because they are the same buttons as < and >, think of them as arrows.
(defun my-pop-local-mark-ring ()
  (interactive)
  (set-mark-command t))

(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring))))))

(global-set-key (kbd "s-,") 'my-pop-local-mark-ring)
(global-set-key (kbd "s-.") 'unpop-to-mark-command)


;; Same keys with Shift will move you back and forward between open buffers.
(global-set-key (kbd "s-<") 'previous-buffer)
(global-set-key (kbd "s->") 'next-buffer)


;; Highlight matching parentheses when moving over them
(show-paren-mode)


;; =================
;; WINDOW MANAGEMENT

;; use ace-window
 (use-package ace-window
      :ensure t
      :init
        (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?o))
        (global-set-key (kbd "C-x o") 'ace-window)
      :delight ace-window-mode)

;; I like =IDO= for switching buffers since I typically know what I'm after
(global-set-key (kbd "<f8>") 'ido-switch-buffer)
(global-set-key (kbd "S-<f8>") 'ibuffer)

;; better jumping using avy-goto-word-1 bound to C-c j
(use-package avy
  :ensure t
  :init (setq avy-background t))
; bind doesnt work on use-package still ?
(global-set-key (kbd "s-h") 'avy-goto-char-timer)
(global-set-key (kbd "s-j") 'avy-goto-char-timer)
(global-set-key (kbd "s-H") 'avy-pop-mark)
(global-set-key (kbd "s-J") 'avy-pop-mark)
(global-set-key (kbd "A-h") 'avy-goto-char-timer)
(global-set-key (kbd "A-j") 'avy-goto-char-timer)
(global-set-key (kbd "A-H") 'avy-pop-mark)
(global-set-key (kbd "A-J") 'avy-pop-mark)

; IDO (Intercatively do things)
 (use-package ido
       :ensure t
       :init  (setq ido-enable-flex-matching t
                    ido-ignore-extensions t
                    ido-use-virtual-buffers t
                    ido-everywhere t)
       :config
       (ido-mode 1)
       (ido-everywhere 1)
       (add-to-list 'completion-ignored-extensions ".pyc"))

;; add to ido the flx package
 (use-package flx-ido
        :ensure t
        :init (setq ido-enable-flex-matching t
                    ido-use-faces nil)
        :config (flx-ido-mode 1))

;; and ido vertically
 (use-package ido-vertical-mode
       :ensure t
       :init               ; I like up and down arrow keys:
       (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
       :config
       (ido-vertical-mode 1))

; sorts ido file by mtime
(defun ido-sort-mtime ()
  "Reorder the IDO file list to sort from most recently modified."
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (ignore-errors
                  (time-less-p
                   (sixth (file-attributes (concat ido-current-directory b)))
                   (sixth (file-attributes (concat ido-current-directory a))))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (and (char-equal (string-to-char x) ?.) x))
              ido-temp-list))))

(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)


;; tramp and root files
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (let* ((file-name (buffer-file-name))
           (file-root (if (string-match "/ssh:\\([^:]+\\):\\(.*\\)" file-name)
                          (concat "/ssh:"  (match-string 1 file-name)
                                  "|sudo:" (match-string 1 file-name)
                                  ":"      (match-string 2 file-name))
                        (concat "/sudo:localhost:" file-name))))
      (find-alternate-file file-root))))

; smex built using IDO to do similar with M-x commands
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex)
  ("M-X" . smex-major-mode-commands))

; If I don't know what I'm after, Helm is better:
(use-package helm
       :ensure t
       :bind  (("C-x M-i" . helm-imenu))
       )

;; Silver searcher (ag)
(use-package ag
  :ensure    t
  :commands  ag
  :init      (setq ag-highlight-search t)
  :config    (add-to-list 'ag-arguments "--word-regexp"))

;; use beacon mode for cursor highlighting
(use-package beacon
  :ensure t
  :config
  (beacon-mode t))

(provide 'setup-navigation)
