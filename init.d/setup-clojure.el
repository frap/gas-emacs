(use-package clojure-mode
  :ensure t
  :delight "☯λlj"
  :mode (("\\.edn$"  . clojurec-mode)
         ("\\.boot$" . clojure-mode)
         ("\\.cljs$" . clojurescript-mode)
         ("\\.cljc$" . clojurec-mode)
         )
  :init
  (defconst clojure--prettify-symbols-alist
    '(("fn"   . ?λ)
      ("__"   . ?⁈)
      ("<=" . ?≤)
      (">=" . ?≥)
      ("<-" . ?←)
      ("->" . ?→)
      ("<=" . ?⇐)
      ("=>" . ?⇒)
      ("lambda" . ?λ)
      ))
  :config
  (add-hook 'clojure-mode-hook 'yas-minor-mode)
  (delight 'yas-minor-mode)
  (add-hook 'clojure-mode-hook 'subword-mode)
  (add-hook 'clojure-mode-hook 'global-prettify-symbols-mode))

(delight '((clojurescript-mode "☯λljs" :major)
           (clojurec-mode "☯λljc"      :major)))
;
(defun figwheel-repl ()
  (interactive)
  (inf-clojure "clojure -Afig"))


                                        ;(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

(use-package html-to-hiccup :ensure t)

(use-package cider
  :ensure t
  :pin melpa-stable
  :delight "🍺"
  :config
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
 ;; (add-hook 'cider-mode-hook #'cider-hydra-mode)
 ;; (add-hook 'clojure-mode-hook #'paredit-mode)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-display-help-banner nil)
;;  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

  :bind  ("C-c r" . cider-repl-reset)
         ("C-c ." . cider-reset-test-run-tests)
;;(  cider-namespace-refresh)
)

;(setq cider-default-cljs-repl 'nashorn)

;(use-package clj-refactor
;  :ensure t
;  :config
;  (add-hook 'clojure-mode-hook (lambda ()
;                                 (clj-refactor-mode 1)
;                                 ;; insert keybinding setup here
;                                 ))
;  (cljr-add-keybindings-with-prefix "C-c C-m")
;  (setq cljr-warn-on-eval nil)
;  :bind ("C-c '" . hydra-cljr-help-menu/body)
;      (setq cljr-warn-on-eval nil)
 ;     (setq cljr-favor-prefix-notation nil)

;  :delight "☠♅"
;)

(defun cider-repl-command (cmd)
  "Execute commands on the cider repl"
  (cider-switch-to-repl-buffer)
  (goto-char (point-max))
  (insert cmd)
  (cider-repl-return)
  (cider-switch-to-last-clojure-buffer))

(defun cider-repl-reset ()
  "Assumes reloaded + tools.namespace is used to reload everything"
  (interactive)
  (save-some-buffers)
  (cider-repl-command "(trivia.core/reset)"))

(defun cider-reset-test-run-tests ()
  (interactive)
  (cider-repl-reset)
  (cider-test-run-project-tests))

(use-package cider-eval-sexp-fu
  :ensure t)

                                        ;(use-package cider-hydra
                                        ;  :ensure t
                                        ;  )

(setq plexus/clojure-fill-column 45)

(defun plexus/cider-eval-and-insert ()
  (interactive)
  (let* ((lbp (line-beginning-position))
         (comment-pos (search-backward ";;=>" lbp t)))
    (if comment-pos
        (progn
          (goto-char comment-pos)
          (delete-region comment-pos (line-end-position))
          (cider-eval-last-sexp t)
          (goto-char comment-pos)
          (insert ";;=> "))
      (progn
        (delete-horizontal-space)
        (let ((pos (point)))
          (cider-eval-last-sexp t)
          (goto-char pos)
          (insert
           (format
            (format "%%%ds;;=> " (if (= (current-column) 0)
                                     0
                                   (max 0 (+ (- plexus/clojure-fill-column (current-column)) 2))))
            "")))))))


;; (define-key cider-mode-map (kbd "C-x C-e") 'cider-eval-last-sexp)
;;(define-key cider-mode-map (kbd "C-x C-w") 'plexus/cider-eval-and-insert)
;;(define-key cider-mode-map (kbd "H-SPC") 'cider-eval-defun-at-point)
;;(define-key cider-mode-map (kbd "M-TAB") 'cider-eval-last-sexp)
;;(define-key cider-mode-map (kbd "C-M-i") 'completion-at-point)

;;(define-key cider-mode-map (kbd "M-TAB") 'company-complete)
;;(define-key cider-mode-map (kbd "C-M-i") 'company-complete)

                                        ;(use-package inf-clojure :ensure t)

;; (setq cider-cljs-lein-repl
;;       "(do (require 'figwheel-sidecar.repl-api)
;;            (figwheel-sidecar.repl-api/start-figwheel!)
;;            (figwheel-sidecar.repl-api/cljs-repl))")

;;(sp-local-pair 'clojure-mode "#(" ")")
;;(sp-local-pair 'clojure-mode "#{" "}")


(defun cider-quit-all ()
  (interactive)
  (progn
    (dolist (connection cider-connections)
      (cider--quit-connection connection))
    (message "All active nREPL connections were closed")))

;; override this to use interrupt-process (SIGINT) instead of kill-process
;; (SIGKILL) so process can clean up
(defun nrepl--maybe-kill-server-buffer (server-buf)
  "Kill SERVER-BUF and its process, subject to user confirmation.
Do nothing if there is a REPL connected to that server."
  (with-current-buffer server-buf
    ;; Don't kill the server if there is a REPL connected to it.
    (when (and (not nrepl-client-buffers)
               (or (not nrepl-prompt-to-kill-server-buffer-on-quit)
                   (y-or-n-p "Also kill server process and buffer? ")))

      (let ((proc (get-buffer-process server-buf)))
        (when (process-live-p proc)
          (set-process-query-on-exit-flag proc nil)
          (interrupt-process proc)) ;; <-- s/kill/interrupt/
        (kill-buffer server-buf)))))


(defun cider--select-zombie-buffer (repl-buffers)
  "Return a zombie buffer from REPL-BUFFERS, or nil if none exists."
  (when-let ((zombie-buffs (seq-remove #'get-buffer-process repl-buffers)))
    (when (y-or-n-p
           (format "Zombie REPL buffers exist (%s).  Reuse? "
                   (mapconcat #'buffer-name zombie-buffs ", ")))
      (if (= (length zombie-buffs) 1)
          (car zombie-buffs)
        (completing-read "Choose REPL buffer: "
                         (mapcar #'buffer-name zombie-buffs)
nil t)))))

(defun cider-find-reusable-repl-buffer (endpoint project-directory)
  "Check whether a reusable connection buffer already exists.
Looks for buffers where `nrepl-endpoint' matches ENDPOINT, or
`nrepl-project-dir' matches PROJECT-DIRECTORY.  If such a buffer was found,
and has no process, return it.  If the process is alive, ask the user for
confirmation and return 'new/nil for y/n answer respectively.  If other
REPL buffers with dead process exist, ask the user if any of those should
be reused."
  (if-let ((repl-buffers (cider-repl-buffers))
           (exact-buff (seq-find
                        (lambda (buff)
                          (with-current-buffer buff
                            (or (and endpoint
                                     (equal endpoint nrepl-endpoint))
                                (and project-directory
                                     (equal project-directory nrepl-project-dir)))))
                        repl-buffers)))
      (progn
        (when (get-buffer-process exact-buff)
          (message "CIDER: Reusing existing buffer, %s" exact-buff))
        exact-buff)
    (or (cider--select-zombie-buffer repl-buffers) 'new)))


(use-package parinfer
  :disabled
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (setq parinfer-extensions
        '(defaults       ; should be included.
           pretty-parens  ; different paren styles for different modes.
           ;;evil           ; If you use Evil.
           lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
           paredit        ; Introduce some paredit commands.
           smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
           smart-yank))   ; Yank behavior depend on mode.
  :hook
  ((clojure-mode
    emacs-lisp-mode
    common-lisp-mode
    scheme-mode
    lisp-mode) . parinfer-mode))

(provide 'setup-clojure)
