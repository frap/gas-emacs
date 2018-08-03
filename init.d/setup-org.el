(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :config
  )

;;(delight 'org-agenda-mode "📅")

(use-package org
  :ensure org-plus-contrib        ; But it comes with Emacs now!?
  :mode (("\\.org$" . org-mode))
  :delight org-mode "📝"
  :init
  (setq org-directory "~/Dropbox/GTD"
        org-use-speed-commands t
        org-use-fast-todo-selection t
        org-treat-S-cursor-todo-selection-as-state-change nil
        org-return-follows-link t
        org-hide-emphasis-markers t
        org-completion-use-ido t
        org-outline-path-complete-in-steps nil
        org-src-fontify-natively t   ;; Pretty code blocks
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-todo-keywords '((sequence "TODO(t)" "PROCHAIN(n)" "TÉLÉPHONE(p)" "RÉUNION(r)" "|" "FINI(f)")
                            (sequence "ATTENDRE(a@/!)" "SUSPENDUE(s@/!)" "|" "ANNULÉ(c@/!)" ))
        org-time-stamp-rounding-minutes (quote (1 1))
        org-clock-out-remove-zero-time-clocks t
        org-agenda-sorting-strategy
        (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
                (todo category-up effort-up)
                (tags category-up effort-up)
                (search category-up)))
        org-agenda-span 10          ;; 10 day overview
        org-agenda-tags-column -102
        org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9)))
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes (quote confirm)
        org-completion-use-ido t
        ido-everywhere t
        ido-mode (quote both)
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        org-indirect-buffer-display 'current-window
        )
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
  (add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode))   ;; Journal entries
  (add-hook 'org-mode-hook 'yas-minor-mode-on)
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-M-|" . indent-rigidly)
         ("C-c a" . org-agenda))
  :config
  (font-lock-add-keywords            ; A bit silly but my headers are now
   'org-mode `(("^\\*+ \\(TODO\\) "  ; shorter, and that is nice canceled
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "🏷")
                          nil)))
               ("^\\*+ \\(PROCHAIN\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "📌")
                          nil)))
               ("^\\*+ \\(ANNULÉ\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "🗑")
                          nil)))
               ("^\\*+ \\(RÉUNION\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "📅")
                          nil)))
               ("^\\*+ \\(SUSPENDUE\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "🔏")
                          nil)))
               ("^\\*+ \\(TÉLÉPHONE\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "📞")
                          nil)))
               ("^\\*+ \\(UN_JOUR\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "⏳")
                          nil)))
               ("^\\*+ \\(FINI\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "✔")
                          nil)))))

  (define-key org-mode-map (kbd "M-C-n") 'org-end-of-item-list)
  (define-key org-mode-map (kbd "M-C-p") 'org-beginning-of-item-list)
  (define-key org-mode-map (kbd "M-C-u") 'outline-up-heading)
  (define-key org-mode-map (kbd "M-C-w") 'org-table-copy-region)
  (define-key org-mode-map (kbd "M-C-y") 'org-table-paste-rectangle)

  (define-key org-mode-map [remap org-return] (lambda () (interactive)
                                                (if (org-in-src-block-p)
                                                    (org-return)
                                                  (org-return-indent))))
  )

;; Auto-update tags whenever the state is changed
(setq org-todo-state-tags-triggers
      '(("ANNULÉ" ("ANNULÉ" . t))
	("ATTENDRE" ("ATTENDRE" . t))
	("SUSPENDUE" ("ATTENDRE") ("SUSPENDUE" . t))
	(done ("ATTENDRE") ("SUSPENDUE"))
	("TODO" ("ATTENDRE") ("ANNULÉ") ("SUSPENDUE"))
	("PROCHAIN" ("ATTENDRE") ("ANNULÉ") ("SUSPENDUE"))
	("FINI" ("ATTENDRE") ("ANNULÉ") ("SUSPENDUE"))))
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'gas/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)
;; don't use pretty things for the clocktable
(setq org-pretty-entities nil)
;; If idle for more than 15 minutes, resolve the things by asking what to do
;; with the clock time
(setq org-clock-idle-time 15)

(defun eos/org-clock-in ()
  (interactive)
  (org-clock-in '(4)))

(global-set-key (kbd "<f5>") #'eos/org-clock-in)
(global-set-key (kbd "C-c <f5>") #'eos/org-clock-in)
;(global-set-key (kbd "<f6>") #'org-clock-out)
;(global-set-key (kbd "C-c O") #'org-clock-out)

(setq  gas/keep-clock-running nil)

(defun gas/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
  Skips capture tasks, projects, and subprojects.
  Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
     (cond
      ((and (member (org-get-todo-state) (list "TODO"))
            (gas/is-task-p))
       "PROCHAIN")
      ((and (member (org-get-todo-state) (list "PROCHAIN"))
            (gas/is-project-p))
       "TODO"))))

  (defun gas/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun gas/punch-in (arg)
  "Start continuous clocking and set the default task to the
   selected task.  If no task is selected set the Organization task
   as the default task."
  (interactive "p")
  (setq gas/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (gas/clock-in-organisation-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-pomodoro '(16))
        (gas/clock-in-organisation-task-as-default)))))

(defun gas/punch-out ()
  (interactive)
  (setq gas/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun gas/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defvar gas/organisation-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun gas/clock-in-organisation-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find gas/organisation-task-id 'marker)
    (org-clock-in '(16))))

(defun gas/clock-out-maybe ()
  (when (and gas/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (gas/clock-in-parent-task)))
;; use discrete minute intervals
(setq org-time-stamp-rounding-minutes (quote (1 1)))
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; ORG AGENDA
; Agenda clock report parameters
;; Dim blocked tasks (and other settings)
(setq org-enforce-todo-dependencies t)
(setq org-agenda-inhibit-startup nil)
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view (disabled)
(setq org-agenda-compact-blocks nil)

(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

;; Set the times to display in the time grid
(setq org-agenda-time-grid
  '((daily today require-timed)
    "----------------"
    (800 1200 1600 2000)))

;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items (quote (closed state)))

(add-hook 'org-clock-out-hook 'gas/clock-out-maybe 'append)

                               ; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
                                        ; global Effort estimate values
                                        ; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))

(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("4:00"))))

;; for html exports
(use-package org-drill
      :ensure org-plus-contrib)

(use-package org-bullets
  :ensure t
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

(setq org-hide-emphasis-markers t)

;; make asterisks appear as unicode bullets
(font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


(use-package org-journal
  :ensure t
  :init
  (setq org-journal-dir "~/Dropbox/GTD/")
  (setq org-journal-date-format "#+TITLE: Journal Entry- %e %b %Y (%A)")
  (setq org-journal-time-format ""))

(defun get-journal-file-today ()
  "Return filename for today's journal entry."
  (let ((daily-name (format-time-string "%Y%m%d")))
    (expand-file-name (concat org-journal-dir daily-name))))

(defun journal-file-today ()
  "Create and load a journal file based on today's date."
  (interactive)
  (find-file (get-journal-file-today)))

(global-set-key (kbd "C-c f j") 'journal-file-today)


(setq  org-agenda-files     (quote ("~/Dropbox/GTD/atea.org"
                                    "~/Dropbox/GTD/refile.org"
                                    "~/Dropbox/GTD/someday.org"
                                    "~/Dropbox/GTD/tickler.org"
)))


(defvar org-default-notes-file "~/Dropbox/GTD/@SUMMARY.org")
(defvar org-default-tasks-file "~/Dropbox/GTD/atea.org")

(defun ha/first-header ()
  (goto-char (point-min))
  (search-forward-regexp "^\* ")
  (beginning-of-line 1)
  (point))

(setq org-deadline-warning-days 10)

(setq org-capture-templates
       (quote (("t" "todo" entry (file "~/Dropbox/GTD/refile.org")
                "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
               ("r" "respond" entry (file "~/Dropbox/GTD/refile.org")
                "* PROCHAIN Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
               ("n" "Les notes" entry (file "~/Dropbox/GTD/refile.org")
                "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
               ("j" "un note quotidien"     entry
                (file (get-journal-file-today))
                "* %?\n\n  %i\n\n  From: %a" :empty-lines 1 :clock-in t :clock-resume t)
               ("m" "Meeting" entry (file "~/Dropbox/GTD/refile.org")
                "* RÉUNION with %? :RÉUNION:\n%U" :clock-in t :clock-resume t)
               ("p" "Phone call" entry (file "~/Dropbox/GTD/refile.org")
                "* TÉLÉPHONE %? :TÉLÉPHONE:\n%U" :clock-in t :clock-resume t)
               ("h" "Habit🙈" entry (file "~/Dropbox/GTD/refile.org")
                "* TODO %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: PROCHAIN\n:END:\n")
               ))
                )

(setq org-agenda-custom-commands (quote
                                  (
                                   ("N" "Notes" tags "NOTE"
                                    ((org-agenda-overriding-header "Notes")
                                     (org-tags-match-list-sublevels t)))
                                   ("h" "Habits" tags-todo "STYLE=\"habit\""
                                    ((org-agenda-overriding-header "Habits")
                                     (org-agenda-sorting-strategy
                                      '(todo-state-down priority-down category-keep))))
                                   (" " "Agenda"
                                    ((agenda "" nil)
                                     (tags-todo "-SUSPENDUE-ANNULÉ/!PROCHAIN"
                                           ((org-agenda-overriding-header "Tâche Prochain")
                                            (org-tags-match-list-sublevels nil)))
                                     (tags-todo "-SUSPENDUE-ANNULÉ/!TODO"
                                                ((org-agenda-overriding-header "Tâche de Travail")
                                                 (org-agenda-sorting-strategy
                                                  '(todo-state-down priority-down))))
                                     (tags "REFILE"
                                           ((org-agenda-overriding-header "Tâche de Refile")
                                            (org-tags-match-list-sublevels nil)))
                                     ))))
 )


(use-package ox-html
  :init
  (setq org-html-postamble nil)
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-toc nil)
  (setq org-html-head-extra "
          <link href='http://fonts.googleapis.com/css?family=Source+Sans+Pro:400,700,400italic,700italic&subset=latin,latin-ext' rel='stylesheet' type='text/css'>
          <link href='http://fonts.googleapis.com/css?family=Source+Code+Pro:400,700' rel='stylesheet' type='text/css'>
          <style type='text/css'>
             body {
                font-family: 'Source Sans Pro', sans-serif;
             }
             pre, code {
                font-family: 'Source Code Pro', monospace;
             }
          </style>"))


(require 'ox-publish)
;;  (require 'ox-rss)

(use-package ox-reveal
  :ensure ox-reveal
  :init
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
  (setq org-reveal-postamble "Andrés Gasson")
  ;; (setq org-reveal-mathjax t)
  )

(use-package htmlize
  :ensure t)

(use-package org
  :config
  (add-to-list 'org-src-lang-modes '("dot" . "graphviz-dot"))

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell      . t)
                                 (js         . t)
                                 (emacs-lisp . t)
                                 (perl       . t)
                                 (clojure    . t)
                                 (python     . t)
                                 (ruby       . t)
                                 (dot        . t)
                                 (css        . t)
                                 (plantuml   . t))
                               ))

(eval-after-load 'org-src
  '(define-key org-src-mode-map
     (kbd "C-x C-s") #'org-edit-src-exit))

;; font colouring in code blocks
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(provide 'setup-org)
