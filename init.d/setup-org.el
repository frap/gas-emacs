;; Timestamp: <>

(use-package org
  :ensure org-plus-contrib        ; But it comes with Emacs now!?
  :mode (("\\.org$" . org-mode))
  :delight org-mode "✒"              ; "📝"
  :init
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
  (add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode))   ;; Journal entries
  (add-hook 'org-mode-hook 'yas-minor-mode-on)
  :custom-face
    (variable-pitch ((t (:family "Source Sans Pro" :height 160 :weight light))))
    ;;(variable-pitch ((t (:family "Avenir Next" :height 160 :weight light))))
    (fixed-pitch ((t (:family "Inconsolata"))))
    (org-done ((t (:background "#E8E8E8" :foreground "#0E0E0E" :strike-through t :weight bold))))
    (org-headline-done ((t (:foreground "#171717" :strike-through t))))
					; (org-document-title ((t (:foreground "#171717" :weight bold :height 1.5))))
;  (org-level-1 ((t (:foreground "#171717" :weight bold :height 1.3))))
;  (org-level-2 ((t (:foreground "yellow" :weight normal :height 1.2))))
;  (org-level-3 ((t (:foreground "blue" :weight normal :height 1.1))))
;  (org-image-actual-width '(600))
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-M-|" . indent-rigidly)
         ("C-c a" . org-agenda)
         ("C-M-|" . indent-rigidly)
         :map org-mode-map
         ("M-n" . outline-next-visible-heading)
         ("M-p" . outline-previous-visible-)
         ("M-C-n" . org-end-of-item-list)
         ("M-C-p" . org-beginning-of-item-list)
         )
  :custom
  (org-directory "~/Dropbox/GTD")
  (org-log-done t)
  (org-startup-indented t)
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-use-speed-commands (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**"))))
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-hide-emphasis-markers t)
  (org-return-follows-link t)
  :config
  (font-lock-add-keywords            ; A bit silly but my headers are now
   'org-mode `(("^ *\\([-]\\) "
                (0 (progn () (compose-region (match-beginning 1) (match-end 1) "•")
			  nil)))
	       ("^\\*+ \\(TODO\\) "  ; shorter, and that is nice canceled
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
               ("^\\*+ \\(ATTENTE\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "⏳")
                          nil)))
               ("^\\*+ \\(FINI\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "✔")
                          nil)))))

  (add-to-list 'org-structure-template-alist '("el" "#+BEGIN_SRC emacs-lisp :tangle yes?\n\n#+END_SRC"))
 ;  (define-key org-mode-map (kbd "M-C-u") 'outline-up-heading)
;  (define-key org-mode-map (kbd "M-C-w") 'org-table-copy-region)
 ; (define-key org-mode-map (kbd "M-C-y") 'org-table-paste-rectangle)

  (define-key org-mode-map [remap org-return] (lambda () (interactive)
                                                (if (org-in-src-block-p)
                                                   (org-return)
                                                  (org-return-indent))))
)

(setq org-agenda-files
      (quote ("~/Dropbox/GTD/atea.org"
              "~/Dropbox/GTD/inbox.org"
              "~/Dropbox/GTD/tickler.org"
              "~/Dropbox/GTD/prochain.org"
              "~/Dropbox/GTD/someday.org"
              "~/Dropbox/GTD/calendars/atea-cal.org"
              "~/Dropbox/GTD/calendars/changecontrol-cal.org"
              )))

(setq org-todo-keywords
      '((sequence "TODO(t)" "PROCHAIN(n)" "TÉLÉPHONE(p)" "RÉUNION(r)" "|" "FINI(f)")
        (sequence "ATTENTE(w@/!)" "SUSPENDUE(s@/!)" "|" "ANNULÉ(c@/!)" )))

(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

;; Auto-update tags whenever the state is changed
(setq org-todo-state-tags-triggers
      '(("ANNULÉ" ("ANNULÉ" . t))
	("ATTENTE" ("ATTENTE" . t))
	("SUSPENDUE" ("ATTENTE") ("SUSPENDUE" . t))
	(done ("ATTENTE") ("SUSPENDUE"))
	("TODO" ("ATTENTE") ("ANNULÉ") ("SUSPENDUE"))
	("PROCHAIN" ("ATTENTE") ("ANNULÉ") ("SUSPENDUE"))
	("FINI" ("ATTENTE") ("ANNULÉ") ("SUSPENDUE"))))

;; todo state change changes
(setq org-todo-keyword-faces
      '(("UN_JOUR"       :foreground "forest green" :weight bold)
        ("PROCHAIN"      :foreground "blue" :weight bold)
        ("ATTENTE"       :foreground "yellow" :weight bold)
        ("FINI"          :foreground "forest green" :weight bold)
        ("ANNULÉ"        :foreground "orange" :weight bold)
        ("TÉLÉPHONE"     :foreground "forest green" :weight bold)
        ("GOAL"          :foreground "blue" :weight bold)
        ("VALUE"         :foreground "red" :weight bold)
        ("QUOTE"         :foreground "yellow" :weight bold)
        ("DEAMONS"       :foreground "red" :weight bold)
        ("RÉUNION"   :foreground "forest green" :weight bold)
        ))

;; couple of short-cut keys to make it easier to edit text.
(defun org-text-bold () "Wraps the region with asterisks."
       (interactive)
       (surround-text "*"))
(defun org-text-italics () "Wraps the region with slashes."
       (interactive)
       (surround-text "/"))
(defun org-text-code () "Wraps the region with equal signs."
       (interactive)
       (surround-text "="))


(defun org-archive-all-done-item ()
  "Archive all item that have with prefix FINI."
  (interactive)
  (save-excursion
    (show-all)
    (goto-char (point-min))
    (if (search-forward-regexp "^[\\*]+ \\(FINI\\|ANNULÉ\\)" nil t)
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp "^[\\*]+ \\(FINI\\|ANNULÉ\\)" nil t)
            (org-advertized-archive-subtree))
          (message "Archive fini!"))
      (message "No need to archive"))))


(use-package org
  :config
  (bind-keys :map org-mode-map
             ("A-b" . (surround-text-with "+"))
             ("s-b" . (surround-text-with "*"))
             ("A-i" . (surround-text-with "/"))
             ("s-i" . (surround-text-with "/"))
             ("A-=" . (surround-text-with "="))
             ("s-=" . (surround-text-with "="))
             ("A-`" . (surround-text-with "~"))
             ("s-`" . (surround-text-with "~"))

             ("C-s-f" . forward-sentence)
             ("C-s-b" . backward-sentence)))

					; UTF bullets

(use-package org-bullets
  :after org
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

(use-package org-fancy-priorities
  :ensure t
  :delight
  :init
  (add-hook 'org-agenda-mode-hook 'org-fancy-priorities-mode)
  (add-hook 'org-mode-hook 'org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list
        '((?A . "❗") (?B . "⬆") (?C . "⬇") (?D . "☕")
          (?1 . "⚡") (?2 . "⮬") (?3 . "⮮") (?4 . "☕")
          (?I . "Imaportant"))))


(use-package ox-md
  :ensure nil
  :defer 3
  :after org)

(setq org-capture-templates
       (quote (("t" "todo :@bureau:" entry (file "~/Dropbox/GTD/inbox.org")
                "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
               ("T" "Tickler" entry
                               (file+headline "~/Dropbox/GTD/tickler.org" "Tickler")
                               "* %i%? \n %U")
               ("r" "respond" entry (file "~/Dropbox/GTD/inbox.org")
                "* PROCHAIN Respond to %:from on %:subject :@bureau:\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
               ("n" "Les notes" entry (file "~/Dropbox/GTD/inbox.org")
                "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
               ("j" "un note quotidien"     entry
                (file (get-journal-file-today))
                "* %?\n\n  %i\n\n  From: %a" :empty-lines 1 :clock-in t :clock-resume t)
               ("m" "Meeting" entry (file "~/Dropbox/GTD/calendars/atea-cal.org")
                "* RÉUNION with %? :RÉUNION:@bureau:\n%U" :clock-in t :clock-resume t)
               ("p" "Phone call" entry (file+headline "~/Dropbox/GTD/atea.org" "Interruptions")
                "* TÉLÉPHONE %? :TÉLÉPHONE:@bureau:\n%U" :clock-in t :clock-resume t)
               ("h" "Habit🙈" entry (file "~/Dropbox/GTD/atea.org")
                "* TODO %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: PROCHAIN\n:END:\n")
               ))
                )

(setq org-tag-alist (quote (("@errand" . ?e)
                            ("@bureau" . ?o)
                            ("@maison" . ?h)
                            ("@ferme"  . ?f)
                            (:newline)
                            ("ATTENTE"  . ?w)
                            ("SUSPENDUE" . ?H)
                            ("ANNULÉ"    . ?c)
                            ("RÉUNION"   . ?m)
                            ("TÉLÉPHONE" . ?p))))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

;; Refiling
; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;;;; IDO setup
; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)


;;;; Org-agenda
(setq org-agenda-start-with-log-mode t)
;; Compact the block agenda view
(setq org-agenda-compact-blocks t)
;; Set the default agenda-view to 2 days
(setq org-agenda-span 1)
;; deadline warning
(setq org-deadline-warning-days 10)

(setq org-agenda-time-grid
      (quote
       ((daily today require-timed)
        (0800 1000 1200 1400 1600 1800)
        "......"
        "----------------")))

(setq org-agenda-custom-commands
  (quote
   (
   (" " "Agenda"
         ((agenda "" nil)
         (tags "REFILE"
               ((org-agenda-overriding-header "Tâches à la Représenter")
                 (org-tags-match-list-sublevels nil)))
         (tags-todo "-ANNULÉ/!PROCHAIN"
                        ((org-agenda-overriding-header "Tâches Courante")
                         (org-tags-match-list-sublevels nil)
                         (org-agenda-sorting-strategy
                               '(todo-state-down priority-down category-keep))))
         (tags-todo "-ATTENTE-ANNULÉ/!"
               ((org-agenda-overriding-header "Projets")
             ;   (org-agenda-skip-function #'gas/skip-non-projects)
                (org-tags-match-list-sublevels 'indented)
                (org-agenda-sorting-strategy
                             '(category-keep))))
         (tags-todo "-ANNULÉ/!"
               ((org-agenda-overriding-header "Projets Bloqués")
             ;   (org-agenda-skip-function #'gas/skip-non-stuck-projects)
                (org-agenda-sorting-strategy
                             '(category-keep))))
         (tags-todo "-ANNULÉ+ATTENTE|SUSPENDUE/!"
                   ((org-agenda-overriding-header "Attente ou Reporté Tâches")
                     ))
         (tags-todo "-SUSPENDUE-ANNULÉ-ATTENTE/+TODO"
               ((org-agenda-overriding-header "Tâches n'appartenant pas à un Projet")
              ;  (org-agenda-skip-function
              ;  (org-query-select "headline" (org-query-gtd-loose-task)))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
         (tags-todo "/+PROCHAIN"
              ((org-agenda-overriding-header "Tâches à Venir en Projets actifs")
            ;   (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-active-project-next-task)))
               (org-tags-match-list-sublevels t)
               (org-agenda-todo-ignore-scheduled 't)
               (org-agenda-todo-ignore-deadlines 't)
               (org-agenda-todo-ignore-with-date 't)
               (org-agenda-sorting-strategy
                '(todo-state-down effort-up category-keep))))
          (tags-todo "/!PROCHAIN"
              ((org-agenda-overriding-header "Projets actifs avec des tâches à venir")
             ;  (org-agenda-skip-function (org-query-select "tree" (org-query-gtd-active-project-armed)))
               (org-tags-match-list-sublevels 't)
               (org-agenda-sorting-strategy
                '(category-keep))))
         nil)))))




(global-set-key (kbd "<f5>") #'gas/punch-in)
(global-set-key (kbd "C-c <f5>") #'gas/punch-out)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "<f12>") 'org-agenda)
;(global-set-key (kbd "<f6>") #'org-clock-out)
;(global-set-key (kbd "C-c O") #'org-clock-out)


;; CLOCKING

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
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
;; moving clock time with shift up - changing increments
(setq org-time-stamp-rounding-minutes (quote (0 5)))
;;  use pretty things for the clocktable
(setq org-pretty-entities t)
;; If idle for more than 15 minutes, resolve the things by asking what to do
;; with the clock time
(setq org-clock-idle-time 15)

(setq gas/keep-clock-running nil)

(defun gas/clock-in-to-next (kw)
  "Switch a task from TODO to PROCHAIN when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from PROCHAIN back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
     (cond
      ((and (member (org-get-todo-state) (list "TODO"))
            (gas/is-task-p))
       "PROCHAIN")
      ((and (member (org-get-todo-state) (list "PROCHAIN"))
            (gas/is-project-p))
       "PROCHAIN"))))


(defun gas/punch-in (arg)
  "Start continuous clocking and set the default task to the
   selected task.  If no task is selected set the Organisation task
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
          (org-clock-in '(16))
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

(defun gas/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when gas/keep-clock-running
            (gas/clock-in-default-task)))))))

(defvar gas/organisation-task-id "b54b48a5-0dd5-4737-823e-cf16863aca62")

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

(defun gas/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

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

(defun gas/is-project-p ()
  "Any task with a todo keyword subtask"
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
      (and is-a-task has-subtask))))

(defun gas/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (gas/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun gas/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (gas/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (gas/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ PROCHAIN " subtree-end t))
                (unless (member "ATTENTE" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun gas/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (gas/list-sublevels-for-projects-indented)
  (if (save-excursion (gas/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((gas/is-project-p)
            nil)
           ((and (gas/is-project-subtree-p) (not (gas/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun gas/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "PROCHAIN"))
            (org-todo "TODO")))))))

(add-hook 'org-after-todo-state-change-hook 'gas/mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'gas/mark-next-parent-tasks-todo 'append)

;; use discrete minute intervals
(setq org-time-stamp-rounding-minutes (quote (1 1)))
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

(setq org-columns-default-format "%14SCHEDULED(When) %Effort{:} %CLOCKSUM(Clked){:} %1PRIORITY %TODO(State) %50ITEM(Task) %TAGS(Tags)")

(add-hook 'org-clock-out-hook 'gas/clock-out-maybe 'append)

;; C-c C-x e 3 of 0:45
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))

;(setq org-agenda-clock-consistency-checks
 ;     (quote (:max-duration "4:00"
  ;            :min-duration 0
  ;            :max-gap 0
  ;            :gap-ok-around ("4:00"))))

;(setq org-hide-emphasis-markers t)

(use-package org-pomodoro
  :ensure t
  :after org
  :bind
  (:map org-agenda-mode-map
        (("I" . org-pomodoro)
         ("O" . gas/punch-out)))
  :config
  (progn
  (setq org-pomodoro-audio-player "afplay")
  ; Bind special hotkey for  org-pomodoro
  ;  (global-unset-key (kbd "C-c C-x C-p"))
   ; (define-key org-mode-map (kbd "C-c C-x C-p") 'org-pomodoro)
  ;  (global-set-key (kbd "C-c C-x C-p") 'org-pomodoro)
  )
  ;;       (setq org-pomodoro-ticking-sound-p t)
  :custom
  (org-pomodoro-format "Pomodoro-%s")
  )
;; show pomodoro count
;;(setq org-agenda-clockreport-parameter-plist
 ;;'(:fileskip0 t :link t :maxlevel 2 :formula "$5=($3+$4)*(60/25);t"))
(custom-set-faces `(org-headline-done ((t (:inherit shadow))))
               ;   `(org-pomodoro-mode-line ((t (:foreground "#2aa198"))))
                  `(org-link ((t (:underline nil))))
                  `(org-date ((t (:underline nil)))))

;;(org-journal-update-auto-mode-alist)

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir "~/Dropbox/journal/")
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

;;(global-set-key (kbd "C-c f j") 'journal-file-today)

;;; org-gcal
(setq package-check-signature nil)
(use-package org-gcal
  :ensure t
  ;;   :after '(auth-source-pass password-store)
  :config
  (setq org-gcal-client-id "887865341451-orrpnv3cu0fnh8hdtge77sv6csqilqtu.apps.googleusercontent.com"
        org-gcal-client-secret "WmOGOCr_aWPJSqmwXHV-29bv"
        org-gcal-file-alist
        '(("agasson@ateasystems.com" . "~/Dropbox/GTD/calendars/atea-cal.org")
          ("ateasystems.com_0ie21uc26j0a41g60b8f99mh1k@group.calendar.google.com" . "~/Dropbox/GTD/calendars/changecontrol-cal.org"))))

(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))

;; run on a timer
(run-at-time (* 5 60) nil
             (lambda ()
               (let ((inhibit-message t))
                 (org-gcal-refresh-token)
                 (org-gcal-fetch))))

(require 'ox-publish)

;; hugo blogging
(use-package ox-hugo
  :defer 3
  :after org)

(use-package ob-plantuml
  :ensure nil
  :after org
  :custom
  (org-plantuml-jar-path
   (expand-file-name "/usr/local/Cellar/plantuml/1.2018.10/libexec/plantuml.jar")))

(use-package htmlize
  :ensure t)
;; Lierate programming
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

(use-package hydra :ensure t
  :config
  ;; Define the templates
  (setq org-structure-template-alist
        '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
          ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
          ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
          ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n/verse>")
          ("n" "#+begin_note\n?\n#+end_note" "<note>\n?\n/note>")
          ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n/center>")
          ("l" "#+begin_export latex\n?\n#+end_export" "<literal style=\"latex\">\n?\n</literal>")
          ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
          ("h" "#+begin_export html\n?\n#+end_exrt" "<literal style=\"html\">\n?\n</literal>")
          ("H" "#+html: " "<literal style=\"html\">?</literal>")
          ("a" "#+begin_export ascii\n?\n#+end_export")
          ("A" "#+ascii: ")
          ("i" "#+index: ?" "#+index: ?")
          ("I" "#+include: %file ?" "<include file=%file markup=\"?\">")))

  ;; Shortcuts
  (defun hot-expand (str &optional mod)
    "Expand org template."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (org-try-structure-completion)
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  (defhydra hydra-org-template (:color blue :hint nil)
    "
     Org template

 block               src block         structure
--------------------------------------------------------------------------------------
_C_: center        _s_: src         _L_: LATEX:
_q_: quote         _e_: emacs lisp  _i_: index:
_E_: example       _p_: python      _I_: INCLUDE:
_v_: verse         _P_: perl        _H_: HTML:
_a_: ascii         _u_: Plantuml    _A_: ASCII:
_l_: latex         _d_: ditaa
_h_: html          _S_: shell
_n_: note          _c_: clojure
"
    ("s" (hot-expand "<s"))
    ("E" (hot-expand "<e"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("C" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("n" (hot-expand "<n"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("e" (hot-expand "<s" "emacs-lisp"))
    ("p" (hot-expand "<s" "python"))
    ("P" (hot-expand "<s" "perl"))
    ("j" (hot-expand "<s" "java"))
    ("c" (hot-expand "<s" "clojure"))
    ("S" (hot-expand "<s" "sh"))
    ("d" (hot-expand "<s" "ditaa :file CHANGE.png :cache yes"))
    ("u" (hot-expand "<s" "plantuml :file CHANGE.svg :cache yes"))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("ESC" nil "quit"))

  (define-key org-mode-map "<"
    (lambda () (interactive)
      (if (or (region-active-p) (looking-back "^"))
          (hydra-org-template/body)
        (self-insert-command 1)))))

(use-package ox-reveal
  :ensure ox-reveal
  :init
  (setq org-reveal-root "file:///Users/agasson/Dev/js/reveal.js")
  (setq org-reveal-hlevel 2)
  (setq org-reveal-postamble "Andrés Gasson")
  (setq org-reveal-mathjax t)
  (setq org-reveal-klipsfy-src t)
  ;(setq org-reveal-external-plugins (klipse . "{:src '%splugin/klipse_reveal.js'}"))
  )


(provide 'setup-org)
