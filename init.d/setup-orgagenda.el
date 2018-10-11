;;; setup-orgagenda.el --- My Org-Agenda settings
;; Time-stamp: <2014-12-11 15:47:13 agasson>
(require 'org-query)

;(delight 'org-agenda-mode "📅")
(delight 'org-agenda-mode "øα")
;;(defvar org-default-notes-file "~/Dropbox/GTD/@SUMMARY.org")
(defvar org-default-tasks-file "~/Dropbox/GTD/atea.org")

(setq org-agenda-files
      (quote ("~/Dropbox/GTD/atea.org"
              "~/Dropbox/GTD/inbox.org"
              "~/Dropbox/GTD/tickler.org"
              "~/Dropbox/GTD/prochain.org"
              "~/Dropbox/GTD/someday.org"
              "~/Dropbox/GTD/calendars/atea-cal.org"
              "~/Dropbox/GTD/calendars/changecontrol-cal.org"
              )))

(defun custom-org-agenda-mode-defaults ()
  (electric-indent-mode -1)
  (org-defkey org-agenda-mode-map "W" 'oh/agenda-remove-restriction)
  (org-defkey org-agenda-mode-map "N" 'oh/agenda-restrict-to-subtree)
  (org-defkey org-agenda-mode-map "P" 'oh/agenda-restrict-to-project)
  (org-defkey org-agenda-mode-map "q" 'bury-buffer)
  ;(org-defkey org-agenda-mode-map "I" 'org-pomodoro)
  ;(org-defkey org-agenda-mode-map "O" 'org-pomodoro)
  (org-defkey org-agenda-mode-map "p" 'gas/punch-in)
  (org-defkey org-agenda-mode-map (kbd "C-c C-x C-i") 'org-pomodoro)
  (org-defkey org-agenda-mode-map (kbd "C-c C-x C-o") 'org-pomodoro)
  (org-defkey org-agenda-mode-map (kbd "C-c C-x C-p") 'gas/punch-in))

(add-hook 'org-agenda-mode-hook 'custom-org-agenda-mode-defaults 'append)

;; DO Not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)
;; Compact the block agenda view
(setq org-agenda-compact-blocks t)
;; Set the default agenda-view to 2 days
(setq org-agenda-span 1)
;(setq org-agenda-tags-column -100) ; take advantage of the screen width
(setq org-agenda-sticky nil)
(setq org-agenda-inhibit-startup t)
(setq org-agenda-use-tag-inheritance t)
(setq org-agenda-show-log t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
(setq org-agenda-block-separator t)

(setq org-agenda-start-with-log-mode t)

(setq org-agenda-time-grid
      (quote
       ((daily today require-timed)
        (0800 1000 1200 1400 1600 1800)
        "......"
        "----------------")))

;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items (quote (closed state)))
; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

(defun jethro/org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (or (org-current-is-todo)
                (not (org-get-scheduled-time (point))))
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(defun jethro/switch-to-agenda ()
  (interactive)
  (org-agenda nil " ")
  (delete-other-windows))

(bind-key "<f1>" 'jethro/switch-to-agenda)

(defun org-query-gtd-active-project ()
  "Is the headline at point an active project"
  (and
   (not (org-query-parent (org-query-stringmatch "^Un jour / peut-être")))
   (org-query-child (org-query-todo))
   (org-query-todo '("PROCHAIN"))))

(defun org-query-gtd-project ()
  "Is the headline at point a waiting project"
  (and
   (not (org-query-parent (org-query-stringmatch "^Un jour / peut-être")))
   (org-query-child (org-query-todo))))

(defun org-query-gtd-someday-project ()
  "Is the headline at point a waiting project"
  (and
   (org-query-parent (org-query-stringmatch "^Un jour / peut-être"))
   (org-query-child (org-query-todo))))


(defun org-query-gtd-active-project-armed ()
  "Active project with a PROCHAIN state child"
  (and (org-query-gtd-active-project)
       (org-query-child (org-query-todo '("PROCHAIN" "ATTENTE")))))


(defun org-query-gtd-active-project-stuck ()
  "Active project with a PROCHAIN state child"
  (and (org-query-gtd-active-project)
       (not (org-query-child (org-query-todo '("PROCHAIN" "ATTENTE"))))))

(defun org-query-gtd-refile ()
  "Tasks to refile"
  (org-query-parent (org-query-stringmatch "^Inbox")))

(defun org-query-gtd-active-project-next-task ()
  "Is the headline a next action in an active project."
  (and
   (org-query-parent (org-query-gtd-active-project))
   (org-query-todo '("PROCHAIN"))))


(defun org-query-gtd-loose-task ()
  "Tasks that do not belong to any project"
  (not (or
        (org-query-parent (or
                           (org-query-todo)
                           (org-query-stringmatch "\\(^Atea\\|^Un jour / peut-être\\)")))
        (org-query-child (org-query-todo)))))

(defun org-query-gtd-someday-loose-task ()
  "Tasks that do not belong to any project"
  (and
   (org-query-parent (org-query-stringmatch "\\(^Atea\\|^Un jour / peut-être\\)"))
   (and
       (not (org-query-parent (org-query-todo)))
       (not (org-query-child (org-query-todo))))))

(defun org-query-gtd-backlog-task ()
  "Tasks in active project with a TODO state"
  (and (org-query-parent (org-query-gtd-active-project))
       (not (org-query-parent (org-query-todo '("ANNULÉ" "FINI"))))
       (not (org-query-parent (org-query-stringmatch "Un jour / peut-être")))))

(defun gas/agendablock-tasks-waiting ()
  `(tags-todo "/+ATTENTE|+SOUTE"
              ((org-agenda-overriding-header "Tâches en attente")
               (org-tags-match-list-sublevels nil)
               (org-agenda-skip-function (org-query-select "headline" (not (org-query-gtd-project))))
               (org-agenda-todo-ignore-scheduled t)
               (org-agenda-todo-ignore-deadlines t)
               )))

(defun gas/agendablock-next-in-active ()
  `(tags-todo "/+PROCHAIN"
              ((org-agenda-overriding-header "Tâches à Venir en Projets actifs")
               (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-active-project-next-task)))
               (org-tags-match-list-sublevels t)
               (org-agenda-todo-ignore-scheduled 't)
               (org-agenda-todo-ignore-deadlines 't)
               (org-agenda-todo-ignore-with-date 't)
               (org-agenda-sorting-strategy
                '(todo-state-down effort-up category-keep)))))

(defun gas/agendablock-backlog-of-active ()
  `(tags-todo "/!TODO"
              ((org-agenda-overriding-header "Arriéré de Projets actifs")
               (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-backlog-task)))
               (org-agenda-todo-ignore-scheduled 't)
               (org-agenda-todo-ignore-deadlines 't)
               (org-agenda-todo-ignore-with-date 't)
               (org-agenda-sorting-strategy
                '(category-keep)))))

(defun gas/agendablock-active-projects-without-next ()
  `(tags-todo "/+PROCHAIN"
              ((org-agenda-overriding-header "Projets sans tâches à venir")
               (org-agenda-skip-function (org-query-select "tree" (org-query-gtd-active-project-stuck)))
               (org-tags-match-list-sublevels 't)
               (org-agenda-sorting-strategy
                '(category-keep)))))

(defun gas/agendablock-active-projects-with-next ()
  `(tags-todo "/!PROCHAIN"
              ((org-agenda-overriding-header "Projets actifs avec des tâches à venir")
               (org-agenda-skip-function (org-query-select "tree" (org-query-gtd-active-project-armed)))
               (org-tags-match-list-sublevels 't)
               (org-agenda-sorting-strategy
                '(category-keep)))))

(defun gas/agendablock-waiting-projects ()
  `(tags-todo "/!TODO|PROCHAIN"
              ((org-agenda-overriding-header "Projets Bloqués")
               (org-agenda-skip-function (org-query-select "tree" (org-query-gtd-project)))
               (org-tags-match-list-sublevels 't)
               (org-agenda-sorting-strategy
                '(category-keep)))))

(defun gas/agendablock-loose-tasks ()
  `(tags-todo "/+TODO"
              ((org-agenda-overriding-header "Tâches n'appartenant pas à un Projet")
               (org-agenda-skip-function
                (org-query-select "headline" (org-query-gtd-loose-task))
               (org-agenda-todo-ignore-scheduled 't)
               (org-agenda-todo-ignore-deadlines 't)
               (org-agenda-todo-ignore-with-date 't)
               (org-agenda-sorting-strategy
                '(category-keep))))))

(defun gas/agendablock-inbox ()
  `(tags-todo "LEVEL=2"
              ((org-agenda-overriding-header "Tâches à la Représenter")
               (org-agenda-skip-function (org-query-select "tree" (org-query-gtd-refile)))
               (org-tags-match-list-sublevels nil))))


;(setq org-agenda-custom-commands
 ;     `((" " "Ordre du Jour"
 ;       ((agenda "" ((org-agenda-ndays 1)))
 ;        ,(gas/agendablock-inbox)
 ;        ,(gas/agendablock-tasks-waiting)
 ;        ,(gas/agendablock-next-in-active)
 ;        ,(gas/agendablock-active-projects-with-next)
 ;        ,(gas/agendablock-active-projects-without-next)
 ;        ,(gas/agendablock-waiting-projects)
 ;        ,(gas/agendablock-backlog-of-active)
 ;        ,(gas/agendablock-checklists))
 ;       nil)
 ;       ("r" "Revoir Ordre du Jour"
 ;      ((agenda "" ((org-agenda-ndays 3)))
 ;       ,(gas/agendablock-inbox)
 ;       ,(gas/agendablock-loose-tasks)
 ;       ,(gas/agendablock-tasks-waiting)
 ;       ,(gas/agendablock-next-in-active)
 ;       ,(gas/agendablock-active-projects-with-next)
 ;       ,(gas/agendablock-active-projects-without-next)
 ;       ,(gas/agendablock-backlog-of-active)
 ;       ,(gas/agendablock-checklists))
 ;      nil)))

;;
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
                (org-agenda-skip-function #'gas/skip-non-projects)
                (org-tags-match-list-sublevels 'indented)
                (org-agenda-sorting-strategy
                             '(category-keep))))
         (tags-todo "-ANNULÉ/!"
               ((org-agenda-overriding-header "Projets Bloqués")
                (org-agenda-skip-function #'gas/skip-non-stuck-projects)
                (org-agenda-sorting-strategy
                             '(category-keep))))
         (tags-todo "-ANNULÉ+ATTENTE|SUSPENDUE/!"
                   ((org-agenda-overriding-header "Attente ou Reporté Tâches")
                     ))
         (tags-todo "-SUSPENDUE-ANNULÉ-ATTENTE/+TODO"
               ((org-agenda-overriding-header "Tâches n'appartenant pas à un Projet")
                (org-agenda-skip-function
                (org-query-select "headline" (org-query-gtd-loose-task)))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
         (tags-todo "/+PROCHAIN"
              ((org-agenda-overriding-header "Tâches à Venir en Projets actifs")
               (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-active-project-next-task)))
               (org-tags-match-list-sublevels t)
               (org-agenda-todo-ignore-scheduled 't)
               (org-agenda-todo-ignore-deadlines 't)
               (org-agenda-todo-ignore-with-date 't)
               (org-agenda-sorting-strategy
                '(todo-state-down effort-up category-keep))))
          (tags-todo "/!PROCHAIN"
              ((org-agenda-overriding-header "Projets actifs avec des tâches à venir")
               (org-agenda-skip-function (org-query-select "tree" (org-query-gtd-active-project-armed)))
               (org-tags-match-list-sublevels 't)
               (org-agenda-sorting-strategy
                '(category-keep))))
         nil)))))

(provide 'setup-orgagenda)
