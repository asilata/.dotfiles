;; Org files locations
(setq org-default-directory "~/MEGAsync/Org/")
(setq org-default-notes-file (concat org-default-directory "todo.org"))
(setq org-agenda-files
      (mapcar (lambda (x) (concat org-default-directory x ".org"))
              '("todo" "math" "service" "teaching" "calendar" "algtop" "shared/shared")))

(setq org-log-done t)
(setq org-refile-targets
      '((org-agenda-files :maxlevel . 5)))
(setq org-refile-use-outline-path 'file)

;; Keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-todo-keyword-faces
      '(("TODO" org-todo)
	("DONE" org-done)
        ("WAITING" :foreground "#F0DFAF" :weight bold)
	("CANCELLED" :foreground "#CC9393" :weight bold :strike-through "#CC9393")
        ))

;;capture todo items using C-c c t
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n%a\n")
        ("d" "todo with deadline" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n%a\nDEADLINE: %t")
        ("c" "Calendar entry" entry
         (file "~/Dropbox/Org/calendar.org")
         "* %?\n%t\n")))

;; Org files customization
(setq org-cycle-separator-lines 1)

;; Agenda customization
(setq org-agenda-window-setup 'current-window)
(setq org-deadline-warning-days 7)
;;show me tasks scheduled or due in next fortnight
(setq org-agenda-span 'fortnight)
;;don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-skip-scheduled-if-done t)
;;don't give awarning colour to tasks with impending deadlines
;;if they are scheduled to be done
(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
;;don't show tasks that are scheduled or have deadlines in the
;;normal todo list
(setq org-agenda-todo-ignore-deadlines 'all)
(setq org-agenda-todo-ignore-scheduled 'all)
(setq org-log-done t)
;;sort tasks in order of when they are due and then by priority
;; (setq org-agenda-sorting-strategy
;;   (quote
;;    ((agenda deadline-up priority-down)
;;     (todo priority-down category-keep)
;;     (tags priority-down category-keep)
;;     (search category-keep))))
(setq org-columns-default-format "%50ITEM(Task) %9TODO %10CLOCKSUM_T(Time today) %10CLOCKSUM(Time total) %16SCHEDULED")
(setq org-agenda-custom-commands
      '(("c" "Comprehensive view"
         ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
                      (org-agenda-span 'day)
                      (org-agenda-ndays 1)
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-start-day "+0d")
                      (org-agenda-todo-ignore-deadlines nil)))
          (todo "TODO"
                ((org-agenda-overriding-header "Unscheduled tasks:")
                 (org-agenda-todo-ignore-deadlines 'all)
                 (org-agenda-todo-ignore-scheduled 'all)))
          (agenda "" ((org-agenda-overriding-header "Upcoming week:")
                      (org-agenda-span 'week)
                      (org-agenda-start-day "+1d")
                      (org-agenda-start-on-weekday nil)
                      ;;(org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                      ;;(org-agenda-prefix-format '((agenda . " %-12:c%?-12t %s%b ")))
                      ))
          (todo "WAITING"
                ((org-agenda-overriding-header "Waiting tasks:")
                 (org-agenda-todo-ignore-deadlines 'all)
                 (org-agenda-todo-ignore-scheduled 'all)))
          ))))

;; Google calendar integration
(use-package org-gcal
  :ensure t
  :config
  (setq org-gcal-client-id
        (string-trim
         (shell-command-to-string "gpg2 -dq ~/.emacs.d/org-gcal/.org-gcal-client-id.gpg")))
  (setq org-gcal-client-secret
        (string-trim
         (shell-command-to-string "gpg2 -dq ~/.emacs.d/org-gcal/.org-gcal-client-secret.gpg")))
  (setq org-gcal-file-alist `(("asilata@gmail.com" .
                               ,(concat org-default-directory "calendar.org"))
                              ("es2hibml3t2m5le9nl83lq0boo@group.calendar.google.com" .
                               ,(concat org-default-directory "algtop.org"))))
  (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync))))

