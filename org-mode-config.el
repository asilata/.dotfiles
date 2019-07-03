;; Org files locations
(setq org-default-directory "~/storage/shared/Stuff/Org/")
(setq org-shared-directory "~/storage/shared/Stuff/Org-shared/")
(setq org-default-notes-file (concat org-default-directory "todo.org"))
(setq org-agenda-files
      (append (file-expand-wildcards (concat org-default-directory "*.org"))
              (file-expand-wildcards (concat org-shared-directory "*.org"))))

(setq org-log-done t)
(setq org-log-state-notes-insert-after-drawers t)
(setq org-refile-targets
      '((org-agenda-files :maxlevel . 5)))
(setq org-refile-use-outline-path 'file)

;; Keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@)" "|" "DONE(d)" "CANCELLED(c@)" "SHELVED(s)" "MEETING(m)")))
(setq org-todo-keyword-faces
      '(("TODO" org-todo)
	("DONE" org-done)
        ("WAITING" :foreground "#F0DFAF" :weight bold)
	("CANCELLED" :foreground "#CC9393" :weight bold :strike-through "#CC9393")
        ("SHELVED" :foreground "#DFAF8F" :weight bold)
        ("MEETING" :foreground "#8CD0D3" :weight bold)
        ))

(setq org-tag-persistent-alist
      '((:startgroup . nil)
        ("work" . ?w)
        ("service" . ?s)
        ("personal" . ?p)
        (:endgroup . nil)
        ("longterm" . ?l)
        ("reading" . ?r)
        ("annoying" . ?a)
        ("shared")
        ("email" . ?e)
        ))

(setq org-tag-faces
      '(("work" . (:foreground "#8CD0D3" :weight bold))
        ("service" . (:foreground "#8CD0D3" :weight bold))
        ("personal" . (:foreground "#8CD0D3" :weight bold))))

;;capture todo items using C-c c t
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      `(("t" "todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n%a\n" :clock-in t :clock-resume t)
        ("r" "respond" entry (file+headline org-default-notes-file "Emails")
         "* TODO Reply to %:from (%:subject) :email:\n%a" :immediate-finish t)
        ("m" "meeting" entry (file+headline org-default-notes-file "Meetings")
         "* MEETING Meeting with %? :meeting:\n" :clock-in t :clock-resume t)
        ("c" "Calendar entry" entry (file ,(concat org-default-directory "calendar.org"))
         "* %?\n%t\n")
        ("f" "Recipes" entry (file ,(concat org-default-directory "recipes.org"))
         "%(org-chef-get-recipe-from-url)"
         :empty-lines 1)))

;; Org files customization
(setq org-cycle-separator-lines 1)

;; Syntax highlighting
(setq org-highlight-latex-and-related '(latex))

;; Agenda customization
(setq org-agenda-window-setup 'current-window)
(setq org-deadline-warning-days 3)
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
(setq org-pretty-entities t)
(setq org-columns-default-format "%50ITEM(Task) %9TODO %10CLOCKSUM_T(Time today) %10CLOCKSUM(Time total) %10EFFORT(Effort)")
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
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled 'todo '("WAITING" "DONE")))
          ;;(org-agenda-prefix-format '((agenda . " %-12:c%?-12t %s%b ")))
                      ))
          (todo "WAITING|SHELVED"
                ((org-agenda-overriding-header "Waiting or shelved tasks:")
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

;; Encryption
(use-package org-crypt
  :config
  (setq org-crypt-key "D93ED1F5")
  (setq org-crypt-disable-auto-save t))

;; Org journal
(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir (concat org-default-directory "journal/"))
  (setq org-journal-enable-encryption t)
  (setq org-journal-file-format "%Y-%m-%d.org")
  )

;; Custom functionality, etc

(eval-after-load 'org-list
  '(add-hook 'org-checkbox-statistics-hook (function auto-done-checkboxes)))

(defun auto-done-checkboxes ()
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point)) end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
            (if (match-end 1)
                (if (equal (match-string 1) "100%")
                    ;; all done - do the state change
                    (org-todo 'done)
                  (org-todo 'todo))
              (if (and (> (match-end 2) (match-beginning 2))
                       (equal (match-string 2) (match-string 3)))
                  (org-todo 'done)
                (org-todo 'todo)))))))
