;;; Org files locations
(setq org-default-directory "~/Org/")
(setq org-shared-directory "~/Org-shared/")
(setq org-default-notes-file (concat org-default-directory "todo.org"))
(setq org-agenda-files
      (append (file-expand-wildcards (concat org-default-directory "*.org"))
              (file-expand-wildcards (concat org-shared-directory "*.org"))
              '("~/Teaching")))

;;; Global options for notes and refiling
(setq org-log-done t)
(setq org-log-state-notes-insert-after-drawers t)
(setq org-refile-targets
      '((org-agenda-files :maxlevel . 5)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

;;; Keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@)" "|" "DONE(d)" "CANCELLED(c@)" "SHELVED(s)" "MEETING(m)" "ONGOING(o)")))

(setq org-todo-keyword-faces
      '(("TODO" org-todo)
	("DONE" org-done)
        ("WAITING" :foreground "#F0DFAF" :weight bold)
	("CANCELLED" :foreground "#CC9393" :weight bold :strike-through "#CC9393")
        ("SHELVED" :foreground "#DFAF8F" :weight bold)
        ("MEETING" :foreground "#8CD0D3" :weight bold)
        ("ONGOING" :foreground "#DC8CC3" :weight bold :italic t)
        ("BOOKMARK" :foreground "#DC8CC3" :weight bold)
        ("READING" :foreground "#F0DFAF" :weight bold)
        ))

;;; Tags
(setq org-tag-persistent-alist
      '((:startgroup . nil)
        ("work" . ?w)
        ("service" . ?s)
        ("personal" . ?p)
        (:endgroup . nil)
        ("longterm" . ?l)
        ("reading" . ?r)
        ("annoying" . ?a)
        ("shared" . ?h)
        ("email" . ?e)
        ("shopping" . ?b)
        ))

(setq org-tag-faces
      '(("work" . (:foreground "#8CD0D3" :weight bold))
        ("service" . (:foreground "#8CD0D3" :weight bold))
        ("personal" . (:foreground "#8CD0D3" :weight bold))))

;;; Captures
(global-set-key (kbd "C-c c") 'org-capture)

;;;; Orca
(use-package orca
  :straight t
  :config
  (setq orca-handler-list
        `((orca-handler-current-buffer
           "\\* Tasks")
          (orca-handler-file
           ,(concat org-default-directory "bookmarks.org")
           "\\* Bookmarks"))))

;;;; Capture templates
(setq org-capture-templates
      `(("t" "todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n%a\n" :clock-in t :clock-resume t)
        ("e" "respond" entry (file+headline org-default-notes-file "Emails")
         "* TODO Reply to %:from (%:subject) :email:\n  DEADLINE:%^{Deadline}t\n  %a" :immediate-finish t)
        ("r" "Reading")
        ("rb" "book" entry (file ,(concat org-default-directory "books.org"))
         "* %^{Status|READING} %^{Title} %^{AUTHOR}p \n  - Recorded on: %U" :empty-lines 1 :immediate-finish t)
        ("rm" "math" entry (file ,(concat org-default-directory "math-reading.org"))
         "* TO-READ %T\n  %a" :empty-lines 1 :immediate-finish t)
        ("j" "Journal")
        ("jj" "journal" entry (file+datetree ,(concat org-default-directory "journal.org.gpg"))
         "* %^{Title}\n  %U\n  %?" :empty-lines 1 :clock-in t :clock-resume t)
        ("jp" "shared journal" entry (file+datetree ,(concat org-shared-directory "journal.org.gpg"))
         "* Note by %n at %<%H:%M>\n  %?" :empty-lines 1 :clock-in t :clock-resume t)
        ("jm" "math journal" entry (file+datetree ,(concat org-default-directory "calculations.org"))
         "* %<%H:%M>\n  %?" :empty-lines 1 :clock-in t :clock-resume t)
        ("b" "borrowed" entry (file+olp ,(concat org-shared-directory "shared.org") "Reminders" "Loans")
         "* BORROWED %^{Thing borrowed} from %^{From|me} to %^{To|me}.\n  DEADLINE:%^{Deadline}t"
         :immediate-finish t)
        ("h" "Health")
        ("hw" "Weight" table-line (file+headline ,(concat org-default-directory "health.org") "Weight")
         "|%<%Y-%m-%d>|%?||" :immediate-finish t)
        ("m" "meeting" entry (file+headline org-default-notes-file "Meetings")
         "* MEETING Meeting with %? :meeting:\n" :clock-in t :clock-resume t)
        ("c" "Calendar entry" entry (file ,(concat org-default-directory "calendar.org"))
         "* %?\n%t\n")
        ("f" "Recipes" entry (file ,(concat org-shared-directory "recipes.org"))
         "%(org-chef-get-recipe-from-url)"
         :empty-lines 1)
        ;; ("F" "Recipes" entry (file ,(concat org-default-directory "recipes.org"))
        ;;  "%(org-chef-recipe-org-string (org-chef-fetch-recipe (caar org-stored-links))"
        ;;  :empty-lines 1)
        ("L" "Link" entry #'orca-handle-link "* BOOKMARK %(orca-wash-link)\nAdded: %U\n%?")
        ("l" "Link" entry #'orca-handle-link "* BOOKMARK %(orca-wash-link)\nAdded: %U\n%?")
        ))

;;; Org files customization
(setq org-cycle-separator-lines 1)

;;; Syntax highlighting
(setq org-highlight-latex-and-related '(latex))
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("amsart" "\\documentclass[a4paper]{amsart}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;;; Agenda customization
;;;; Viewing options
(setq org-agenda-window-setup 'current-window)
(setq org-deadline-warning-days 3)
(setq org-agenda-span 'fortnight)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
(setq org-agenda-todo-ignore-deadlines 'all)
(setq org-agenda-todo-ignore-scheduled 'all)
(setq org-agenda-todo-list-sublevels nil)
(setq org-log-done t)
(setq org-pretty-entities t)
(setq org-columns-default-format "%50ITEM(Task) %9TODO %10CLOCKSUM_T(Time today) %10CLOCKSUM(Time total) %10EFFORT(Effort)")

;;;; Custom agendas
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
          (todo "ONGOING"
                ((org-agenda-overriding-header "Ongoing tasks:")
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

;;; Google calendar integration
(use-package org-gcal
  :straight t
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
  (setq org-gcal-up-days 7)
  (setq org-gcal-down-days 7)
  (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-fetch))))

;;; Encryption
(use-package org-crypt
  :config
  (setq org-crypt-key "D93ED1F5")
  (setq org-crypt-disable-auto-save t))

;;; Org journal
(use-package org-journal
  :straight t
  :config
  (setq org-journal-dir (concat org-default-directory "journal/"))
  (setq org-journal-enable-encryption t)
  (setq org-journal-file-format "%Y-%m-%d.org")
  )

;;; Org roam
(use-package org-roam
  :hook 
  (after-init . org-roam-mode)
  :straight (:host github :repo "jethrokuan/org-roam")
  :custom
  (org-roam-directory (concat org-default-directory "Roam"))
  (org-roam-capture-templates
   `(("d" "default" plain #'org-roam--capture-get-point
      "* %U\n  %?"
      :file-name "%<%Y%m%d%H%M%S>-${slug}"
      :head "#+TITLE: ${title}\n\n- references :: \n\n"
      :unnarrowed t)))
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

(use-package company-org-roam
  :after org-roam company org
  :config
  (company-org-roam-init))

;;; Custom functions
;;;; Mark todo as done if all checkboxes are done
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

;;; Local variables
;; Local Variables:
;; byte-compile-warnings: (not free-vars callargs)
;; End:
