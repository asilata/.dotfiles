(require 'cl-lib)

(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"
    "Root directory of the emacs configuration."))
(defvar user-opt-directory (concat user-emacs-directory "opt/")
  "User-installed Emacs packages go here.")
(defvar local-config-directory (concat user-emacs-directory "local/")
  "Machine-local configuration files go here.")

(mapc (lambda (dir)
        (unless (file-exists-p dir)
          (make-directory dir)))
      (list user-emacs-directory user-opt-directory local-config-directory))

(add-to-list 'load-path user-opt-directory)
(let ((default-directory user-opt-directory))
  (normal-top-level-add-subdirs-to-load-path))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(unless (and (fboundp 'server-running-p) (server-running-p)) (server-start))

(use-package exwm
  :straight t
  :config
  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(1 "HDMI-1"))
  (add-hook 'exwm-randr-screen-change-hook
            (defun ab/exwm-randr ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output eDP-1 --output HDMI-1")))
  (exwm-randr-enable)
  (defun exwm-rename-buffer ()
    (interactive)
    (exwm-workspace-rename-buffer
     (concat exwm-class-name ":"
             (if (<= (length exwm-title) 50) exwm-title
               (concat (substring exwm-title 0 49) "...")))))
  
  (add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook 'exwm-rename-buffer)
  (setq exwm-input-global-keys
        `(
          ;; 's-q': Reset (to line-mode).
          ([?\s-q] . exwm-reset)
          ;; 's-w': Switch workspace.
          ([?\s-w] . exwm-workspace-switch)
          ;; 's-r': Launch application.
          ([?\s-r] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ([?\s-f] . flip-window)))
  
  (push '?\s-n exwm-input-prefix-keys)
  (push '?\s-p exwm-input-prefix-keys)
  (use-package exwm-edit
    :straight t)
  (exwm-enable))

(use-package ace-window
  :straight t
  :bind (("C-x o" . ace-window))
  :custom
  (aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?i ?d))
  (aw-minibuffer-flag t)
  (ace-window-display-mode t)
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
  )

(use-package popper
  :straight t
  :bind (("C-`" . popper-toggle-latest)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Apropos\\*"
          ("\\*Async Shell Command\\*" . hide)
          help-mode
          compilation-mode
          "magit-log"
          ("magit-diff" . hide)
          "Zoom:chat"
          "\\*ednc-log\\*"
          ))
  (popper-mode +1)
  (popper-echo-mode +1))

(defun flip-window ()
  (interactive)
  (let ((win (get-mru-window t t t)))
    (if win
        (progn
          (select-frame-set-input-focus (window-frame win))
          (select-window win))
      (mode-line-other-buffer))))

(defun ab/tab-bar-name ()
    (let ((project-name (projectile-project-name))
          (old-name (tab-bar-tab-name-current-with-count)))
      (if (string= "-" project-name)
          old-name
        (concat project-name ": " old-name))))

(use-package tab-bar-mode
  :bind (("s-n" . tab-next)
         ("s-p" . tab-previous))
  :config
  (setq tab-bar-tab-name-function 'ab/tab-bar-name))

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(setq fill-column 90)
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell t)

(if (fboundp 'fringe-mode) (fringe-mode 4))
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          (buffer-name))
                                        "%b")))

(setq global-auto-revert-mode 1)
(setq auto-revert-interval 3600)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*" ; don't mess with special buffers
        )
  )

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package org
  :after counsel
  :straight t
  :bind (("C-c a" . org-agenda)
         (:map org-mode-map
               ("C-c C-j" . consult-outline)))
  :config
  (setq org-default-directory "~/Org/"
        org-shared-directory "~/Org-shared/")
  (setq org-roam-directory (concat org-default-directory "Roam/"))
  (setq org-default-notes-file (concat org-default-directory "todo.org"))
  (setq org-agenda-files
        (append (file-expand-wildcards (concat org-default-directory "*.org"))
                (file-expand-wildcards (concat org-shared-directory "*.org"))
                (directory-files-recursively (concat org-default-directory "Projects") org-agenda-file-regexp)
                (directory-files-recursively (concat org-default-directory "Teaching") org-agenda-file-regexp)
                `(,(concat org-roam-directory "meetings.org")
                  ,(concat org-roam-directory "calculations.org"))
                ))
  (setq org-log-done t)
  (setq org-log-state-notes-insert-after-drawers t)
  (setq org-refile-targets
        `((org-agenda-files :maxlevel . 5)
          (,(concat org-roam-directory "meetings.org") :maxlevel . 5)
          (,(concat org-roam-directory "calculations.org") :maxlevel . 5)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@)" "|" "DONE(d)" "CANCELLED(c@)" "SHELVED(s)" "MEETING(m)" "ONGOING(o)")))
  
  (setq org-todo-keyword-faces
        '(("TODO" org-todo)
  	("DONE" org-done)
          ("WAITING" :foreground "#F0DFAF" :weight bold)
  	("CANCELLED" :foreground "#CC9393" :weight bold)
          ("SHELVED" :foreground "#DFAF8F" :weight bold)
          ("MEETING" :foreground "#8CD0D3" :weight bold)
          ("ONGOING" :foreground "#DC8CC3" :weight bold :italic t)
          ("BOOKMARK" :foreground "#DC8CC3" :weight bold)
          ("READING" :foreground "#F0DFAF" :weight bold)
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
          ("shared" . ?h)
          ("email" . ?e)
          ("shopping" . ?b)
          ))
  
  (setq org-tag-faces
        '(("work" . (:foreground "#8CD0D3" :weight bold))
          ("service" . (:foreground "#8CD0D3" :weight bold))
          ("personal" . (:foreground "#8CD0D3" :weight bold))))
  (global-set-key (kbd "C-c c") 'org-capture)
  (use-package orca
    :straight t
    :config
    (setq orca-handler-list
          `((orca-handler-current-buffer
             "\\* Tasks")
            (orca-handler-file
             ,(concat org-default-directory "bookmarks.org")
             "\\* Bookmarks"))))
  (setq org-cycle-separator-lines 1)
  (setq org-highlight-latex-and-related '(latex))
  (setq org-export-with-toc nil
        org-export-with-smart-quotes t)
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("amsart" "\\documentclass[a4paper]{amsart}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  (setq org-latex-pdf-process '("latexmk -shell-escape -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))
  (setq org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted")))
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
  (setq org-agenda-custom-commands
        '(("c" "Comprehensive view"
           ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
                        (org-agenda-span 'day)
                        (org-agenda-ndays 1)
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "+0d")
                        (org-agenda-todo-ignore-deadlines nil)))
            (agenda "" ((org-agenda-overriding-header "Upcoming week:")
                        (org-agenda-span 'week)
                        (org-agenda-start-day "+1d")
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled 'todo '("WAITING" "DONE")))
                        ;;(org-agenda-prefix-format '((agenda . " %-12:c%?-12t %s%b ")))
                        ))
            (todo "TODO"
                  ((org-agenda-overriding-header "Unscheduled tasks:")
                   (org-agenda-todo-ignore-deadlines 'all)
                   (org-agenda-todo-ignore-scheduled 'all)))
            (todo "ONGOING"
                  ((org-agenda-overriding-header "Ongoing tasks:")
                   (org-agenda-todo-ignore-deadlines 'all)
                   (org-agenda-todo-ignore-scheduled 'all)))
  
            (todo "WAITING|SHELVED"
                  ((org-agenda-overriding-header "Waiting or shelved tasks:")
                   (org-agenda-todo-ignore-deadlines 'all)
                   (org-agenda-todo-ignore-scheduled 'all)))
            ))))
  (use-package org-super-agenda
    :straight t
    :init
    (org-super-agenda-mode))
  (setq org-super-agenda-groups
        '((:discard (:category "fun"))
          (:auto-category t)))
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
    ;;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-fetch)))
    )
  
  (setq calendar-latitude 149.13)
  (setq calendar-longitude -35.28)
  (setq calendar-location-name "Canberra")
  (use-package org-crypt
    :config
    (setq org-crypt-key "D93ED1F5")
    (setq org-crypt-disable-auto-save t))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((latex . t)
     (dot . t)
     (emacs-lisp . t)
     (python . t)
     (shell . t)
     (org . t)
     (sass . t)))
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (use-package org-journal
    :straight t
    :config
    (setq org-journal-dir (concat org-default-directory "journal/"))
    (setq org-journal-enable-encryption t)
    (setq org-journal-file-format "%Y-%m-%d.org")
    )
  (use-package org-ref
    :straight t
    :config
    (setq
     org-ref-default-bibliography '("~/Bibliography/math.bib")
     org-ref-pdf-directory "~/Papers/"
     org-ref-completion-library 'org-ref-ivy-cite
     org-ref-notes-function 'org-ref-notes-function-many-files))
  (use-package citeproc
    :straight t)
  (setq org-cite-global-bibliography '("/home/asilata/Bibliography/math.bib"))
  (use-package org-roam
    :hook (after-init . org-roam-setup)
    :straight (:host github :repo "org-roam/org-roam")
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n g" . org-roam-graph)
           ("C-c n t" . org-roam-dailies-capture-today)
           ("C-c n i" . org-roam-node-insert))
    :custom
    (org-roam-capture-templates
     (let* ((org-roam-file-name-format "%<%Y%m%d%H%M%S>-${slug}.org")
            (org-roam-common-head "#+title: ${title}\n#+created: %U\n")
            (org-roam-notes-head "\n* Comments\n\n* References\n\n")
            (orb-title-format "${title} (${author})")
            (orb-file-name-format "Bibnotes/${citekey}.org")
            (orb-front-matter "#+created: %U\n\n")
            (orb-common-head (concat "#+title: " orb-title-format "\n" orb-front-matter)))
       `(("d" "default" plain "* Notes\n%?"
          :target (file+head ,org-roam-file-name-format ,(concat org-roam-common-head org-roam-notes-head))
          :unnarrowed t)
         ("l" "link" plain "* Notes\n"
          :target (file+head ,org-roam-file-name-format ,(concat org-roam-common-head org-roam-notes-head))        
          :immediate-finish t)
         ("p" "person" plain "%?"
          :target (file+head "People/${slug}.org" ,org-roam-common-head)
          :immediate-finish t)
         ("r" "ref" plain "* Notes\n%?"
          :target (file+head ,orb-file-name-format ,orb-common-head)
          :unnarrowed t)
         )))
  (org-roam-dailies-directory "Dailies/")
  (org-roam-dailies-capture-templates
   (let* ((daily-title-format "%<%Y-%m-%d>")
          (daily-front-matter (concat "#+title: " daily-title-format "\n#+created: %U\n")))
     `(("d" "daily" entry "* %?"
        :if-new (file+head ,daily-title-format ,daily-front-matter)
        :olp ("Notes"))
       ("c" "calculation" entry "* %?"
        :if-new (file+head ,daily-title-format ,daily-front-matter)
        :olp ("Calculations"))
       ("m" "meeting" entry "* MEETING :meeting\n  - with :: %^{Meeting with}\n  %? "
        :if-new (file+head ,daily-title-format ,daily-front-matter)        
        :olp ("Meetings")
        :clock-in t :clock-resume t))))
  (org-roam-tag-sources '(prop all-directories))
  :config
  (require 'org-roam-protocol)
  :init
  (setq org-roam-v2-ack t))
  (use-package org-roam-ui
    :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
      :after org-roam
      :hook (after-init . org-roam-ui-mode)
      :config
      (setq org-roam-ui-sync-theme t
            org-roam-ui-follow t
            org-roam-ui-update-on-save t
            org-roam-ui-open-on-start t))
  (use-package org-roam-bibtex
    :after org-roam ivy-bibtex
    :straight t
    :bind (:map org-mode-map
                (("C-c n a" . orb-note-actions)))
    :custom
    (org-roam-bibtex-mode 1))
  (use-package deft
    :straight t
    :after org-roam
    :bind ("C-c n d" . deft)
    :custom
    (deft-recursive t)
    (deft-use-filter-string-for-filename t)
    (deft-default-extension "org")
    (deft-directory org-roam-directory)
    )
  (use-package org-brain
    :straight t
    :init
    (setq org-brain-path (concat org-default-directory "Brain/"))
    :config
    (setq org-track-id-globally t)
    (setq org-id-locations-file (concat user-emacs-directory ".org-id-locations"))
    (push '("b" "Brain" plain (function org-brain-goto-end)
            "* %i%?" :empty-lines 1)
          org-capture-templates)
    (setq org-brain-visualize-default-choices 'all)
    (setq org-brain-title-max-length 12)
    (setq org-brain-include-file-entries t
          org-brain-file-entries-use-title t)
    (setq org-brain-file-from-input-function
          (lambda (x) (if (cdr x) (car x) (concat org-brain-path "default"))))
    )
  (setq org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . find-file-other-window)
          (wl . wl-other-frame)))
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
  
  (eval-after-load 'org-list
    '(add-hook 'org-checkbox-statistics-hook (function auto-done-checkboxes)))
  (let ((org-private-settings (concat user-opt-directory "private/org-private-settings.el")))
    (if (file-exists-p org-private-settings)
        (load org-private-settings)))
  (add-hook 'org-mode-hook
            (lambda ()
              (visual-line-mode 1)
              (org-cdlatex-mode 1)))
  (setq org-use-speed-commands t))

(use-package org-modern
  :straight t
  :config
  (add-hook 'org-mode-hook #'org-modern-mode))

(use-package ox-reveal
  :straight t
  :config
  (use-package htmlize :straight t)
  (setq org-reveal-root (concat "file://" (expand-file-name "~/opt/revealjs"))))

(use-package org-chef
  :straight t)

(use-package org-mime
  :straight t)

(use-package org-noter
  :straight t)

(use-package org-pdfview
  :straight t
  :config
  (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link)))))

;; (use-package calfw
;;   :straight t)
;; (use-package calfw-org
;;   :straight t)

(use-package org-download
  :straight t
  :custom
  (org-download-screenshot-method "spectacle -b -n -r -o %s")
  (org-download-image-dir "assets/"))

(use-package zenburn-theme
  :straight t
  :config
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     `(mu4e-replied-face ((t (:foreground ,zenburn-fg))))
     `(hl-line-face ((t (:background ,zenburn-bg-2))))
     `(hl-line ((t (:background ,zenburn-bg-2))))))
  (load-theme 'zenburn t)
  )

(use-package all-the-icons
  :straight t)

(use-package all-the-icons-dired
  :straight t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ivy
  :straight t
  :config
  (all-the-icons-ivy-setup))

(use-package rainbow-mode
  :straight t
  :mode "\\.\\(el|scss|sass\\)")

;; (use-package dired-sidebar
;;   :straight t
;;   :bind (("C-x C-d" . dired-sidebar-toggle-sidebar))
;;   :commands
;;   (dired-sidebar-toggle-sidebar))

(setq dired-listing-switches "-alh")

(use-package dired-narrow
  :straight t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))
(use-package dired-collapse
  :straight t
  :custom
  (dired-collapse-mode t))

(use-package dired-subtree
  :straight t
  :bind
  (:map dired-mode-map
        ("i" . dired-subtree-toggle)))

(use-package treemacs
  :straight t
  :bind (:map global-map
              ([f8] . treemacs-select-window))
  :config
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))

(use-package treemacs-magit
  :after treemacs magit
  :straight t)

(global-prettify-symbols-mode 1)
(add-hook 'org-mode-hook
          (lambda ()
            (push '("[ ]" . "⬜") prettify-symbols-alist)
            (push '("[X]" . "✔") prettify-symbols-alist)
            (push '("TODO" . "⬜") prettify-symbols-alist)
            (push '("DONE" . "✔") prettify-symbols-alist)
            (push '("CANCELLED" . "✘") prettify-symbols-alist)
            (push '("WAITING" . "⏳") prettify-symbols-alist)
            (push '("SHELVED" . "⭮") prettify-symbols-alist)
            (push '("BORROWED" . "💰") prettify-symbols-alist)
            (push '("RETURNED" . "✔") prettify-symbols-alist)
            (push '("ONGOING" . "🏃") prettify-symbols-alist)))

;; (use-package beacon-mode
;;   :straight (:host github :repo "Malabarba/beacon")
;;   :config
;;   (beacon-mode 1))

(use-package highlight-indent-guides
  :straight t
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package smartparens
  :straight t
  :config
  (show-paren-mode 1)
  (setq show-paren-style 'parenthesis)
  (use-package smartparens-config)
  (smartparens-global-mode 1))

(use-package parinfer
  :straight t
  :init
  (progn
    (setq parinfer-extensions
          '(defaults))))


(electric-indent-mode 1)
(electric-layout-mode 1)
(global-hl-line-mode 1)

(use-package volatile-highlights
  :straight t
  :config (volatile-highlights-mode 1))

(setq-default indent-tabs-mode nil)     ;Don't use tabs to indent...
(setq-default tab-width 8)         ;...but maintain correct appearance

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." )

(use-package god-mode
  :straight t
  :bind ("<escape>" . god-mode-all)
  :config
  (god-mode)
  (add-hook 'post-command-hook #'ab/god-mode-update-cursor-type))

(defun ab/god-mode-update-cursor-type ()
  (setq cursor-type
        (if (or god-local-mode buffer-read-only) 'bar 'box)))

(use-package ryo-modal
  :straight t
  :commands ryo-modal-mode
  :bind ("C-c SPC" . ryo-modal-mode)
  :config
  (ryo-modal-keys
   (:mode 'org-mode)
   ("n" org-next-visible-heading)
   ("p" org-previous-visible-heading))
  )

(use-package objed
  :straight t)

(use-package multiple-cursors
  :straight t
  :bind (("C-c m c" . mc/edit-lines)
         ("C-c m n" . mc/mark-next-like-this)
         ("C-c m p" . mc/mark-previous-like-this)
         ("C-c m a" . mc/mark-all-like-this)))

(defun toggle-comment-line-or-region (&optional arg)
  "Toggle commenting on current line or region (ARG), then go to the next line."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
  (forward-line))

(use-package outshine
  :straight t
  :init
  (defvar outline-minor-mode-prefix "\M-#")
  :config
  (setq outshine-use-speed-commands t)
  (add-hook 'prog-mode-hook 'outshine-mode)
  (add-hook 'LaTeX-mode-hook 'outshine-mode))

(use-package browse-kill-ring
  :straight t)

;; (use-package avy
;;   :straight t
;;   :bind (("M-s" . avy-goto-char-timer)))

;; (use-
;;  package ivy
;;  :straight t
;;  :bind (("C-c C-r" . ivy-resume)
;;         ("C-c v" . ivy-push-view)
;;         ("C-c V" . ivy-pop-view))
;;  :config
;;  (use-package ivy-hydra :straight t)
;;  (ivy-mode 1)
;;  (setq ivy-use-virtual-buffers t))

;; (use-package swiper
;;   :straight t
;;   :bind (("C-s" . swiper-isearch)))

;; (use-package counsel
;;   :straight t
;;   :bind (("M-x" . counsel-M-x)
;;          ("C-x C-f" . counsel-find-file)
;;          ("C-x C-g" . counsel-git)
;;          ("C-h v" . counsel-describe-variable)
;;          ("C-h f" . counsel-describe-function)))

;; (use-package ivy-prescient
;;   :straight t
;;   :config
;;   (ivy-prescient-mode))

;; (use-package ivy-rich
;;   :straight t
;;   :after ivy
;;   :config
;;   (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich--ivy-switch-buffer-transformer)
;;   (ivy-rich-mode 1))

;; (use-package wgrep
;;   :straight t
;;   :after ivy)

;; (use-package selectrum
;;   :straight t
;;   :config
;;   (use-package selectrum-prescient :straight t)
;;   :custom
;;   (selectrum-prescient-mode +1)
;;   (prescient-persist-mode +1)
;;   :init
;;   (selectrum-mode +1))

(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ;;("C-c h" . consult-history)
         ;;("C-c m" . consult-mode-command)
         ;;("C-c b" . consult-bookmark)
         ;;("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . switch-to-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ;;("M-#" . consult-register-load)
         ;;("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;;("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ;; ("M-s f" . consult-find)
         ;; ("M-s F" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ;; ("M-s G" . consult-git-grep)
         ;; ("M-s r" . consult-ripgrep)
         ;; ("M-s l" . consult-line)
         ("C-s"   . consult-line)                    ;; isearch alternative
         ;; ("M-s L" . consult-line-multi)
         ;; ("M-s m" . consult-multi-occur)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ;; ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  ;;(setq consult-project-root-function
  ;; (lambda ()
  ;;   (when-let (project (project-current))
  ;;     (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
)

(use-package consult-reftex
  :straight (:host github :repo "karthink/consult-reftex"))

(use-package citar
  :straight t
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  (citar-bibliography '("~/Bibliography/math.bib")))

(use-package marginalia
  :straight t
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :straight t
  :init
  (savehist-mode))

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package which-key :straight t
  :config
  (which-key-mode 1))

(use-package smart-mode-line
  :straight t
  :config
  (progn (sml/setup)))

(global-set-key [f1]          'revert-buffer)
(global-set-key [f2]          'goto-line)
(global-set-key [f5]          'query-replace)
(global-set-key [home]        'beginning-of-line)
(global-set-key [end]         'end-of-line)
(global-set-key [C-home]      'beginning-of-buffer)
(global-set-key [C-end]       'end-of-buffer)
(global-set-key (kbd "C-;")   'toggle-comment-line-or-region)
;; (global-set-key (kbd "C-x C-j") 'jekyll-new-post)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(setq backup-by-copying t
      delete-old-versions t
      kept-old-versions 2
      kept-new-versions 2
      version-control t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(defun delete-old-backup-files ()
  "Delete backup files that have not been accessed in a month."
  (let ((month (* 60 60 24 7 30))
        (current (float-time (current-time))))
    (dolist (file (directory-files temporary-file-directory t))
      (when (and (backup-file-name-p file)
                 (> (- current (float-time (nth 5 (file-attributes file))))
                    month))
        (message "%s" file)
        (delete-file file)))))
(delete-old-backup-files)

(use-package midnight)

(use-package company
  :straight t
  :config
  (global-company-mode 1))

(use-package company-prescient
  :straight t
  :config
  (company-prescient-mode))

(use-package magit
  :straight t
  :bind (([f6] . magit-status)))

(use-package diff-hl
  :straight t
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode 1))

(defun ab/normalise-bib ()
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   "bibtool -r ~/Bibliography/rules.rsc" t t "*Messages*"))

(use-package bibretrieve
  :straight (:host github :repo "asilata/bibretrieve")
  :config
  (add-hook
   'bibretrieve-pre-write-bib-items-hook
   'ab/normalise-bib))

(use-package ivy-bibtex
  :straight t
  :config
  (setq ivy-re-builders-alist '((ivy-bibtex . ivy--regex-ignore-order)
                                (t . ivy--regex-plus)))
  (setq bibtex-completion-notes-path "~/Org/Roam/Bibnotes")
  (setq bibtex-completion-bibliography '("~/Bibliography/math.bib"))
  (setq bibtex-completion-library-path '("~/Papers"))
  (ivy-set-display-transformer
   'org-ref-ivy-insert-cite-link
   'ivy-bibtex-display-transformer))

(use-package auctex
  :straight t
  :init
  (use-package auctex-latexmk
    :straight t
    :config
    (auctex-latexmk-setup))
  :defer t
  :bind (([f7] . TeX-error-overview))
  :config
  (use-package smartparens-latex)
  (set-default 'preview-scale-function 2))

(use-package reftex :straight t
             :config
             (setq reftex-default-bibliography "~/Bibliography/math.bib"))

(use-package cdlatex
  :straight t
  :custom
  (cdlatex-takeover-parenthesis nil)
  (cdlatex-math-symbol-alist '((?> ("\\to" "\\Longrightarrow"))))
  (cdlatex-math-modify-alist '((?b "\\mathbb" nil t nil nil)
                               (?f "\\mathfrak" "\\frak" t nil nil)
                               (?o "\\operatorname" nil t nil nil)
                               (?s "\\mathscr" "\\textscr" t nil nil))))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (TeX-global-PDF-mode 1)
            (flyspell-mode 1)
            (auto-fill-mode 0)
            (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
            (TeX-source-correlate-mode 1)
            (visual-line-mode 1)
            (yas-minor-mode 0)
            (reftex-mode 1)
            (cdlatex-mode 1)
            ))

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

(use-package counsel-projectile
  :straight t
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (counsel-projectile-mode 1))

(use-package dumb-jump
  :straight t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back))
  )

(use-package conf-mode
  :mode ("rc$"))

(use-package dokuwiki-mode
  :straight t)

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(flycheck-define-checker vale
  "A checker for prose"
  :command ("vale" "--output" "line"
            source)
  :standard-input nil
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":" (id (one-or-more (not (any ":")))) ":" (message) line-end))
  :modes (markdown-mode org-mode text-mode)
  )
(add-to-list 'flycheck-checkers 'vale 'append)

(use-package graphviz-dot-mode
  :straight t
  :config
  (use-package company-graphviz-dot)
  (setq graphviz-dot-indent-width 4))

(use-package haskell-mode
  :straight t
  :config
  (add-hook 'haskell-mode-hook
            'turn-on-haskell-indentation))

(use-package lean-mode
  :straight t
  :config
  (setq lean-rootdir "~/opt/lean-nightly-linux"))

(use-package lisp-mode
  :init
  (progn
    (use-package eldoc
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
    (font-lock-add-keywords 'emacs-lisp-mode
                            '(("use-package" . font-lock-keyword-face)))))

(load "emacs-Macaulay2.el" t)

(use-package markdown-mode
  :straight t
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\|txt\\)$" . markdown-mode)
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (orgtbl-mode 1))))

(use-package ox-tufte :straight t)

(use-package sage-shell-mode
  :straight t
  :config
  (setq sage-shell:sage-executable (substring (shell-command-to-string "which sage") 0 -1))
  (sage-shell:define-alias)
  (setq sage-shell:use-prompt-toolkit t))

(use-package ob-sagemath
  :straight t
  :config
  ;; Ob-sagemath supports only evaluating with a session.
  (setq org-babel-default-header-args:sage '((:session . t)
                                             (:results . "output")))
  ;; C-c c for asynchronous evaluating (only for SageMath code blocks).
  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c x") 'ob-sagemath-execute-async)))

(use-package scratch
  :straight t)

(use-package scss-mode
  :straight t
  ;; :mode "\\.\\(scss|sass\\)"
  :config
  (add-hook 'scss-mode-hook
            (lambda ()
              (setq scss-compile-at-save nil))))

(add-to-list 'load-path "/usr/share/Singular/emacs")
(autoload 'singular "singular"
  "Start Singular using default values." t)
(autoload 'singular-other "singular"
  "Ask for arguments and start Singular." t)
(setq auto-mode-alist (cons '("\\.sing\\'" . c++-mode) auto-mode-alist))

(use-package textile-mode
  :straight t
  :mode ("\\.textile\\'" . textile-mode)
  :config
  (add-hook 'textile-mode-hook
            'turn-on-orgtbl))

(use-package web-mode
  :straight t
  :mode ("\\.html?\\'" . web-mode)
  :config
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-pairing t))

(use-package yaml-mode
  :straight t)

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

;; (defun jekyll-new-post (title directory)
;;   "Create a new post titled TITLE in DIRECTORY."
;;   (interactive "MEnter post title: \nDEnter directory to save in: ")
;;   (let ((post-file-name (expand-file-name (format "%s/%s.markdown"
;;                                                   directory
;;                                                   (concat (format-time-string "%Y-%m-%d-") (downcase (replace-regexp-in-string " " "-" title)))))))
;;     (find-file post-file-name)
;;     (insert "---\n")
;;     (insert (format "title: \"%s\"\n" title))
;;     (insert (format "date: \"%s\"\n" (format-time-string "%Y-%m-%d %H:%M:%S %z")))
;;     (insert "---\n\n")
;;     (markdown-mode)))

(use-package mu4e
  :straight (:files (:defaults "build/mu4e/*"))
  :defer nil
  :custom   (mu4e-mu-binary (expand-file-name "build/mu/mu" (straight--repos-dir "mu")))
  :bind (("C-c p" . mml-secure-message-sign-pgpmime))
  :config
  (require 'mu4e-contrib)
  (setq mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval (* 60 15))
  (setq mu4e-change-filenames-when-moving t)
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-service 587
        message-kill-buffer-on-exit t)
  (let ((mu4e-private-settings (concat user-opt-directory "private/mu4e-private-settings.el")))
    (if (file-exists-p mu4e-private-settings)
        (load mu4e-private-settings)))
  (setq mu4e-index-cleanup t
        mu4e-index-lazy-check nil)
  (setq mu4e-headers-date-format "  %_d %b %y"
        mu4e-headers-time-format "%_l:%M %P"
        mu4e-headers-fields
        '((:human-date . 12)
          (:flags . 4)
          (:size . 8)
          (:from-or-to . 20)
          (:thread-subject . nil))
        mu4e-headers-skip-duplicates t)
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-split-view 'horizontal)
  (add-to-list 'display-buffer-alist
               `(,(regexp-quote mu4e-main-buffer-name)
                 display-buffer-same-window))
  (setq mu4e-view-show-images t)
  (setq mu4e-html2text-command 'mu4e-shr2text)
  (setq mu4e-attachment-dir "/tmp")
  (add-hook 'mu4e-view-mode-hook
            (lambda ()
              (local-set-key (kbd "<tab>") 'shr-next-link)
              (local-set-key (kbd "<backtab>") 'shr-previous-link)))
  (setq mu4e-compose-format-flowed t)
  (setq mu4e-bookmarks
        '((:name  "Reasonable recent messages"
                  :query "date:6m..now AND to:asilata AND (maildir:/ANU/INBOX OR maildir:/Gmail/INBOX OR tag:\\\\Important) AND NOT flag:list"
                  :key ?r
                  :favorite t)
          (:name "Flagged"
                 :query "flag:flagged AND NOT (flag:trashed  OR maildir:\"ANU/Deleted Items\" OR maildir:\"/Gmail/[Gmail]/Bin\")"
                 :key ?f)          
          (:name "Today and untrashed"
                 :query "date:today..now AND NOT (flag:trashed  OR maildir:\"ANU/Deleted Items\" OR maildir:\"/Gmail/[Gmail]/Bin\")"
                 :key ?t)
          (:name "Last week and untrashed"
                 :query "date:7d..now AND NOT (flag:trashed  OR maildir:\"ANU/Deleted Items\" OR maildir:\"/Gmail/[Gmail]/Bin\")"
                 :key ?w)            
          (:name "Mailing lists"
                 :query "flag:list AND (maildir:/ANU/INBOX OR maildir:/Gmail/INBOX)"
                 :key ?l)
          (:name "Messages with images" :query "mime:image/*" :key ?p :hide t)))
  (add-hook 'mu4e-headers-found-hook
            (lambda () (setq truncate-lines t)))
  
  (add-to-list 'mu4e-view-actions
               '("retag message" . mu4e-action-retag-message) t)
  (add-to-list 'mu4e-headers-actions
               '("retag message" . mu4e-action-retag-message) t)
  
  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              (turn-off-auto-fill)
              (visual-line-mode 1)
              (use-hard-newlines -1)))
  
  (add-hook 'mu4e-mark-execute-pre-hook
    (lambda (mark msg)
      (cond ((member mark '(refile trash)) (mu4e-action-retag-message msg "-\\\\Inbox"))
            ((equal mark 'flag) (mu4e-action-retag-message msg "\\\\Starred"))
            ((equal mark 'unflag) (mu4e-action-retag-message msg "-\\\\Starred")))))
  (setq mu4e-maildir-shortcuts
        '((:maildir "/ANU/INBOX" :key ?a)
          (:maildir "/Gmail/INBOX" :key ?g)
          (:maildir "/MIT" :key ?m)))
  (require 'mu4e-org)
  (setq org-mu4e-link-query-in-headers-mode nil)
  (use-package mu4e-alert
    :straight t
    :config
    (setq mu4e-alert-interesting-mail-query
          (concat
           "date:6m..now"
           "flag:unread"         
           "AND to:asilata"
           "AND (maildir:/ANU/INBOX OR maildir:/Gmail/INBOX OR tag:\\\\\\\\Important)"
           ))
    (mu4e-alert-set-default-style 'libnotify)
    (setq mu4e-alert-email-notification-types '(subjects))
    (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
    (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
    )
  )

(setq epg-gpg-program "gpg2")
(setq epg-pinentry-mode 'loopback)

(use-package elfeed
  :straight t
  :config
  (require 'elfeed-link)
  (use-package elfeed-org :straight t)
  (elfeed-org)
  (setq rmh-elfeed-org-files '("~/.elfeed/elfeed.org"))
  (setq elfeed-search-title-max-width 1000)
  (setq elfeed-use-curl nil))

(use-package pdf-tools
  :straight t
  :bind (:map pdf-view-mode-map
              (("C-s" . isearch-forward)))
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width))

(use-package pdf-tools-org
  :straight (:host github :repo "machc/pdf-tools-org"))

(when (file-exists-p local-config-directory)
  (mapc 'load (directory-files local-config-directory 't "^[^#].*el$")))

(setq custom-file (concat local-config-directory "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

(byte-recompile-directory user-emacs-directory)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Local Variables:
;; byte-compile-warnings: (not free-vars callargs cl-functions)  
;; End:
