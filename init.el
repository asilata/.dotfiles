;;==============================================================================
;; Emacs initialization file
;; (Inspired by emacs-prelude)
;;==============================================================================

;;; Initial boilerplate
(require 'cl-lib)

(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"
    "Root directory of the emacs configuration."))
(defvar user-opt-directory (concat user-emacs-directory "opt/")
  "User-installed Emacs packages go here.")
(defvar local-config-directory (concat user-emacs-directory "local/")
  "Machine-local configuration files go here.")

;;;; Create these directories if they don't exist.
(mapc (lambda (dir)
        (unless (file-exists-p dir)
          (make-directory dir)))
      (list user-emacs-directory user-opt-directory local-config-directory))

(add-to-list 'load-path user-opt-directory)
(let ((default-directory user-opt-directory))
  (normal-top-level-add-subdirs-to-load-path))

;;;; To fix cursor colour bug...
(setq inhibit-x-resources 't)

;;; Package management
;;;; Straight
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

;;;; Install use-package if not installed
(straight-use-package 'use-package)

;;; Window management
;;;; EXWM
(server-start)
(use-package exwm
  :straight t
  :config
  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(1 "DP-1"))
  (add-hook 'exwm-randr-screen-change-hook
            (defun ab/exwm-randr ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output eDP-1 --output DP-1")))
  (exwm-randr-enable)  
  (let ((exwm-config-file (concat user-opt-directory "exwm-config-file.el")))
    (if (file-exists-p exwm-config-file)
        (load exwm-config-file)))
  (exwm-enable))



(use-package exwm-edit
  :straight t)

;;;; Ace-window
(use-package ace-window
  :straight t
  :bind (("C-x o" . ace-window))
  :custom
  (aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?i ?d))
  (aw-minibuffer-flag t)
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
  )

;;;; Winner-mode
(use-package winner
  :straight t
  :bind (("M-<left>" . winner-undo)
         ("M-<right>" . winner-redo))
  :config
  (winner-mode t))

;;;; Flip window
(defun flip-window ()
  (interactive)
  (let ((win (get-mru-window t t t)))
    (if win
        (progn
          (select-frame-set-input-focus (window-frame win))
          (select-window win))
      (mode-line-other-buffer))))
;;;; Tab-bar-mode
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

;;; Org-mode
;;;; Org
(use-package org
  :after counsel
  :straight t
  :bind (("C-c a" . org-agenda)
         (:map org-mode-map
               ("C-c C-j" . counsel-outline)))
  :config
  (use-package org-bullets :straight t)
  (let ((org-config-file (concat user-opt-directory "org-mode-config.el")))
    (if (file-exists-p org-config-file)
        (load org-config-file)))
  (add-hook 'org-mode-hook
            (lambda ()
              (visual-line-mode 1)
              (org-bullets-mode 1)))
  (setq org-use-speed-commands t))

;;;; Org-reveal
(use-package ox-reveal
  :straight t
  :config
  (use-package htmlize :straight t)
  (setq org-reveal-root (concat "file://" (expand-file-name "~/opt/revealjs"))))

;;;; Org-chef
(use-package org-chef
  :straight t)

;;;; Org-mime
(use-package org-mime
  :straight t)

;;;; Org-noter
(use-package org-noter
  :straight t)

;;;; Org-pdfview
(use-package org-pdfview
  :straight t
  :config
  (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link)))))

;;;; Calfw
(use-package calfw
  :straight t)
(use-package calfw-org
  :straight t)
;;; Buffer customizations
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
;;;; Auto-revert buffers from files
(setq global-auto-revert-mode 1)
(setq auto-revert-interval 3600)

;;;; Uniquify buffer names
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*" ; don't muck with special buffers
        )
  )

;;;; iBuffer mode
(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

;;; Colour themes and prettification
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
  :mode "\\.\\(el|scss|sass\\)")

(use-package dired-sidebar
  :straight t
  :bind (("C-x C-d" . dired-sidebar-toggle-sidebar))
  :commands
  (dired-sidebar-toggle-sidebar))

(use-package treemacs
  :straight t
  :bind (:map global-map
              ([f8] . treemacs-select-window))
  :custom
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))

(use-package treemacs-magit
  :after treemacs magit
  :straight t)

(use-package dired-narrow
  :straight t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;;;; Prettify symbols
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

;;;; Beacon mode
;; (use-package beacon-mode
;;   :straight (:host github :repo "Malabarba/beacon")
;;   :config
;;   (beacon-mode 1))

;;; Editing
(use-package smartparens
  :straight t
  :config
  (show-paren-mode 1)
  (setq show-paren-style 'parenthesis)
  (use-package smartparens-config)
  (smartparens-global-mode 1)
  )

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

;;;; Objed
(use-package objed
  :straight t)

;;;; Multiple cursors
(use-package multiple-cursors
  :straight t
  :bind (("C-c m c" . mc/edit-lines)
         ("C-c m n" . mc/mark-next-like-this)
         ("C-c m p" . mc/mark-previous-like-this)
         ("C-c m a" . mc/mark-all-like-this)))

;;;; Toggle comments function
(defun toggle-comment-line-or-region (&optional arg)
  "Toggle commenting on current line or region (ARG), then go to the next line."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
  (forward-line))


;;;; Outshine mode
(use-package outshine
  :straight t
  :init
  (defvar outline-minor-mode-prefix "\M-#")
  :config
  (setq outshine-use-speed-commands t)
  (add-hook 'prog-mode-hook 'outshine-mode)
  (add-hook 'LaTeX-mode-hook 'outshine-mode))

;;;; Browse kill ring
(use-package browse-kill-ring
  :straight t)
;;; Minibuffer and search
;;;; Ivy, etc
(use-package avy
  :straight t
  :bind (("M-s" . avy-goto-char-timer)))

(use-package ivy
  :straight t
  :bind (("C-c C-r" . ivy-resume)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view))
  :config
  (use-package ivy-hydra :straight t)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))

(use-package swiper
  :straight t
  :bind (("C-s" . swiper-isearch)))

(use-package counsel
  :straight t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-g" . counsel-git)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)))

(use-package ivy-prescient
  :straight t
  :config
  (ivy-prescient-mode))

(use-package ivy-rich
  :straight t
  :after ivy
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich--ivy-switch-buffer-transformer)
  (ivy-rich-mode 1))

(use-package wgrep
  :straight t
  :after ivy)

;;;; Other goodies
(use-package which-key :straight t
  :config
  (which-key-mode 1))

(use-package smart-mode-line
  :straight t
  :config
  (progn (sml/setup)))

;;; Global keybindings
(global-set-key [f1]          'revert-buffer)
(global-set-key [f2]          'goto-line)
(global-set-key [f5]          'query-replace)
(global-set-key [home]        'beginning-of-line)
(global-set-key [end]         'end-of-line)
(global-set-key [C-home]      'beginning-of-buffer)
(global-set-key [C-end]       'end-of-buffer)
(global-set-key (kbd "C-;")   'toggle-comment-line-or-region)
(global-set-key (kbd "C-x C-j") 'jekyll-new-post)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;; Backup and cleanup
;;;; Back up files
(setq backup-by-copying t
      delete-old-versions t
      kept-old-versions 2
      kept-new-versions 2
      version-control t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;;; Delete old backup files
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

;;;; Clean up old buffers.
(use-package midnight)


;;; Completion
(use-package company
  :straight t
  :config
  (global-company-mode 1))

(use-package company-prescient
  :straight t
  :config
  (company-prescient-mode))

;;; Git
(use-package magit
  :straight t
  :bind (([f6] . magit-status)))

;;;; Diff-hl
(use-package diff-hl
  :straight t
  :custom
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode 1))

;;; Programming
;;;; Projects and jumping
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
  :config
  (setq dumb-jump-selector 'ivy))

;;;; Assorted packages
;;;;; Conf-mode
(use-package conf-mode
  :mode ("rc$"))

;;;;; Dokuwiki-mode
(use-package dokuwiki-mode
  :straight t)

;;;;; Flycheck
(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;;;; Graphviz
(use-package graphviz-dot-mode
  :straight t
  :config
  (use-package company-graphviz-dot)
  (setq graphviz-dot-indent-width 4))

;;;;; Haskell
(use-package haskell-mode
  :straight t
  :config
  (add-hook 'haskell-mode-hook
            'turn-on-haskell-indentation))

;;;;; LaTeX etc

(defun ab/normalise-bib ()
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   "bibtool -r ~/Bibliography/rules.rsc" t t "*Messages*"))

(use-package auctex
  :straight t
  :init
  (use-package bibretrieve
    :straight (:host github :repo "asilata/bibretrieve")
    :config
    (add-hook
     'bibretrieve-pre-write-bib-items-hook
     'ab/normalise-bib
     ))
  (use-package auctex-latexmk
    :straight t
    :config
    (auctex-latexmk-setup))
  :defer t
  :bind (([f7] . TeX-error-overview))
  :config
  (use-package reftex :straight t
    :config
    (setq reftex-default-bibliography "~/Bibliography/math.bib"))
  (use-package smartparens-latex)
  (set-default 'preview-scale-function 2))

(use-package cdlatex
  :straight t)

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
            ))

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

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
   'ivy-bibtex-display-transformer)
  )

;;;;; Lean
(use-package lean-mode
  :straight t
  :config
  (setq lean-rootdir "~/opt/lean-nightly-linux"))

;;;;; Lisp
(use-package lisp-mode
  :init
  (progn
    (use-package eldoc
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
    (font-lock-add-keywords 'emacs-lisp-mode
                            '(("use-package" . font-lock-keyword-face)))))

;;;;; Macaulay 2
(load "emacs-Macaulay2.el" t)

;;;;; Markdown
(use-package markdown-mode
  :straight t
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\|txt\\)$" . markdown-mode)
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (orgtbl-mode 1))))

;;;;; Ox-tufte
(use-package ox-tufte :straight t)
;;;;; Sage
(use-package sage-shell-mode
  :straight t
  :config
  (setq sage-shell:sage-executable (substring (shell-command-to-string "which sage") 0 -1))
  (sage-shell:define-alias)
  (setq sage-shell:use-prompt-toolkit t))

;;;;; Scratch
(use-package scratch
  :straight t)
;;;;; SCSS
(use-package scss-mode
  :straight t
  :mode "\\.\\(scss|sass\\)"
  :config
  (add-hook 'scss-mode-hook
            (lambda ()
              (setq scss-compile-at-save nil))))

;;;;; Singular
(add-to-list 'load-path "/usr/share/Singular/emacs")
(autoload 'singular "singular"
  "Start Singular using default values." t)
(autoload 'singular-other "singular"
  "Ask for arguments and start Singular." t)
(setq auto-mode-alist (cons '("\\.sing\\'" . c++-mode) auto-mode-alist))

;;;;; Textile
(use-package textile-mode
  :straight t
  :mode ("\\.textile\\'" . textile-mode)
  :config
  (add-hook 'textile-mode-hook
            'turn-on-orgtbl))

;;;;; Web-mode
(use-package web-mode
  :straight t
  :mode ("\\.html?\\'" . web-mode)
  :config
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-pairing t))

;;;;; YAML
(use-package yaml-mode
  :straight t)

;;;;; Yasnippet
(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

;;;; Jekyll stuff (new post function, modified from hyde-mode's version)
(defun jekyll-new-post (title directory)
  "Create a new post titled TITLE in DIRECTORY."
  (interactive "MEnter post title: \nDEnter directory to save in: ")
  (let ((post-file-name (expand-file-name (format "%s/%s.markdown"
                                                  directory
                                                  (concat (format-time-string "%Y-%m-%d-") (downcase (replace-regexp-in-string " " "-" title)))))))
    (find-file post-file-name)
    (insert "---\n")
    (insert (format "title: \"%s\"\n" title))
    (insert (format "date: \"%s\"\n" (format-time-string "%Y-%m-%d %H:%M:%S %z")))
    (insert "---\n\n")
    (markdown-mode)))

;;; Email
;;;; mu4e
(use-package mu4e
  :straight (:host github :repo "djcb/mu" :branch "release/1.6" :files (:defaults "build/mu4e/*"))
  :defer nil
  :custom   (mu4e-mu-binary (expand-file-name "build/mu/mu" (straight--repos-dir "mu")))
  :bind (("C-c p" . mml-secure-message-sign-pgpmime))
  :config
  (require 'mu4e-contrib)
  (let ((mu4e-config-file (concat user-opt-directory "mu4e-config.el")))
    (if (file-exists-p mu4e-config-file)
        (load mu4e-config-file)))
  )

;;;; GPG
(setenv "GPG_AGENT_INFO" nil)
(setq auth-source-debug t)
(setq epg-gpg-program "gpg2")
(require 'epa-file)
(epa-file-enable)
(setq epa-pinentry-mode 'loopback)
(setq epg-pinentry-mode 'loopback)

;;; Elfeed
(use-package elfeed
  :straight t
  :config
  (require 'elfeed-link)
  (use-package elfeed-org :straight t)
  (elfeed-org)
  (setq rmh-elfeed-org-files '("~/.elfeed/elfeed.org"))
  (setq elfeed-search-title-max-width 1000)
  (setq elfeed-use-curl nil))

;;; PDF tools
(use-package pdf-tools
  :straight t
  :bind (:map pdf-view-mode-map
              (("C-s" . isearch-forward)))
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width))

(use-package pdf-tools-org
  :straight (:host github :repo "machc/pdf-tools-org"))


;;; Endnotes
;;;; Load local settings if they exist.
(when (file-exists-p local-config-directory)
  (mapc 'load (directory-files local-config-directory 't "^[^#].*el$")))

;;;; New custom file (for the output of custom-set-variables, etc).
(setq custom-file (concat local-config-directory "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;;;; Recompile all previously byte-compiled files in the directory.
(byte-recompile-directory user-emacs-directory)

;;;; Add package.el just so that package-list-packages includes them
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;;;; Local variables
;; Local Variables:
;; byte-compile-warnings: (not free-vars callargs cl-functions)
;; End:
