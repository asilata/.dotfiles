;;==============================================================================
;; Emacs initialization file
;; (Inspired by emacs-prelude)
;;==============================================================================

(require 'cl)

(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"
    "Root directory of the emacs configuration."))
(defvar user-opt-directory (concat user-emacs-directory "opt/")
  "User-installed Emacs packages go here.")
(defvar local-config-directory (concat user-emacs-directory "local/")
  "Machine-local configuration files go here.")

;; Create these directories if they don't exist.
(mapc (lambda (dir)
        (unless (file-exists-p dir)
          (make-directory dir)))
      (list user-emacs-directory user-opt-directory local-config-directory))

(add-to-list 'load-path user-opt-directory)
(let ((default-directory user-opt-directory))
  (normal-top-level-add-subdirs-to-load-path))

;; To fix cursor colour bug...
(setq inhibit-x-resources 't)

;;; Package management with MELPA (in addition to the GNU archive).
(require 'package)
;(require 'melpa)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(setq url-http-attempt-keepalives nil)

;;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  )
(eval-when-compile
  (require 'use-package))

;;; Auto-update packages every 5 days
(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 5)
   (auto-package-update-maybe))

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

(if (fboundp 'fringe-mode) (fringe-mode 4))
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          (buffer-name))
                                        "%b")))
(use-package rainbow-mode
  :mode "\\.\\(el|scss|sass\\)")

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*" ; don't muck with special buffers
        )
  )

;;; Colour themes and prettification
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t)
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     `(mu4e-replied-face ((t (:foreground ,zenburn-fg))))))
  )

(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup))

(use-package dired-sidebar
  :ensure t
  :bind (("C-x C-d" . dired-sidebar-toggle-sidebar))
  :commands
  (dired-sidebar-toggle-sidebar))

;;; Editing
(use-package smartparens
  :ensure t
  :config
  (show-paren-mode 1)
  (setq show-paren-style 'parenthesis)
  (use-package smartparens-config)
  (smartparens-global-mode 1)
  )

(electric-indent-mode 1)
(electric-layout-mode 1)
(global-hl-line-mode 1)

(use-package volatile-highlights
  :ensure t
  :config (volatile-highlights-mode 1))

(setq-default indent-tabs-mode nil)     ;Don't use tabs to indent...
(setq-default tab-width 8)         ;...but maintain correct appearance

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." )


;;; Minibuffer and search
(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-char-timer)))

(use-package ivy
  :ensure t
  :bind (("C-c C-r" . ivy-resume)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view))
  :config
  (use-package ivy-hydra :ensure t)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper-isearch)))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-g" . counsel-git)))

(use-package smart-mode-line
  :ensure t
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

;; Auto-revert buffers from files
(setq global-auto-revert-mode 1)
(setq auto-revert-interval 3600)

;;; Backup and cleanup
;; Back up files
(setq backup-by-copying t
      delete-old-versions t
      kept-old-versions 2
      kept-new-versions 2
      version-control t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Delete old backup files
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

;; Clean up old buffers.
(use-package midnight)


;; Completion
(use-package company
  :ensure t
  :config
  (global-company-mode 1))

;;; Git
(use-package magit
  :ensure t
  :bind (([f6] . magit-status)))

;;; Programming
(use-package counsel-projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (counsel-projectile-mode 1))

(use-package lean-mode
  :ensure t
  :config
  (setq lean-rootdir "~/opt/lean-nightly-linux"))

(use-package conf-mode
  :mode ("rc$"))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package markdown-mode
  :ensure t
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\|txt\\)$" . markdown-mode)
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (orgtbl-mode 1))))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m c" . mc/edit-lines)
         ("C-c m n" . mc/mark-next-like-this)
         ("C-c m p" . mc/mark-previous-like-this)
         ("C-c m a" . mc/mark-all-like-this)))

(use-package sage-shell-mode
  :ensure t
  :config
  (setq sage-shell:sage-executable (substring (shell-command-to-string "which sage") 0 -1))
  (sage-shell:define-alias)
  (setq sage-shell:use-prompt-toolkit t))

(use-package textile-mode
  :ensure t
  :mode ("\\.textile\\'" . textile-mode)
  :config
  (add-hook 'textile-mode-hook
            'turn-on-orgtbl))

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" . web-mode)
  :config
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-pairing t))

(use-package yaml-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(defun toggle-comment-line-or-region (&optional arg)
  "Toggle commenting on current line or region (ARG), then go to the next line."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
  (forward-line))

;; Jekyll stuff (new post function, modified from hyde-mode's version)
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

;;; Mode-specific hooks
(use-package auctex
  :ensure t
  :init
  (use-package bibretrieve
    :config
    (add-hook
     'bibretrieve-pre-write-bib-items-hook
     (lambda ()
       (shell-command-on-region
        (point-min) (point-max)
        "bibtool -r ~/Bibliography/rules.rsc" t t "*Messages*"))))
  :defer t
  :config
  (use-package reftex :ensure t)
  (use-package smartparens-latex)
  (set-default 'preview-scale-function 2)
  (use-package auctex-latexmk
    :ensure t
    :config
    (auctex-latexmk-setup)))

(add-hook 'LaTeX-mode-hook
          (lambda ()
	    (TeX-global-PDF-mode 1)
            (flyspell-mode 1)
            (auto-fill-mode 0)
            (setq TeX-view-program-list '(("Okular" "okular %o")))
            (setq TeX-view-program-selection '((output-pdf "Okular")))
            (reftex-mode 1)
            (visual-line-mode 1)
            (yas-minor-mode 0)
            ))

(use-package ivy-bibtex
  :ensure t
  :config
  (setq ivy-re-builders-alist '((ivy-bibtex . ivy--regex-ignore-order)
                                (t . ivy--regex-plus)))
  (setq bibtex-completion-bibliography
        '("~/Bibliography/math.bib"))
  (setq bibtex-completion-library-path '("~/Papers"))
  (setq bibtex-completion-pdf-open-function (lambda (p) (call-process "okular" nil 0 nil p))))

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook
            'turn-on-haskell-indentation))

(use-package lisp-mode
  :init
  (progn
    (use-package eldoc
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
    (font-lock-add-keywords 'emacs-lisp-mode
			'(("use-package" . font-lock-keyword-face)))))

(use-package scss-mode
  :ensure t
  :mode "\\.\\(scss|sass\\)"
  :config
  (add-hook 'scss-mode-hook
            (lambda ()
              (setq scss-compile-at-save nil))))


;; Email
(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :demand t
  :bind (("C-c p" . mml-secure-message-sign-pgpmime))
  :config
  (let ((mu4e-config-file (concat user-opt-directory "mu4e-config.el")))
    (if (file-exists-p mu4e-config-file)
        (load mu4e-config-file))))

;; GPG
(setq epg-gpg-program "gpg2")

;; Org-mode
(use-package org
  :bind (("C-c a" . 'org-agenda))
  :config
  (use-package org-bullets :ensure t)
  (let ((org-config-file (concat user-opt-directory "org-mode-config.el")))
    (if (file-exists-p org-config-file)
        (load org-config-file)))
  (add-hook 'org-mode-hook
            (lambda ()
              (visual-line-mode 1)
              (org-bullets-mode 1))))

;; Org and reveal
(use-package ox-reveal
  :ensure t
  :config
  (use-package htmlize :ensure t)
  (setq org-reveal-root (concat "file://" (expand-file-name "~/opt/revealjs"))))

(use-package org-chef
  :ensure t)

(use-package org-noter
  :ensure t)

;; PDF tools
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width))

;; Macaulay 2 start
(load "emacs-Macaulay2.el" t)
;; Macaulay 2 end

;; Singular stuff
(add-to-list 'load-path "/usr/share/Singular/emacs")
(autoload 'singular "singular"
  "Start Singular using default values." t)
(autoload 'singular-other "singular"
  "Ask for arguments and start Singular." t)
(setq auto-mode-alist (cons '("\\.sing\\'" . c++-mode) auto-mode-alist))

;; Load local settings if they exist.
(when (file-exists-p local-config-directory)
  (mapc 'load (directory-files local-config-directory 't "^[^#].*el$")))

;; Made new custom file (for the output of custom-set-variables, etc).
(setq custom-file (concat local-config-directory "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; Recompile all previously byte-compiled files in the directory.
(byte-recompile-directory user-emacs-directory)

;; Local Variables:
;; byte-compile-warnings: (not free-vars callargs cl-functions)
;; End:
