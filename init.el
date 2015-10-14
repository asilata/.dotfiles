;;==============================================================================
;; Emacs initialization file
;; (Inspired by emacs-prelude)
;;==============================================================================

(require 'cl)

(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"
    "Root directory of the emacs configuration."))
(defvar user-opt-directory (concat user-emacs-directory "opt/")
  "User-installed emacs packages go here.")
(defvar local-config-directory (concat user-emacs-directory "local/")
  "Machine-local configuration files go here")
(add-to-list 'load-path user-opt-directory)
(let ((default-directory user-opt-directory))
  (normal-top-level-add-subdirs-to-load-path))


;;; Package management with MELPA (in addition to the GNU archive).
(require 'package)
;(require 'melpa)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)

(setq url-http-attempt-keepalives nil)

;;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  )
(font-lock-add-keywords 'emacs-lisp-mode
			'(("use-package" . font-lock-keyword-face)))
(require 'use-package)

;;; Install the required packages
(defvar required-packages-list
  '(auctex auto-complete auto-complete-auctex
           haskell-mode magit markdown-mode org paredit rainbow-mode
           smartparens switch-window textile-mode volatile-highlights
           web-mode zenburn-theme)
  "List of packages required to be installed at startup.")

(defun required-packages-installed-p ()
  (loop for pkg in required-packages-list
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (required-packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  (dolist (pkg required-packages-list)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

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

(if (fboundp 'fringe-mode) (fringe-mode 2))
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name)))
                                        "%b")))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*" ; don't muck with special buffers
        )
  )

;;; Colour themes
(load-theme 'zenburn t)

;;; Editing
(use-package smartparens
  :ensure t
  :config
  (show-paren-mode 1)
  (setq show-paren-style 'parenthesis)
  (use-package smartparens-config)
  (use-package smartparens-latex)
  (smartparens-global-mode 1)
  )


(electric-indent-mode 1)
(electric-layout-mode 1)
(global-hl-line-mode 1)

(use-package volatile-highlights
  :config (volatile-highlights-mode 1))

(setq-default indent-tabs-mode nil)     ;Don't use tabs to indent...
(setq-default tab-width 8)         ;...but maintain correct appearance

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." )

(use-package ido
  :config
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10
        ido-default-file-method 'selected-window)
  (ido-mode 1)
  )

(use-package icomplete
  :config
  (set-default 'imenu-auto-rescan t)
  (icomplete-mode 1) ;Show completions in minibuffer
  )

;;; Global keybindings
(global-set-key [f1]          'revert-buffer)
(global-set-key [f2]          'goto-line)
(global-set-key [f5]          'query-replace)
(global-set-key [f6]          'magit-status)
(global-set-key [f12]         'kill-this-buffer)
(global-set-key [home]        'beginning-of-line)
(global-set-key [end]         'end-of-line)
(global-set-key [C-home]      'beginning-of-buffer)
(global-set-key [C-end]       'end-of-buffer)
(global-set-key (kbd "C-x o") 'switch-window) ;; Visual window switching
(global-set-key (kbd "C-;")   'toggle-comment-line-or-region)

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

;;; Programming
;; (require 'yasnippet)
;; (yas-global-mode 1)

(defun toggle-comment-line-or-region (&optional arg)
  "Toggle commenting on current line or region, then go to the next line"
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
  (next-line))

(use-package markdown-mode
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\|txt\\)$" . markdown-mode))

(use-package textile-mode
  :mode ("\\.textile\\'" . textile-mode))

(use-package web-mode
  :mode ("\\.html?\\'" . web-mode)
  :config
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-pairing t))

;; Hyde mode for writing jekyll stuff.
;;(require 'hyde)

;;; Mode-specific hooks
(use-package reftex)

(use-package auto-complete
  :config
  (use-package auto-complete-auctex)
  (ac-flyspell-workaround))

(use-package auctex-latexmk
  :ensure t
  :config
  (auctex-latexmk-setup))

(add-hook 'LaTeX-mode-hook
          (lambda ()
	    (TeX-global-PDF-mode 1)
            (flyspell-mode 1)
            (auto-fill-mode 0)
            (auto-complete-mode 1)
            (setq TeX-view-program-list '(("Okular" "okular %o")))
            (setq TeX-view-program-selection '((output-pdf "Okular")))
            (reftex-mode 1)
            (visual-line-mode 1)
            ))

(add-hook 'haskell-mode-hook
          'turn-on-haskell-indentation)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (turn-on-eldoc-mode)
            (rainbow-mode 1)))

(add-hook 'markdown-mode-hook
          'turn-on-orgtbl)

(add-hook 'scss-mode-hook
          (lambda ()
            (setq scss-compile-at-save nil)
            (rainbow-mode 1)))

(add-hook 'textile-mode-hook
          'turn-on-orgtbl)

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

;; Recompile all previously byte-compiled files in the directory.
(byte-recompile-directory user-emacs-directory)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (auctex-latexmk use-package auctex zenburn-theme web-mode volatile-highlights textile-mode switch-window smartparens scss-mode rainbow-mode python php+-mode paredit org muttrc-mode markdown-mode magit haskell-mode haml-mode autopair auto-complete-auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
