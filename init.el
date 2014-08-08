
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
(package-initialize)

(setq url-http-attempt-keepalives nil)

;;; Install the required packages
(defvar required-packages-list
  '(auctex magit markdown-mode paredit rainbow-mode volatile-highlights zenburn-theme
           haskell-mode autopair org)
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

(if (fboundp 'fringe-mode) (fringe-mode 4))
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name)))
                                        "%b")))

(require 'uniquify) ;; For better naming of buffers with the same name.
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;;; Colour themes
(load-theme 'zenburn t)

;;; Editing goodies
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
(global-hl-line-mode 1)

(require 'autopair)
(autopair-global-mode)
(electric-indent-mode 1)
(electric-layout-mode 1)

(require 'volatile-highlights)
(volatile-highlights-mode 1)

(setq-default indent-tabs-mode nil)     ;Don't use tabs to indent...
(setq-default tab-width 8)         ;...but maintain correct appearance

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." )

(ido-mode 1)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-default-file-method 'selected-window)

(icomplete-mode 1) ;Show completions in minibuffer
(set-default 'imenu-auto-rescan t)

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
(require 'midnight)

;;; Programming goodies
;; (require 'yasnippet)
;; (yas-global-mode 1)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;(require 'php+-mode)
;(php+-mode-setup)

;; Hyde mode for writing jekyll stuff.
;;(require 'hyde)

;;; Mode-specific hooks
(load "emacs-Macaulay2.el" t)

(require 'reftex)
(require 'auto-complete)
(require 'auto-complete-auctex)
(ac-flyspell-workaround)

(add-hook 'LaTeX-mode-hook
          (lambda ()
	    (TeX-PDF-mode 1)
            (flyspell-mode 1)
            (auto-fill-mode 0)
            (setq TeX-view-program-list '(("Okular" "okular %o")))
            (setq TeX-view-program-selection '((output-pdf "Okular")))
            (reftex-mode 1)
            ))
;(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)

(load "emacs-Macaulay2.el" t)

(add-hook 'haskell-mode-hook
          'turn-on-haskell-indentation)

(add-hook 'scheme-mode-hook
          (lambda () (paredit-mode 1)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (paredit-mode 1)
            (turn-on-eldoc-mode)
            (rainbow-mode 1)))

(add-hook 'markdown-mode-hook
          'turn-on-orgtbl)

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
