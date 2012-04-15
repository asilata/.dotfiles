;;==============================================================================
;; Emacs initialization file.
;;==============================================================================

(defvar config-directory "~/.emacs.d/")

;;; Package installation and management with MELPA
;; (require 'package)
;; (require 'melpa)
;; (add-to-list 'package-archives
;; 	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (package-initialize)

;; (setq url-http-attempt-keepalives nil)
;;; MORE TO BE ADDED TO PACKAGE MANAGEMENT

;;; Buffer customizations:
(setq inhibit-startup-screen t)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(blink-cursor-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(if (fboundp 'fringe-mode) (fringe-mode 4))
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name)))
                                        "%b")))

;;; Colour themes


;;; Editing goodies
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
(global-hl-line-mode 1)
;;(volatile-highlights-mode 1) ;; THIS MODE REQUIRES INSTALLATION

(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

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

(icomplete-mode 1)
(set-default 'imenu-auto-rescan t)

;;; Global keybindings
(global-set-key [f1]          'revert-buffer)
(global-set-key [f2]          'goto-line)
(global-set-key [f5]          'query-replace)
(global-set-key [f12]         'kill-this-buffer)
(global-set-key [home]        'beginning-of-line)
(global-set-key [end]         'end-of-line)
(global-set-key [C-home]      'beginning-of-buffer)
(global-set-key [C-end]       'end-of-buffer)

;;; Backing up files
