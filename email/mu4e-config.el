;;; Getting mail
(setq mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval (* 60 15))
(setq mu4e-change-filenames-when-moving t)

;;; Sending mail
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587
      message-kill-buffer-on-exit t)

;;; Indexing
(setq mu4e-index-cleanup nil
      mu4e-index-lazy-check t)

;;; General view settings
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

;;; Message view settings
(setq mu4e-view-use-gnus nil)
(setq mu4e-view-show-images t)
(setq mu4e-html2text-command 'mu4e-shr2text)
(setq mu4e-attachment-dir "/tmp")
(add-hook 'mu4e-view-mode-hook
          (lambda ()
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))

;;; Message editing settings


;;; Bookmarks
(setq mu4e-bookmarks
      `(,(make-mu4e-bookmark
          :name  "Reasonable recent messages"
          :query "date:12m..now AND to:asilata AND (maildir:/ANU/INBOX OR maildir:/Gmail/INBOX OR tag:\\\\Important) AND NOT flag:list"
          :key ?r)
        ,(make-mu4e-bookmark
          :name "Mailing lists"
          :query "flag:list AND (maildir:/ANU/INBOX OR maildir:/Gmail/INBOX)"
          :key ?l)
        ,(make-mu4e-bookmark
          :name "ANU Inbox"
          :query "maildir:\"/ANU/INBOX\""
          :key ?a)
        ,(make-mu4e-bookmark
          :name "Gmail Inbox"
          :query "maildir:\"/Gmail/INBOX\""
          :key ?g)
        ,(make-mu4e-bookmark
          :name "Today and untrashed"
          :query "date:today..now AND NOT (flag:trashed  OR maildir:\"ANU/Deleted Items\" OR maildir:\"/Gmail/[Gmail]/Bin\")"
          :key ?t)
        ,(make-mu4e-bookmark
          :name "Last week and untrashed"
          :query "date:7d..now AND NOT (flag:trashed  OR maildir:\"ANU/Deleted Items\" OR maildir:\"/Gmail/[Gmail]/Bin\")"
          :key ?w)
        ))


;;; Maildirs shortcuts (NOT WORKING)
(setq mu4e-maildirs-shortcuts
      '( (:maildir "/ANU/INBOX" :key ?a)
         (:maildir "/Gmail/INBOX" :key ?g)))

;;; Custom functions
;; (defun my/delete-without-trashing ()
;;   (let (tfolder (mu4e-get-trash-folder (mu4e-message-at-point)))
;;     (mu4e-mark-set refile tfolder)))
;; (define-key mu4e-headers-mode-map (kbd "C-!") 'my/delete-without-trashing)

;;; Various hooks
(add-hook 'mu4e-headers-found-hook
          (lambda () (setq truncate-lines t)))

(add-to-list 'mu4e-view-actions
             '("retag message" . mu4e-action-retag-message) t)
(add-to-list 'mu4e-headers-actions
             '("retag message" . mu4e-action-retag-message) t)

(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (auto-fill-mode 0)
            (visual-line-mode 1)))

(add-hook 'mu4e-mark-execute-pre-hook
  (lambda (mark msg)
    (cond ((member mark '(refile trash)) (mu4e-action-retag-message msg "-\\\\Inbox"))
          ((equal mark 'flag) (mu4e-action-retag-message msg "\\\\Starred"))
          ((equal mark 'unflag) (mu4e-action-retag-message msg "-\\\\Starred")))))

;;; Emacs default
(setq mail-user-agent 'mu4e-user-agent)

;;; Attachments from dired

;;; Confirm on sending
(setq message-confirm-send t)
;;; Maildirs extra
;; (use-package mu4e-maildirs-extension
;;   :straight t
;;   :config
;;   (mu4e-maildirs-extension)
;;   (setq mu4e-maildirs-extension-use-bookmarks nil))

;;; Org mode integration
(require 'org-mu4e)
(setq org-mu4e-link-query-in-headers-mode nil)

;; Desktop notifications
(use-package mu4e-alert
  :straight t
  :config
  (setq mu4e-alert-interesting-mail-query
        (concat
         "flag:unread"
         " AND NOT maildir:\"/Gmail/[Gmail]/Bin\""
         " AND NOT maildir:\"/ANU/Deleted Items\""))
  (mu4e-alert-set-default-style 'libnotify)
  (setq mu4e-alert-email-notification-types '(subjects))
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  )

;;; mu4e goodies
;; (use-package mu4e-goodies
;;   :straight (:host github :repo "panjie/mu4e-goodies")
;;   :config
;;   (require 'mu4e-goodies))

;;; mu4e conversation
(use-package mu4e-conversation
  :straight t)
;;; Private settings (user settings, contexts)
(let ((mu4e-private-settings (concat user-opt-directory "private/mu4e-private-settings.el")))
  (if (file-exists-p mu4e-private-settings)
      (load mu4e-private-settings)))

;;; Endnotes
;; Local Variables:
;; byte-compile-warnings: (not free-vars callargs cl-functions)
;; End:
