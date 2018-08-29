;; Getting mail
(setq mu4e-maildir "/home/asilata/Mail")
(setq mu4e-get-mail-command "offlineimap"
      mu4e-update-interval 300)

;; Sending mail
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587
      message-kill-buffer-on-exit t)

;; Global user settings
(setq user-full-name  "Asilata Bapat"
      mu4e-compose-signature "\nAsilata"
      mu4e-user-mail-address-list '("asilata.bapat@anu.edu.au" "asilata@gmail.com" "asilata@uga.edu" "asilata@math.uchicago.edu" "asilata@mit.edu"))

;; Context switching
(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "ANU"
          :match-func
          (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches
               msg '(:to :cc :from) "asilata.bapat@anu.edu.au")))
          :vars
          '((mu4e-sent-folder . "/ANU/Sent Items")
            (mu4e-drafts-folder . "/ANU/Drafts")
            (mu4e-trash-folder . "/ANU/Trash")
            (mu4e-refile-folder . "/ANU/Archive")
            (smtpmail-smtp-server . "smtp.office365.com")	       
            ;(mu4e-sent-messages-behavior 'sent)
            (user-mail-address . "asilata.bapat@anu.edu.au")
            (mu4e-reply-to-address . "asilata.bapat@anu.edu.au")))
        
        ,(make-mu4e-context
          :name "Gmail"
          :match-func
          (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches
               msg '(:to :cc :from) "asilata@gmail.com")))
          :vars
          '((mu4e-sent-folder . "/Gmail/[Gmail]/All Mail")
            (mu4e-drafts-folder . "/Gmail/[Gmail]/Drafts")
            (mu4e-trash-folder . "/Gmail/[Gmail]/Bin")
            ;(mu4e-refile-folder . "/Gmail/[Gmail].Archive")
            (smtpmail-smtp-server . "smtp.office365.com")	       
            (mu4e-sent-messages-behavior . 'delete)
            (user-mail-address . "asilata@gmail.com")
            (mu4e-reply-to-address . "asilata@gmail.com")))
        )
      )

;; Indexing
(setq mu4e-index-cleanup nil
      ;mu4e-index-lazy-check t
      )

;; mu4e display settings
(setq mu4e-headers-date-format "  %_d %b"
      mu4e-headers-time-format "%_l:%M %P"
      mu4e-headers-fields
      '((:human-date . 10)
        (:flags . 5)
        (:from-or-to . 20)
        (:thread-subject . nil))
      mu4e-headers-skip-duplicates t)
(setq message-kill-buffer-on-exit t)
(setq mu4e-use-fancy-chars t)
(setq mu4e-attachment-dir "/tmp")
(setq mu4e-view-show-images t)

;; Bookmarks
(setq mu4e-bookmarks
      `(,(make-mu4e-bookmark
          :name  "Reasonable but unread messages"
          :query "flag:unread AND (maildir: 'ANU/INBOX' OR tag:\\\\Important)"
          :key ?u)
        ,(make-mu4e-bookmark
          :name "Today's messages"
          :query "date:today..now"
          :key ?t)
        ,(make-mu4e-bookmark
          :name "Last 7 days"
          :query "date:7d..now"
          :key ?w)
        ,(make-mu4e-bookmark
          :name "Procrastinated messages"
          :query "tag:todo"
          :key ?p)))

;; Various hooks
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

;;Maildirs extra
(use-package mu4e-maildirs-extension
  :ensure t
  :config
  (mu4e-maildirs-extension))

;; Org mode integration
(require 'org-mu4e)
(setq org-mu4e-link-query-in-headers-mode nil)

;; Desktop notifications
(use-package mu4e-alert
  :ensure t
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (setq mu4e-alert-email-notification-types '(subjects))
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  )

;; Colours
