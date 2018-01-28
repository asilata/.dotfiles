(setq mu4e-maildir "/home/asilata/Mail/ANU"
      mu4e-sent-folder "/Sent Items"
      mu4e-drafts-folder "/Drafts"
      mu4e-trash-folder "/Trash"
      mu4e-refile-folder "/Archive")
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.office365.com")
(setq mu4e-get-mail-command "offlineimap")
(setq message-kill-buffer-on-exit t)
(setq mu4e-use-fancy-chars t)
(setq mu4e-attachment-dir "/tmp")
(setq mu4e-view-show-images t)
