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
        ([?\s-s] . flip-window)))

(push '?\s-n exwm-input-prefix-keys)
(push '?\s-p exwm-input-prefix-keys)
;; Local Variables:
;; byte-compile-warnings: (not free-vars callargs)
;; End:
