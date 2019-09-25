(setq helloworld "hihi")
(setq exwm-input-global-keys
      `(
        ;; 's-q': Reset (to line-mode).
        ([?\s-q] . exwm-reset)
        ;; 's-w': Switch workspace.
        ([?\s-w] . exwm-workspace-switch)
        ;; 's-r': Launch application.
        ([?\s-r] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))))
      
;; Local Variables:
;; byte-compile-warnings: (not free-vars callargs)
;; End:
