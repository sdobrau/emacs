;;; mine

;;;###autoload
(defun exwm-shell-command (command)
  "Executes a shell command, but doesn't create a buffer for the
output."
  (interactive (list (read-shell-command "🌞 " )))
  (start-process-shell-command command nil command))



(provide 'os-extras)
