;;; fix, cancel excessive timers aggressive-indent--indent-if-changed

;; https://github.com/Malabarba/aggressive-indent-mode/issues/112

(defun cancel-aggressive-indent-timers ()
  (interactive)
  (let ((count 0))
    (dolist (timer timer-idle-list)
      (when (eq 'aggressive-indent--indent-if-changed (aref timer 5))
        (incf count)
        (cancel-timer timer)))
    (when (> count 0)
      (message "Cancelled %s aggressive-indent timers" count))))

(run-with-idle-timer 60 t 'cancel-aggressive-indent-timers)

(provide 'aggressive-indent-extras)
