;;; mine

(defun my-turbo-log-symbol-at-point ()
  (interactive)
  (easy-kill 1)
  (easy-kill-thing 'symbol 1)
  (easy-kill-mark-region)
  (turbo-log-print))



(provide 'turbo-log-extras)
