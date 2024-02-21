(defun my-shelldon-send-defun ()
  (interactive)
  (mark-defun)
  (shelldon-send-region))

(defun my-shelldon-setx-region-or-line-run-then-remove-set-x ()
  (interactive))

(provide 'shelldon-extras)
