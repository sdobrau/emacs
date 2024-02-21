;;; mine

(defun debug-on-variable-change-at-point ()
  (interactive)
  (debug-on-variable-change (thing-at-point 'symbol)))

(defun debug-on-entry-defun-at-point ()
  (interactive)
  (debug-on-variable-change (thing-at-point 'symbol)))



(provide 'debugging-extras)
