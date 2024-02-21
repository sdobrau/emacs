;;; from xc

(defun xc/copy-symbol-at-point ()
  "Place symbol at point in `kill-ring'."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
	       (beg (car bounds))
	       (end (cdr bounds))
	       (sym (thing-at-point 'symbol)))
    (kill-ring-save beg end)
    (message "\"%s\"" sym)))



(provide 'prog-extras)
