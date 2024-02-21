(setq gnus-plugged nil)
(defun kf-gnus-replug-hook () (setq gnus-plugged t))
(defun kf-gnus-unplug-hook () (setq gnus-plugged nil))
(add-hook 'gnus-group-prepare-hook 'kf-gnus-replug-hook)
(add-hook 'gnus-after-exiting-gnus-hook 'kf-gnus-unplug-hook)

;; Standards have changed.
(setq-default gnus-large-newsgroup nil)

;; Do to Gnus what I used do to Python (but don't do to Python any
;; more Because Standards).
(setq gnus-thread-indent-level 2)
;; Why in the heck is this not the default?
(setq gnus-gcc-mark-as-read t)



(provide 'kf-gnus)
