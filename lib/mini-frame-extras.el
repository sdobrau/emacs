;;; mine

(defun toggle-mouse-focus ()
  (interactive)
  (if (eq focus-follows-mouse t)
      (setq-default focus-follows-mouse nil
                    mouse-autoselect-window nil)
    (setq-default focus-follows-mouse t
                  mouse-autoselect-window t)))



(provide 'mini-frame-extras)
