;; TODO: setup maybe for laptop
;; https://github.com/mnp/dispwatch
(leaf dispwatch
  :ensure t
  :global-minor-mode dispwatch-mode)

;; COMMIT: disable hook for the moment
;;:hook (dispwatch-display-change-hooks . my-display-changed-hook))
;; example
(defun my-display-changed-hook (disp)
  (message "rejiggering for %s" disp)
  (cond ((equal disp '(3840 . 1080))   ; laptop + ext monitor
         (setq font-size-pt 10))
        ((equal disp '(1920 . 1080))    ; just laptop
         (setq font-size-pt 11))
        (t (message "unknown display size %sx%s" (car disp) (cdr disp)))))
