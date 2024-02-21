;; mine
;; partly inspired from tibor

(defun my-popup-scratch-buffer-v ()
  "Create a vertical split with a new Org-mode buffer"
  (interactive)
  (split-window-below-and-focus)
  (switch-to-buffer (scratch--create (scratch--buffer-querymode) "*scratch*")))

(defun my-popup-scratch-buffer-h ()
  "Create a vertical split with a new Org-mode buffer"
  (interactive)
  (split-window-right-and-focus)
  (switch-to-buffer (scratch--create (scratch--buffer-querymode) "*scratch*")))



(provide 'scratch-extras)
