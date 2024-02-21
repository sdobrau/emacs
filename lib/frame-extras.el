;;; mine

(defun kill-other-frames ()
  ;; pull current-frame which is usually what we want
  (let ((cur-frame `(frame-configuration ,(cadr
                                           (current-frame-configuration)))))
    ;; set a new frame configuration with this frame, deleting all others
    ;; see readme
    ;; TODO: selected-frame?
    (set-frame-configuration cur-frame t)))

(defun list-buffers-in-current-frame ()
  "List all buffers in the current frame."
  (mapcar (lambda (x) (window-buffer x))
          (mapc (lambda (y)
                  (window-buffer y))
                (window-list))))

;; TODO: wtf??
(defun kill-buffer--around (orig-fun &rest args)
  (interactive "p")
  (apply orig-fun args)
  (delete-frame))

(defun floating-detached-shell (&optional whatshell)
  (interactive)
  (let ((whatshell (or whatshell "eshell")))
    (with-selected-frame (make-frame '((name . "shell")
                                       (width . 80)
                                       (height . 30)))
      (with-current-buffer (shell)
        (add-hook 'kill-buffer-hook #'delete-frame 0 t)))))



(provide 'frame-extras)
