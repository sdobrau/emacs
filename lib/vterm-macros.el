(defun my-run-last-term-command-in-other-vterm-window ()
  (interactive)
  (setq last-command-event 15)
  (xc/switch-to-last-window)
  (setq last-command-event 16)
  (vterm-send-C-p)
  (setq last-command-event 13)
  (vterm-send-C-j)
  (xc/switch-to-last-window))

(defun my-run-last-source-command-in-other-vterm-window ()
  (interactive)
  (xc/switch-to-last-window)
  (vterm-send-C-r)
  (vterm-insert "source")
  ;; why twice ?
  (vterm-send-C-j)
  (vterm-send-C-j)
  (xc/switch-to-last-window))

;; TODO: my-run-current-defun-last-invocation-in-other-vterm-window()
(defun my-run-last-function-invocation-in-other-vterm-window ()
  (interactive)
  (xc/switch-to-last-window)
  (vterm-send-C-r)
  (vterm-insert "source")
  ;; why twice ?
  (vterm-send-C-j)
  (vterm-send-C-j)
  (xc/switch-to-last-window))

(defun my-vterm-mount-last-cmd-hook ()
  (interactive)
  (with-current-buffer (current-buffer)
    (add-hook 'after-save-hook #'my-run-last-term-command-in-other-vterm-window nil t)))

(defun my-vterm-mount-source-cmd-hook ()
  (interactive)
  (with-current-buffer (current-buffer)
    (add-hook 'after-save-hook #'my-run-last-source-command-in-other-vterm-window nil t)))

(defun my-vterm-unmount-source-cmd-hook ()
  (interactive)
  (with-current-buffer (current-buffer)
    (remove-hook 'after-save-hook #'my-run-last-source-command-in-other-vterm-window t)))

(defun my-vterm-unmount-last-cmd-hook ()
  (interactive)
  (with-current-buffer (current-buffer)
    (remove-hook 'after-save-hook #'my-run-last-term-command-in-other-vterm-window t)))



(provide 'vterm-macros)
