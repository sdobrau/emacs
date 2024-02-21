;;; npostavs

(defun yas-try-expanding-auto-snippets ()
  "Expand snippets with the `auto' condition.
This is intended to be added to `post-command-hook'."
  (when (bound-and-true-p yas-minor-mode)
    (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
      (yas-expand))))

(add-hook 'post-command-hook #'yas-try-expanding-auto-snippets)

;;; from koek

(defun koek-ws/disable-final-empty-line ()
  "Disable final empty line for current."
  (setq-local require-final-newline nil))

(add-hook 'snippet-mode-hook #'koek-ws/disable-final-empty-line)

(add-hook 'snippet-mode-hook #'my-yaml-mode-hook)

(provide 'yasnippet-extras)
