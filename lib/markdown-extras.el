;;; from kf

(defun kf-browse-markdown (&optional not-github-flavored)
  "Browse the current Markdown buffer as HTML, in the default browser.
Treat the input as GitHub-flavored Markdown unless optional argument
NOT-GITHUB-FLAVORED is non-nil.  Requires 'markdown' and 'pandoc'."
  (interactive "P")
  (let ((orig-buf (current-buffer))
        (output-buf (get-buffer-create "*HTML from Markdown*")))
    (set-buffer output-buf)
    (delete-region (point-min) (point-max))
    (set-buffer orig-buf)
    (let ((cmd (if not-github-flavored
                   "markdown"
                 "pandoc -f gfm -o -")))
      (shell-command-on-region (point-min) (point-max) cmd output-buf))
    (browse-url-of-buffer output-buf)))



(provide 'markdown-extras)
