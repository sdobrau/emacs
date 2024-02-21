;;; mine
;;;; for org-download

(defun org-download-file-format-default (filename &optional arg)
  (interactive "P")
  "It's affected by `org-download-timestamp'."
  (concat
   (nth 4 (org-heading-components))
   "-"
   (format-time-string org-download-timestamp)
   filename))



(provide 'org-download-extras)
