;;; https://gist.github.com/alphapapa/79ea7c33d03c9c975634559b1a776418

(defun outline-hide-all ()
  "Hide everything except the first KEEP-LEVEL headers."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Skip the prelude, if any.
    (unless (outline-on-heading-p t) (outline-next-heading))
    (hide-subtree)
    (condition-case err
        (while (outline-next-heading)
          (hide-subtree))
      (error nil))))



(provide 'outline-extras)
