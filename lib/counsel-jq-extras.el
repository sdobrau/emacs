;;; mine
;;;; jq-yq wrappers

(defun counsel-jq-jq()
  (interactive)
  (setq counsel-jq-command "jq")
  (counsel-jq))

(defun counsel-jq-yq()
(interactive)
(setq counsel-jq-command "yq")
(counsel-jq))



(provide 'counsel-jq-extras)
