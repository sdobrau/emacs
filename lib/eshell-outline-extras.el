;;; mine

(defun my-eshell-outline-previous-recenter()
  (interactive)
  (outline-previous-heading)
  (recenter 1))

(defun my-eshell-outline-next-recenter()
  (interactive)
  (outline-next-heading)
  (recenter 1))



(provide 'eshell-outline-extras)
