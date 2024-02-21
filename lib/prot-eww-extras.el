;; mine

(defun my-prot-eww-browse-dwim-from-yank-or-clipboard (&optional arg)
  (interactive "P")
  (prot-eww-browse-dwim (simpleclip-get-contents)))



(provide 'prot-eww-extras)
