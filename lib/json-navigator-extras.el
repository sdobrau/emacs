;; spacemacs

(defun spacemacs/json-navigator-dwim (arg)
  "Display the JSON hierarchy of the whole buffer or the active region.
If ARG is a universal prefix argument then display the hierarchy after point."
  (interactive "P")
  (if arg
      (json-navigator-navigate-after-point)
    (if (not (use-region-p))
        (save-excursion (json-navigator-navigate-region (point-min) (point-max)))
      (json-navigator-navigate-region (region-beginning) (region-end)))))



(provide 'json-navigator-extras)
