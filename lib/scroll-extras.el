;;; from kf

(defun kf-next-line (&optional nlines)
  "Move down NLINES (default: 1), preserving column, and recenter window."
  (interactive "p")
  (or nlines (setq nlines 1))
  (kf-down-and-recenter nlines))

(defun kf-previous-line (&optional nlines)
  "Scroll the buffer to move line position."
  (interactive "p")
  (or nlines (setq nlines 1))
  (kf-down-and-recenter (- 0 nlines)))

(defun kf-down-and-recenter (nlines)
  "Move viewport up, and point down, by NLINES, preserving column.
  Negative NLINES means what you think it means."
  (let ((col
         (if (or (eq last-command 'kf-next-line)
                 (eq last-command 'kf-previous-line))
             kf-current-column
           (setq kf-current-column (current-column)))))
    (scroll-up nlines)
    (forward-line nlines)
    (move-to-column col)))



(provide 'scroll-extras)
