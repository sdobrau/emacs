;;; from kf
(defun kf-fill-paragraph (&optional justify)
  "Like fill-paragraph, but don't mark the buffer as modified if no change.

Emacs's native fill-paragraph is like the burglar who breaks into
your house, rearranges all your furniture exactly as it was, and
departs: even if the result of the fill is to leave the buffer in
exactly the same state, it still marks the buffer as modified so you
know you've been broken into.

Note: to get this accepted into Emacs, it should watch the md5sum for
just the affected region rather than the entire buffer.  See
`fill-region' and `fill-region-as-paragraph' in textmodes/fill.el.
The elegant solution would be a new macro, '(detect-buffer-unmodified
from to)' or something, that just wraps the relevant body of code in
those two functions.  Then it could be used by other fill functions
easily too."
  (interactive "P")
  (let ((orig-md5sum (md5 (current-buffer)))
        (was-modified-before-fill (buffer-modified-p)))
    (fill-paragraph justify)
    (let ((new-md5sum (md5 (current-buffer))))
      (when (string-equal orig-md5sum new-md5sum)
        (set-buffer-modified-p was-modified-before-fill)))))



(provide 'fill-extras)
