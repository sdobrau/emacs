;;; from kf

(defun kf-insert-variable (var)
  "Insert the value of lisp variable VAR into the current buffer.
 VAR should be a string or a number; if it is a number, it will be
 converted into a string and then inserted."
  (interactive "*vInsert variable: ")
  (cond
   ((stringp (symbol-value var)) (insert (symbol-value var)))
   ((numberp (symbol-value var)) (insert (int-to-string (symbol-value var))))
   (t
    (error (concat (symbol-name var) " not a string or integer!")))))



(provide 'lisp-extras)
