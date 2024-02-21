;;; mine

;; yyr remove hook

(defun clean-hook (hook)
  "Set nil to HOOK."
  (interactive
   (let ((v (variable-at-point))
         (enable-recursive-minibuffers t)
         val)
     (setq val (completing-read
                (if (symbolp v)
                    (format
                     "Clean hook (default %s): " v)
                  "Clean hook: ")
                obarray (lambda (vv)
                          (and (boundp vv)
                               (not (keywordp vv))
                               (string-match ".*-hook" (symbol-name vv))))
                t nil nil))
     (list val
           (prefix-numeric-value current-prefix-arg))))
  (funcall 'set (intern hook) nil)
  (message "%s set to nil" hook))



(provide 'hook-extras)
