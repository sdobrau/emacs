;;; jeremyf
;; consult helper to use current region if selected

(defun jf/consult-first-param-is-initial-text (consult-fn &rest rest)
  "Advising function around CONSULT-FN.

  The CONSULT-FN's first parameter should be the initial text.

  When there's an active region, use that as the first parameter
  for CONSULT-FN.  Otherwise, use an empty string the first
  parameter.  This function handles the REST of the parameters."
  (interactive)
  (apply consult-fn
         (when (use-region-p)
           (buffer-substring
            (region-beginning) (region-end)))
         rest))

(defun jf/consult-ripgrep-wrapper (consult-fn &optional dir given-initial)
  "Advising function around CONSULT-FN.

  DIR and GIVEN-INITIAL match the method signature of `consult-wrapper'."
  (interactive "P")
  (let ((initial (list (or given-initial
                           (when (use-region-p)
                             (buffer-substring (region-beginning) (region-end)))))))
    (apply consult-fn dir initial)))

(advice-add #'consult-line
            :around #'jf/consult-first-param-is-initial-text
            '((name . "wrapper")))
(advice-add #'consult-ripgrep
            :around #'jf/consult-ripgrep-wrapper
            '((name . "wrapper")))



(provide 'consult-extras)
