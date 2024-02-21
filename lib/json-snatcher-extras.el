;;; mine

(defun jsons-print-path-jq ()
  (interactive)
  "Print the jq path to the JSON value under point, and save it in the kill ring."
  (let* ((path (jsons-get-path))
         (i 0)
         (jq_str ".")
         key)
    (setq path (reverse path))
    (while (< i (length path))
      (if (numberp (elt path i))
          (progn
            (setq jq_str (concat jq_str "[" (number-to-string (elt path i)) "]"))
            (setq i (+ i 1)))
        (progn
          (setq key (elt path i))
          (setq jq_str (concat jq_str (substring key 1 (- (length key) 1))))
          (setq i (+ i 1))))
      (when (elt path i)
        (unless (numberp (elt path i))
          (setq jq_str (concat jq_str ".")))))
    (progn (kill-new jq_str)
           (princ jq_str))))

(defun jsons-print-path-python ()
  (interactive)
  "Print the python path to the JSON value under point, and save it in the kill ring."
  (let ((path (jsons-get-path))
        (i 0)
        (python_str ""))
    (setq path (reverse path))
    (while (< i (length path))
      (if (numberp (elt path i))
          (progn
            (setq python_str (concat python_str "[" (number-to-string (elt path i)) "]"))
            (setq i (+ i 1)))
        (progn
          (setq python_str (concat python_str "[" (elt path i) "]"))
          (setq i (+ i 1)))))
    (progn (kill-new python_str)
           (princ python_str))))



(provide 'json-snatcher-extras)
