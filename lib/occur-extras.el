;;; Occur - Kin Cho <kin@dynarc.com> (deftsp)

(defun kin-occur-flush-lines-containing-str (str)
  (interactive "sflush: ")
  (let ((buffer-read-only))
    (save-excursion
      (beginning-of-buffer)
      (flush-lines str))))

;; TODO: fix
(defun kin-occur-keep-lines-containing-str (str)
  (interactive "skeep: ")
  (let ((buffer-read-only))
    (save-excursion
      (beginning-of-buffer)
      (keep-lines str))))

;;; daanturo

;;;###autoload
(cl-defun daanturo-list|hide-not-matching|unmatched-lines|reversed-occur
    (regexp &optional (src-buf (current-buffer)) region-beg region-end)
  "List lines in SRC-BUF which doesn't match REGEXP."
  (interactive (list (read-regexp
                      (format "%s" 'daanturo-list|hide-not-matching|unmatched-lines|reversed-occur))
                     (current-buffer)
                     (if (use-region-p) (region-beginning) (point-min))
                     (if (use-region-p) (region-end) (point-max))))
  (daanturo-with-deferred-gc
   (let* ((buffer-name-to-insert
           (buffer-name src-buf))
          (output-buffer
           (generate-new-buffer
            (format "*%s %s %s*"
                    #'daanturo-list|hide-not-matching|unmatched-lines|reversed-occur
                    regexp
                    (replace-regexp-in-string
                     (regexp-quote (symbol-name #'daanturo-list|hide-not-matching|unmatched-lines|reversed-occur))
                     "" buffer-name-to-insert)))))
     (with-current-buffer output-buffer
       (apply #'insert-buffer-substring
              buffer-name-to-insert
              (list region-beg region-end))
       ;; (cl-letf (((symbol-function #'occur-after-change-function) #'ignore)) (flush-lines regexp (point-min) (point-max)))
       (dlet ((inhibit-read-only t))
         (remove-text-properties (point-min) (point-max) '(read-only t)))
       (flush-lines regexp (point-min) (point-max)))
     (when (called-interactively-p 'interactive)
       (pop-to-buffer output-buffer))
     output-buffer)))

(provide 'occur-extras)
