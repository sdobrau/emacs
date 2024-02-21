(cl-defmethod initialize-instance :after
  ((connection emacsql-sqlite-connection) &rest _rest)
  (emacsql-sqlite-ensure-binary)
  (let* ((process-connection-type nil)  ; use a pipe
         (coding-system-for-write 'utf-8-auto)
         (coding-system-for-read 'utf-8-auto)
         (file (slot-value connection 'file))
         (buffer (generate-new-buffer " *emacsql-sqlite*"))
         (fullfile (if file (expand-file-name file) ":memory:"))
         (process (start-process
                   "emacsql-sqlite" buffer emacsql-sqlite-executable fullfile)))
    (setf (slot-value connection 'process) process)
    (setf (process-sentinel process)
          (lambda (proc str)
            (let ((buf (process-buffer proc)))
              (message "--- %s" str)
              (message ">>>%s<<<" (with-current-buffer buf (buffer-string)))
              (kill-buffer buf))))
    (emacsql-wait connection)
    (emacsql connection [:pragma (= busy-timeout $s1)]
             (/ (* emacsql-global-timeout 1000) 2))
    (emacsql-register connection)))



(provide 'emacsql-sqlite-extras)
