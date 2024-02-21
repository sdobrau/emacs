;;; tramp-refresh
;; https://emacs.stackexchange.com/questions/29008/tramp-keeps-opening-the-connection

(defun tramp-refresh ()
  (interactive)
  (recentf-cleanup)
  (tramp-cleanup-all-buffers)
  (tramp-cleanup-all-connections))

;;; 10sr: tramp-find-file

(defun my-tramp-remote-find-file (f)
  "Open F."
  (interactive (list (read-file-name "tramp scp: "
                                     "/scp:"
                                     nil ;; "/scp:"
                                     (confirm-nonexistent-file-or-buffer))))
  (find-file f))

;;; jao: ssh-tramp completing read TODO: eshell func

(defun jao-tramp-hosts ()
  (seq-uniq
   (mapcan (lambda (x)
             (remove nil (mapcar 'cadr (apply (car x) (cdr x)))))
           (tramp-get-completion-function "ssh"))
   #'string=))

;; inspired from jao, using jao-tramp-hosts but for eshell
(defun my-ssh ()
  (interactive)
  (let ((h (completing-read "Host: " (jao-tramp-hosts)))
        (u (read-string "User: ")))
    (my-exec-in-new-eshell-buffer
     (format "ssh %s@%s" u h)
     (format "*eshell ssh: %s@%s" u h))))

;; TODO: exec cmd in new eshell buffer
(defun my-exec-in-new-eshell-buffer (cmd &optional name)
  (if name (setq eshell-buffer-name name))
  (with-current-buffer (eshell t)
    (insert cmd)
    (eshell-send-input))
  (setq eshell-buffer-name "*eshell*"))


(provide 'tramp-extras)
