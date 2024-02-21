;; TODO: or existing host ssh
;; TODO: think through for duplicates
;; TODO: CORNERCASE: prompt still underway and does not include hostname that will b
;; connected to soon. So subsequent connections will still open a new terminal
(defun my-vterm-frame-command-in-buffer (&optional initial-cmd)
  (interactive)
  (nameframe-with-frame "vterm" nil)
  (vterm 4)
  (sit-for 0.1)

  (if initial-cmd
      (progn (vterm-send-string initial-cmd)
             (vterm-send-C-j))))

(defun my-vterm-ssh-frame-multi (hoststr)
  ;; (message "hoststr is %s" hoststr)
  (cond
   ((eq hoststr "") (error "No hosts"))
   (t (seq-map (lambda (host)
                 (progn ;; (message "host passed is %s" host)
                   (my-vterm-ssh-frame host)))
               (s-split "\n" hoststr)))))

(defun my-vterm-ssh-frame (&optional host arg)
  (interactive "P")
  (let* ((user (cond ((eq (prefix-numeric-value arg) 4) "ccaemrs")
                     (t "isdccaemrs")))
         (host (or host (read-string "Host: "))))
    ;; first matching, or new if nonexistent

    ;; (message "host-passed in my-vterm-ssh-frame: %s" host)
    (my-vterm-frame-command-in-buffer
     (concat "ssh " user "@" host))))

(defun my-switch-to-buffer-from-outside-focus-frame (buffer)
  (interactive)
  (x-focus-frame (nameframe-with-frame "quick"))
  (switch-to-buffer buffer))

(defun my-buffer-list-simple ()
  (string-join (--filter (string-match-p "[[:alnum:]]*" (buffer-name it)) (nasy/buffer-list))
               " "))



(provide 'vterm-anywhere)
