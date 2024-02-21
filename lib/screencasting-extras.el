;;; mine

(defun scrot (&optional arg)
  (interactive "P")
  ;; TODO: generalized function to complete dir, make if nonexistent,
  ;; and simple complete intf
  ;; TODO: store last dir and use that as default arg for directory
  ;; if not using prefix
  ;; TODO: apply/prog/ ???
  (exwm-shell-command
   (concat ("scrot "
           (if (eq (car current-prefix-arg) 4) "" "-s ")
           (if (eq (car current-prefix-arg) 16)
               (read-directory-name "Dir: " my-screenshots-directory)
             my-screenshots-directory)
           (read-string "name: ")
           ".png")))



(provide 'screencasting-extras)
