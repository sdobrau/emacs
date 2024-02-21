(require 'daanturo-core-functions)

;;;###autoload
(defun daanturo-dired-get-explitly-marked-files (&optional marker short-names)
  "Return explicitly marked files in current buffer."
  (save-excursion
    (goto-char (point-min))
    (dlet ((dired-marker-char (or marker dired-marker-char)))
      (--> (dired-get-marked-files)
           (if short-names
               (-map #'abbreviate-file-name it)
             it)))))

;;;###autoload
(defun daanturo-dired-do-rename (&optional arg)
  "Rename/move the file at point with itself as the initial input.
With non-nil ARG or marked files, call `dired-do-rename' instead."
  (interactive "P")
  (if (or arg
          (daanturo-dired-get-explitly-marked-files))
      (call-interactively #'dired-do-rename)
    (let* ((old-file (string-remove-suffix "/" (dired-file-name-at-point)))
           (new-file
            (read-directory-name "Rename or move to: "
                                 nil nil nil
                                 (file-name-nondirectory old-file))))
      (dired-rename-file old-file new-file nil))))

(defun daanturo-dired-delete-no-trash ()
  "Like `dired-do-delete' but don't move them to trash."
  (interactive)
  (cond
   ((and (member system-type '(gnu/linux))
         ;; (not (file-remote-p default-directory))
         )
    (let* ((files (-map #'file-relative-name
                        (or
                         (daanturo-dired-get-explitly-marked-files dired-del-marker)
                         (dired-get-marked-files))))
           (command (format
                     "rm -rf %s"
                     (apply #'format
                            (string-join (-repeat (length files) "%S") " ")
                            files)))
           (buf0 (current-buffer))
           (prompt (format "Delete %s files? " (length files))))
      (daanturo-shell-command
       (read-shell-command prompt command)
       :buffer t
       :callback (lambda (_ _ cmd-buf)
                   (daanturo-message (daanturo-buffer-string cmd-buf nil 'trim-last-newline))
                   (kill-buffer cmd-buf)
                   (when (equal buf0 (current-buffer))
                     (revert-buffer))))))
   (t
    (dlet ((delete-by-moving-to-trash nil)
           (dired-recursive-deletes 'always))
      (dired-do-delete)))))



(provide 'daanturo-dired)
