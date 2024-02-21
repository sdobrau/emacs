
;;; mine

;;;; directory helpers

(defun directory-files-recursively-without-first (path) ;; quelpa
  "Return list of directory files from PATH recursively without first two entries."
  (let ((result '()))
    (mapc
     (lambda (file)
       (if (file-directory-p file)
           (progn
             ;; When directory is not empty.
             (when (cddr (directory-files file))
               (dolist (subfile (quelpa-directory-files file))
                 (add-to-list 'result subfile))))
         (add-to-list 'result file)))
     (mapcar
      (lambda (file) (expand-file-name file path))
      ;; Without first two entries because they are always "." and "..".
      (remove ".." (remove "." (directory-files path)))))
    result))

;; TODO: currying? if slash then add a slash to argument of whole fuction
(defun first-topmost-directory (&optional dir slash)
  "Return first topmost directory of DIR, optionally current directory.
If SLASH is non-nil, append a leading slash to the directory."
  (interactive "P")
  (-last-item (s-split-words (or dir
                                 default-directory))))

(defun directory-files-no-dots-absolute (dir)
  (directory-files-recursively dir directory-files-no-dot-files-regexp nil))

;;; daanturo

;;;###autoload
(defun daanturo-directory-children (dir &optional full match nosort count)
  "`directory-files' on DIR.
Somehow, manually process by elisp is still faster than
regexp (`directory-files-no-dot-files-regexp')."
  (named-let recur ((accu '())
                    (files (directory-files dir full match nosort count))
                    (exclude '("." "..")))
    (let ((f (car files)))
      (cond ((not (and files exclude))
             `(,@(nreverse accu) . ,files))
            ((member f exclude)
             (recur accu (cdr files) (delete f exclude)))
            (t
             (recur (cons f accu) (cdr files) exclude))))))

(defun daanturo-uniqify-directory-list (directory-list)
  (--> (-map (lambda (f) (daanturo-ensure-string-suffix "/" f))
             directory-list)
       delete-dups))

;;;###autoload
(defun daanturo-recent-directory-list ()
  (recentf-mode)
  (--> (dlet ((non-essential t))
         (-map (lambda (f)
                 (if (or (string-suffix-p "/" f)
                         (file-directory-p f))
                     f
                   (file-name-parent-directory f)))
               recentf-list))
       (-map #'abbreviate-file-name it)
       daanturo-uniqify-directory-list))

;;;###autoload
(defun my-directory-has-files-p (directory regexp)
  (--> (directory-files directory nil regexp nil 3)
       (cl-delete '("." "..") it :test #'-contains?)
       (< 0 (length it))))



(provide 'directory-extras)
