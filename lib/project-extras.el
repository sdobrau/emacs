;;; daanturo

(defun daanturo-recent-project-roots ()
  (--> (-keep #'daanturo-project-root (daanturo-recent-directory-list))
       (append (project-known-project-roots) it)
       (-map #'abbreviate-file-name it)
       daanturo-uniqify-directory-list))



(provide 'project-extras)
