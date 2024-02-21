;; https://www.reddit.com/r/emacs/comments/10qo7vb/comment/j6rmvvf
;; COMMIT: add open-on-github
;; TODO: fixqq
;;;###autoload
(defun open-on-github ()
  (interactive)
  (let
      ((repo-url (magit-git-string-ng "git remote get-url --push origin"))
       (commit-hash (magit-git-string-ng "git rev-parse HEAD"))
       (start-line (if (use-region-p)
                       (line-number-at-pos (region-beginning))
                     (line-number-at-pos)))
       (end-line (if (use-region-p) (line-number-at-pos (region-end)))))
    (unless repo-url (error  "not in a git repo"))
    (browse-url
     (concat
      (substring repo-url 0 -4)
      "/blob/"
      commit-hash
      "/"
      (substring buffer-file-name (length (projectile-project-root)))
      "#L" (number-to-string start-line)
      (if (and (use-region-p) (< 0 (- end-line start-line)))
          (concat "..L" (number-to-string end-line)))
      ))))



(provide 'git-extras)
