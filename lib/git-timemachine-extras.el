;;; redguard

(defun redguardtoo-git-timemachine-show-selected-revision ()
  "Show last (current) revision of file."
  (interactive)
  (let* ((collection (mapcar (lambda (rev)
                               ;; re-shape list for the ivy-read
                               (cons (concat (substring-no-properties (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                             (git-timemachine--revisions))))
    ;; TODO: replace with native completing-read
    (ivy-read "commits:"
              collection
              :action (lambda (rev)
                        ;; compatible with ivy 8+ and later ivy version
                        (unless (string-match "^[a-z0-9]*$" (car rev))
                          (setq rev (cdr rev)))
                        (git-timemachine-show-revision rev)))))

(defun redguardtoo-git-timemachine ()
  "Open git snapshot with the selected version."
  (interactive)
  (git-timemachine--start #'redguardtoo-git-timemachine-show-selected-revision))

(defun redguardtoo-git-rebase-interactive (&optional user-select-branch)
  "Rebase interactively on the closest branch or tag in git log output.
If USER-SELECT-BRANCH is not nil, rebase on the tag or branch selected by user."
  (interactive "P")
  (let* ((cmd "git --no-pager log --decorate --oneline -n 1024")
         (lines (my-lines-from-command-output cmd))
         (targets (delq nil
                        (mapcar (lambda (e)
                                  (when (and (string-match "^[a-z0-9]+ (\\([^()]+\\)) " e)
                                             (not (string-match "^[a-z0-9]+ (HEAD " e)))
                                    (match-string 1 e)))
                                lines)))
         based)
    (cond
     ((or (not targets) (null targets))
      (message "No tag or branch is found to base on."))
     ((or (not user-select-branch) (eq (length targets) 1))
      ;; select the closest/only tag or branch
      (setq based (my-git-extract-based (nth 0 targets) lines)))
     (t
      ;; select the one tag or branch
      (setq based (my-git-extract-based (completing-read "Select based: " targets)
                                        lines))))

    ;; start git rebase
    (when based
      (magit-rebase-interactive based nil))))



(provide 'git-timemachine-extras)
