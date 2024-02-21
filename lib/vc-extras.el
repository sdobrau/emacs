;;; mine

;;;; back

(defun my-git-commit-in-minibuffer (&optional commit-str)
  (interactive)
  (let ((commit-str (or commit-str
                        (read-string "commit msg: "))))
    (start-process "gitcom" nil
                   "git"
                   "commit"
                   "-am"
                   commit-str)))

;;; for work

(defun my-git-push ()
  (interactive)
  (start-process "gitpush" nil
                 "git"
                 "push"))

(defun my-git-commit-and-push (&optional commit-str)
  (interactive)
  (my-git-commit-in-minibuffer commit-str)
  (my-git-push))

(defun my-git-commit-and-push-old (&optional commit-str)
  (interactive)
  (my-git-commit-in-minibuffer commit-str)
  (my-git-push)
  (message "pushed to upstream"))

;;; front

(defun my-git-fix-commit ()
  (interactive)
  (my-git-commit-in-minibuffer "fix"))

(defun my-git-commit-fix-and-push ()
  (interactive)
  (my-git-commit-and-push "fix"))

(defun my-git-commit-fix-and-push-old ()
  (interactive)
  (my-git-commit-and-push-old "fix"))

;;; redguardtoo

(defun redguardtoo-hint-untracked-files ()
  "If untracked files and committed files share same extension, warn users."

  ;; don't scan whole home directory
  (unless (string= (file-truename default-directory) (file-truename "~/"))
    (let* ((exts (mapcar 'file-name-extension (my-lines-from-command-output "git diff-tree --no-commit-id --name-only -r HEAD")))
           (untracked-files (my-lines-from-command-output "git --no-pager ls-files --others --exclude-standard"))
           (lookup-ext (make-hash-table :test #'equal))
           rlt)
      ;; file extensions of files in HEAD commit
      (dolist (ext exts)
        (puthash ext t lookup-ext))
      ;; If untracked file has same file extension as committed files
      ;; maybe they should be staged too?
      (dolist (file untracked-files)
        (when (gethash (file-name-extension file) lookup-ext)
          (push (file-name-nondirectory file) rlt)))
      (when rlt
        (message "Stage files? %s" (mapconcat 'identity rlt " "))))))

(defun redguardtoo-git-check-status ()
  "Check git repo status."
  ;; use timer here to wait magit cool down
  (run-with-idle-timer 1 #'my-hint-untracked-files))

(defun redguardtoo-git-checkout-current-file ()
  "Git checkout current file."
  (interactive)
  (when (buffer-file-name))
  ;; (yes-or-no-p (format "git checkout %s?" HACK: don’t need this -- Me. --
  ;;                      (file-name-nondirectory (buffer-file-name)))))
  (let* ((filename (git-get-current-file-relative-path)))
    (shell-command (concat "git checkout " filename))
    (message "DONE! git checkout %s" filename)
    ;; my addition lol
    (revert-buffer)))

(defun git-get-current-file-relative-path ()
  "Get relative path of current file for Git."
  (replace-regexp-in-string (concat "^" (file-name-as-directory default-directory))
                            ""
                            buffer-file-name))



(provide 'vc-extras)
