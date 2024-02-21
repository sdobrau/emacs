;; -*- lexical-binding: t; -*-

(require 'daanturo-version-control)

(defun daanturo-magit/switch-project-and-status ()
  "Select a project and run `magit-status' there."
  (interactive)
  (let ((old-window (selected-window)))
    ;; TODO: please occupy current window if the current one is `magit', too
    (magit-status-setup-buffer (project-prompt-project-dir))
    (delete-other-windows)))

;;;###autoload
(defun daanturo-magit-diff-commit-or-branch-at-point-to-HEAD ()
  (interactive)
  (magit-diff-range
   (format "%s.." (magit-branch-or-commit-at-point))))

;;;###autoload
(defun daanturo-magit-discard-no-trash ()
  (interactive)
  (dlet ((magit-delete-by-moving-to-trash nil))
    (magit-discard)))

;;;###autoload
(defun daanturo-magit-log-surrounding-commits (commit branch)
  "(`magit-log-merged' COMMIT BRANCH)."
  (interactive
   (list (or (magit-commit-at-point)
             (magit-read-branch-or-commit "`daanturo-magit-log-surrounding-commits': "))
         (or
          ;; (and (= 1 (length magit-buffer-revisions))
          ;;      (car magit-buffer-revisions))
          (magit-branch-at-point)
          (magit-get-current-branch)
          (magit-read-branch "Branch: "))))
  (declare (interactive-only t))
  (apply #'magit-log-merged commit branch
         (magit-log-arguments)))

;;;###autoload
(defun daanturo-magit-read-branch-or-commit-initial-input-as-commit-at-point---around-a (func &rest args)
  (let ((commit (magit-commit-at-point)))
    (minibuffer-with-setup-hook
        (lambda ()
          (when (and commit
                     (not (string-empty-p commit))
                     (string-empty-p (minibuffer-contents-no-properties)))
            (insert commit)))
      (apply func args))))

;;;###autoload
(defun magit-wiki-auto-display-magit-process-buffer (&rest _)
  "Automatically display the process buffer when it is updated."
  (let ((magit-display-buffer-noselect t))
    (magit-process-buffer)))

;;;###autoload
(defun daanturo-magit-blame-show-commit-at-point-current-line ()
  (interactive)
  (magit-show-commit (daanturo-git-blame-commit-hash)))

(provide 'daanturo-magit)
