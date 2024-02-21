;;; mine

;; speed up magit: https://jakemccrary.com/blog/2020/11/14/speeding-up-magit/

;;;###autoload
(defun my-fast-magit-refresh-toggle ()
  (interactive)
  (if (eq magit-status-sections-hook '(my-fast-magit-refresh-off))
      (my-fast-magit-refresh-on)
    (my-fast-magit-refresh-off)))

;;;###autoload
(defun my-fast-magit-refresh-on ()
  (interactive)
  (setq magit-status-sections-hook '(;;magit-insert-status-headers
                                     magit-insert-merge-log
                                     magit-insert-rebase-sequence
                                     magit-insert-am-sequence
                                     magit-insert-sequencer-sequence
                                     magit-insert-bisect-output
                                     magit-insert-bisect-rest
                                     magit-insert-bisect-log
                                     ;;magit-insert-untracked-files
                                     magit-insert-unstaged-changes
                                     magit-insert-staged-changes
                                     ;;magit-insert-stashes
                                     ;;magit-insert-unpushed-to-pushremote
                                        ;magit-insert-unpushed-to-upstream-or-recent
                                     ;;magit-insert-unpulled-from-pushremote
                                     ;;magit-insert-unpulled-from-upstream)
                                     ))
  (magit-refresh-buffer))

;;;###autoload
(defun my-fast-magit-refresh-off ()
  (interactive)
  (setq magit-status-sections-hook '(magit-insert-status-headers
                                     magit-insert-merge-log
                                     magit-insert-rebase-sequence
                                     magit-insert-am-sequence
                                     magit-insert-sequencer-sequence
                                     magit-insert-bisect-output
                                     magit-insert-bisect-rest
                                     magit-insert-bisect-log
                                     magit-insert-untracked-files
                                     magit-insert-unstaged-changes
                                     magit-insert-staged-changes
                                     magit-insert-stashes
                                     magit-insert-unpushed-to-pushremote
                                     magit-insert-unpushed-to-upstream-or-recent
                                     magit-insert-unpulled-from-pushremote
                                     magit-insert-unpulled-from-upstream))
  (magit-refresh-buffer))

;;; xenodium: doh!

;; https://xenodium.com/doh-undo-last-commit-magit-edition/

(defun ar/magit-soft-reset-head~1 ()
  "Soft reset current git repo to HEAD~1."
  (interactive)
  (magit-reset-soft "HEAD~1"))

;;; marcowahl: mw/ whole commit message history for status and buffer

(defun mw/advice/add-last-commit-messages (&optional directory cache)
  (mw/add-commit-messages (mw/vc-last-commit-messages)))

(defun mw/add-commit-messages (messages)
  "Add the messages to the pool of commit messages."
  (dolist (el messages)
    (ring-insert log-edit-comment-ring el)))

(defun mw/vc-last-commit-messages ()
  "Return list of last few commit messages."
  (let ((ctr-max 9)
        (ctr 0)
        messages)
    (catch 'break
      (while (not (= ctr ctr-max))
        (with-temp-buffer
          (let* ((revision-spec
                  (concat "HEAD~" (number-to-string ctr)))
                 (retval (vc-git--call (current-buffer) "log" revision-spec "-1" "--pretty=%B")))
            (if (not (= 0 retval))
                (throw 'break t))
            (setf messages (cons (buffer-string) messages))))
        (cl-incf ctr)))
    messages))

;;; akirak: magit-repolist extras

(defun akirak/magit-repolist-column-group (_id)
  (f-filename (abbreviate-file-name (f-parent default-directory))))

(defun akirak/magit-repolist-column-path (_id)
  (string-join `(,@(--map (cond
                           ((string-empty-p it)
                            "")
                           ((string-prefix-p "." it)
                            (substring it 1 2))
                           (t
                            (substring it 0 1)))
                          (split-string (f-short (f-parent default-directory)) "/"))
                 ,(f-filename default-directory))
               "/"))

(defun akirak/magit-repolist-column-commit-date (_id)
  "Insert a description of the repository's `HEAD' revision."
  (let ((v (or (magit-git-string "describe" "--tags")
               ;; If there are no tags, use the date in MELPA format.
               (magit-git-string "show" "--no-patch" "--format=%cd"
                                 "--date=format:%Y-%m-%d"))))
    (if (and v (string-match-p "\\`[0-9]" v))
        v
      (concat " " v))))

(defun akirak/magit-repolist-column-unmerged (id)
  (let ((lines (magit-git-lines "show-unmerged-branches"
                                (magit-repolist-column-branch id))))
    (string-join lines ",")))

(defun akirak/magit-repolist-column-origin (_id)
  (string-trim-left (or (magit-git-string "remote" "get-url" "origin")
                        "")
                    (rx (or "https://" "git@" "git://"))))

(defun akirak/magit-repolist-column-dirty (_id)
  "Insert a letter if there are uncommitted changes.

Show N if there is at least one untracked file.
Show U if there is at least one unstaged file.
Show S if there is at least one staged file.
Only one letter is shown, the first that applies."
  (cond ((magit-untracked-files) "?")
        ((magit-unstaged-files)  "*")
        ((magit-staged-files)    "+")))

(defun akirak/magit-modulelist-column-path (path)
  (let* ((segs (f-split path)))
    (apply #'f-join `(,@(--map (seq-take it 1) (-butlast segs))
                      ,(-last-item segs)))))

;; with bindings
;; k

;;;###autoload
(defun akirak/magit-repolist-kill-origin-url-at-point ()
  (interactive)
  (let* ((default-directory (tabulated-list-get-id))
         (url (magit-git-string "remote" "get-url" "origin")))
    (kill-new url)
    (message "Saved to kill ring: %s" url)))

;; "D"

;;;###autoload
(defun akirak/magit-repolist-trash-repository-at-point (&optional arg)
  (interactive "P")
  (when (akirak/trash-git-repository (tabulated-list-get-id) arg)
    (tabulated-list-delete-entry)))

;; "R"

;;;###autoload
(defun akirak/magit-repolist-rename-repository-at-point ()
  (interactive)
  (let* ((dir (tabulated-list-get-id))
         (worktrees (let ((default-directory dir))
                      (mapcar #'car (magit-list-worktrees)))))
    (if (> (length worktrees) 1)
        (user-error "You can't rename the repository if it has other working trees.")
      (let ((new-name (read-directory-name "New name: ")))
        (when (file-exists-p new-name)
          (user-error "File/directory %s already exists" new-name))
        (rename-file dir new-name)))))

;; daanturo

;;;###autoload
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
  "Automatically display the process buffer when it is updated.
https://github.com/magit/magit/wiki/Tips-and-Tricks#automatically-displaying-the-process-buffer"
  (dlet ((magit-display-buffer-noselect t))
    (magit-process-buffer)))

;;;###autoload
(defun daanturo-magit-blame-show-commit-at-point-current-line ()
  (interactive)
  (magit-show-commit (daanturo-git-blame-commit-hash)))

;;; mw

;;;###autoload
(defun mw-magit-mark-section ()
  "Mark the current magit section."
  (interactive)
  (let ((section (magit-current-section)))
    (if (eq section magit-root-section)
        (mark-whole-buffer)
      (when-let ((beg (oref section content)))
        (when-let ((end (oref section end)))
          (goto-char beg)
          (set-mark (point)) ;; my addition
          (push-mark end))))))

;;;###autoload
(defun mw-magit-narrow-to-section ()
  "Narrow to current magit section."
  (interactive)
  (let ((section (magit-current-section)))
    (when-let ((beg (oref section content)))
      (when-let ((end (oref section end)))
        (narrow-to-region beg end)))))

;;; https://www.reddit.com/r/emacs/comments/ojzv53/comment/h55vkl6

(defun lw-magit-checkout-last (&optional start-point)
  (interactive)
  (magit-branch-checkout "-" start-point))
(transient-append-suffix 'magit-branch "w"
  '("-" "last branch" lw-magit-checkout-last))


(provide 'my-magit-extras)
