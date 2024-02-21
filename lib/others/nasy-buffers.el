;;;###autoload
(defvar nasy/real-buffer-functions
  '(nasy/dired-buffer-p)
  "a list of predicate functions run to determine if a buffer is real, unlike
  `nasy/unreal-buffer-functions'. they are passed one argument: the buffer to be
  tested.
  should any of its function returns non-nil, the rest of the functions are
  ignored and the buffer is considered real.
  see `nasy/real-buffer-p' for more information.")

;;;###autoload
(defvar nasy/unreal-buffer-functions
  '(minibufferp nasy/special-buffer-p nasy/non-file-visiting-buffer-p)
  "a list of predicate functions run to determine if a buffer is *not* real,
  unlike `nasy/real-buffer-functions'. they are passed one argument: the buffer to
  be tested.
  should any of these functions return non-nil, the rest of the functions are
  ignored and the buffer is considered unreal.
  see `nasy/real-buffer-p' for more information.")

;;;###autoload
(defvar-local nasy/real-buffer-p nil
  "if non-nil, this buffer should be considered real no matter what. see
  `nasy/real-buffer-p' for more information.")

;;;###autoload
(defvar nasy/fallback-buffer-name "*scratch*"
  "the name of the buffer to fall back to if no other buffers exist (will create
  it if it doesn't exist).")


;;
;; functions

;;;###autoload
(defun nasy/buffer-frame-predicate (buf)
  "to be used as the default frame buffer-predicate parameter. returns nil if
  buf should be skipped over by functions like `next-buffer' and `other-buffer'."
  (or (nasy/real-buffer-p buf)
      (eq buf (nasy/fallback-buffer))))

;;;###autoload
(defun nasy/fallback-buffer ()
  "returns the fallback buffer, creating it if necessary. by default this is the
  scratch buffer. see `nasy/fallback-buffer-name' to change this."
  (let (buffer-list-update-hook)
    (get-buffer-create nasy/fallback-buffer-name)))

;;;###autoload
(defalias 'nasy/buffer-list #'buffer-list)


;;;###autoload
(defun nasy/project-root (&optional dir)
  "return the project root of dir (defaults to `default-directory').
  returns nil if not in a project."
  (let ((projectile-project-root
         (unless dir (bound-and-true-p projectile-project-root)))
        projectile-require-project-root)
    (projectile-project-root dir)))


;;;###autoload
(defun nasy/project-buffer-list (&optional project)
  "return a list of buffers belonging to the specified project.
  if project is nil, default to the current project.
  if no project is active, return all buffers."
  (let ((buffers (nasy/buffer-list)))
    (if-let* ((project-root
               (if project (expand-file-name project)
                 (nasy/project-root))))
        (cl-loop for buf in buffers
                 if (projectile-project-buffer-p buf project-root)
                 collect buf)
      buffers)))

;;;###autoload
(defun nasy/open-projects ()
  "return a list of projects with open buffers."
  (cl-loop with projects = (make-hash-table :test 'equal :size 8)
           for buffer in (nasy/buffer-list)
           if (buffer-live-p buffer)
           if (nasy/real-buffer-p buffer)
           if (with-current-buffer buffer (nasy/project-root))
           do (puthash (abbreviate-file-name it) t projects)
           finally return (hash-table-keys projects)))

;;;###autoload
(defun nasy/dired-buffer-p (buf)
  "returns non-nil if buf is a dired buffer."
  (with-current-buffer buf (derived-mode-p 'dired-mode)))

;;;###autoload
(defun nasy/special-buffer-p (buf)
  "returns non-nil if buf's name starts and ends with an *."
  (equal (substring (buffer-name buf) 0 1) "*"))

;;;###autoload
(defun nasy/temp-buffer-p (buf)
  "returns non-nil if buf is temporary."
  (equal (substring (buffer-name buf) 0 1) " "))

;;;###autoload
(defun nasy/visible-buffer-p (buf)
  "return non-nil if buf is visible."
  (get-buffer-window buf))

;;;###autoload
(defun nasy/buried-buffer-p (buf)
  "return non-nil if buf is not visible."
  (not (nasy/visible-buffer-p buf)))

;;;###autoload
(defun nasy/non-file-visiting-buffer-p (buf)
  "returns non-nil if buf does not have a value for `buffer-file-name'."
  (not (buffer-file-name buf)))

;;;###autoload
(defun nasy/real-buffer-list (&optional buffer-list)
  "return a list of buffers that satify `nasy/real-buffer-p'."
  (cl-remove-if-not #'nasy/real-buffer-p (or buffer-list (nasy/buffer-list))))

;;;###autoload
(defun nasy/real-buffer-p (buffer-or-name)
  "returns t if buffer-or-name is a 'real' buffer.
  a real buffer is a useful buffer; a first class citizen in doom. real ones
  should get special treatment, because we will be spending most of our time in
  them. unreal ones should be low-profile and easy to cast aside, so we can focus
  on real ones.
  the exact criteria for a real buffer is:
    1. a non-nil value for the buffer-local value of the `nasy/real-buffer-p'
       variable or
    2. any function in `nasy/real-buffer-functions' returns non-nil or
    3. none of the functions in `nasy/unreal-buffer-functions' must return
       non-nil.
  if buffer-or-name is omitted or nil, the current buffer is tested."
  (or (bufferp buffer-or-name)
      (stringp buffer-or-name)
      (signal 'wrong-type-argument (list '(bufferp stringp) buffer-or-name)))
  (when-let (buf (get-buffer buffer-or-name))
    (and (buffer-live-p buf)
         (not (nasy/temp-buffer-p buf))
         (or (buffer-local-value 'nasy/real-buffer-p buf)
             (run-hook-with-args-until-success 'nasy/real-buffer-functions buf)
             (not (run-hook-with-args-until-success 'nasy/unreal-buffer-functions buf))))))

;;;###autoload
(defun nasy/unreal-buffer-p (buffer-or-name)
  "return t if buffer-or-name is an 'unreal' buffer.
  see `nasy/real-buffer-p' for details on what that means."
  (not (nasy/real-buffer-p buffer-or-name)))

;;;###autoload
(defun nasy/buffers-in-mode (modes &optional buffer-list derived-p)
  "return a list of buffers whose `major-mode' is `eq' to mode(s).
  if derived-p, test with `derived-mode-p', otherwise use `eq'."
  (let ((modes (nasy/enlist modes)))
    (cl-remove-if-not (if derived-p
                          (lambda (buf)
                            (with-current-buffer buf
                              (apply #'derived-mode-p modes)))
                        (lambda (buf)
                          (memq (buffer-local-value 'major-mode buf) modes)))
                      (or buffer-list (nasy/buffer-list)))))

;;;###autoload
(defun nasy/visible-windows (&optional window-list)
  "return a list of the visible, non-popup (dedicated) windows."
  (cl-loop for window in (or window-list (window-list))
           when (or (window-parameter window 'visible)
                    (not (window-dedicated-p window)))
           collect window))

;;;###autoload
(defun nasy/visible-buffers (&optional buffer-list)
  "return a list of visible buffers (i.e. not buried)."
  (if buffer-list
      (cl-remove-if-not #'get-buffer-window buffer-list)
    (delete-dups (mapcar #'window-buffer (window-list)))))

;;;###autoload
(defun nasy/buried-buffers (&optional buffer-list)
  "get a list of buffers that are buried."
  (cl-remove-if #'get-buffer-window (or buffer-list (nasy/buffer-list))))

;;;###autoload
(defun nasy/matching-buffers (pattern &optional buffer-list)
  "get a list of all buffers that match the regex pattern."
  (cl-loop for buf in (or buffer-list (nasy/buffer-list))
           when (string-match-p pattern (buffer-name buf))
           collect buf))

;;;###autoload
(defun nasy/set-buffer-real (buffer flag)
  "forcibly mark buffer as flag (non-nil = real)."
  (with-current-buffer buffer
    (setq nasy/real-buffer-p flag)))

;;;###autoload
(defun nasy/kill-buffer-and-windows (buffer)
  "kill the buffer and delete all the windows it's displayed in."
  (dolist (window (get-buffer-window-list buffer))
    (unless (one-window-p t)
      (delete-window window)))
  (kill-buffer buffer))

;;;###autoload
(defun nasy/fixup-windows (windows)
  "ensure that each of windows is showing a real buffer or the fallback buffer."
  (dolist (window windows)
    (with-selected-window window
      (when (nasy/unreal-buffer-p (window-buffer))
        (previous-buffer)
        (when (nasy/unreal-buffer-p (window-buffer))
          (switch-to-buffer (nasy/fallback-buffer)))))))

;;;###autoload
(defun nasy/kill-buffer-fixup-windows (buffer)
  "kill the buffer and ensure all the windows it was displayed in have switched
  to a real buffer or the fallback buffer."
  (let ((windows (get-buffer-window-list buffer)))
    (kill-buffer buffer)
    (nasy/fixup-windows (cl-remove-if-not #'window-live-p windows))))

;;;###autoload
(defun nasy/kill-buffers-fixup-windows (buffers)
  "kill the buffers and ensure all the windows they were displayed in have
  switched to a real buffer or the fallback buffer."
  (let ((seen-windows (make-hash-table :test 'eq :size 8)))
    (dolist (buffer buffers)
      (let ((windows (get-buffer-window-list buffer)))
        (kill-buffer buffer)
        (dolist (window (cl-remove-if-not #'window-live-p windows))
          (puthash window t seen-windows))))
    (nasy/fixup-windows (hash-table-keys seen-windows))))

;;;###autoload
(defun nasy/-kill-matching-buffers (pattern &optional buffer-list)
  "kill all buffers (in current workspace or in buffer-list) that match the
  regex pattern. returns the number of killed buffers."
  (let ((buffers (nasy/matching-buffers pattern buffer-list)))
    (dolist (buf buffers (length buffers))
      (kill-buffer buf))))


;;
;; hooks

;;;###autoload
(defun nasy/mark-buffer-as-real-h ()
  "hook function that marks the current buffer as real."
  (nasy/set-buffer-real (current-buffer) t))


;;
;; interactive commands

;;;###autoload
(defun nasy/kill-this-buffer-in-all-windows (buffer &optional dont-save)
  "kill buffer globally and ensure all windows previously showing this buffer
  have switched to a real buffer or the fallback buffer.
  if dont-save, don't prompt to save modified buffers (discarding their changes)."
  (interactive
   (list (current-buffer) current-prefix-arg))
  (cl-assert (bufferp buffer) t)
  (when (and (buffer-modified-p buffer) dont-save)
    (with-current-buffer buffer
      (set-buffer-modified-p nil)))
  (nasy/kill-buffer-fixup-windows buffer))


(defun nasy/message-or-count (interactive message count)
  (if interactive
      (message message count)
    count))

;;;###autoload
(defun nasy/kill-all-buffers (&optional buffer-list interactive)
  "kill all buffers and closes their windows.
  if the prefix arg is passed, doesn't close windows and only kill buffers that
  belong to the current project."
  (interactive
   (list (if current-prefix-arg
             (nasy/project-buffer-list)
           (nasy/buffer-list))
         t))
  (if (null buffer-list)
      (message "no buffers to kill")
    (save-some-buffers)
    (delete-other-windows)
    (when (memq (current-buffer) buffer-list)
      (switch-to-buffer (nasy/fallback-buffer)))
    (mapc #'kill-buffer buffer-list)
    (nasy/message-or-count
     interactive "killed %d buffers"
     (- (length buffer-list)
        (length (cl-remove-if-not #'buffer-live-p buffer-list))))))

;;;###autoload
(defun nasy/kill-other-buffers (&optional buffer-list interactive)
  "kill all other buffers (besides the current one).
  if the prefix arg is passed, kill only buffers that belong to the current
  project."
  (interactive
   (list (delq (current-buffer)
               (if current-prefix-arg
                   (nasy/project-buffer-list)
                 (nasy/buffer-list)))
         t))
  (mapc #'nasy/kill-buffer-and-windows buffer-list)
  (nasy/message-or-count
   interactive "killed %d other buffers"
   (- (length buffer-list)
      (length (cl-remove-if-not #'buffer-live-p buffer-list)))))

;;;###autoload
(defun nasy/kill-matching-buffers (pattern &optional buffer-list interactive)
  "kill buffers that match pattern in buffer-list.
  if the prefix arg is passed, only kill matching buffers in the current project."
  (interactive
   (list (read-regexp "buffer pattern: ")
         (if current-prefix-arg
             (nasy/project-buffer-list)
           (nasy/buffer-list))
         t))
  (nasy/-kill-matching-buffers pattern buffer-list)
  (when interactive
    (message "killed %d buffer(s)"
             (- (length buffer-list)
                (length (cl-remove-if-not #'buffer-live-p buffer-list))))))

;;;###autoload
(defun nasy/kill-buried-buffers (&optional buffer-list interactive)
  "kill buffers that are buried.
  if project-p (universal argument), only kill buried buffers belonging to the
  current project."
  (interactive
   (list (nasy/buried-buffers
          (if current-prefix-arg (nasy/project-buffer-list)))
         t))
  (mapc #'kill-buffer buffer-list)
  (nasy/message-or-count
   interactive "killed %d buried buffers"
   (- (length buffer-list)
      (length (cl-remove-if-not #'buffer-live-p buffer-list)))))

;;;###autoload
(defun nasy/kill-project-buffers (project &optional interactive)
  "kill buffers for the specified project."
  (interactive
   (list (if-let (open-projects (nasy/open-projects))
             (completing-read
              "kill buffers for project: " open-projects
              nil t nil nil
              (if-let* ((project-root (nasy/project-root))
                        (project-root (abbreviate-file-name project-root))
                        ((member project-root open-projects)))
                  project-root))
           (message "no projects are open!")
           nil)
         t))
  (when project
    (let ((buffer-list (nasy/project-buffer-list project)))
      (nasy/kill-buffers-fixup-windows buffer-list)
      (nasy/message-or-count
       interactive "killed %d project buffers"
       (- (length buffer-list)
          (length (cl-remove-if-not #'buffer-live-p buffer-list)))))))


;;;###autoload
(defun nasy/kill-buffers-no-company-box ()
  "kill all buffers except company box buffers."
  (interactive)
  (nasy/kill-all-buffers
   (cl-loop for buffer in (nasy/buffer-list)
            when (not (string-match-p "company-box" (buffer-name buffer)))
            collect buffer)))


;;;###autoload
(defun nasy/scratch ()
  "switch buffer to scratch."
  (interactive)
  (switch-to-buffer "*scratch*"))



(provide 'nasy-buffers)
