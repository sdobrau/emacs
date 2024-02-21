;;; mine

;;;###autoload
(defun my-dired-emacs ()
  (interactive)
  (dired user-emacs-directory))

;;; redguardtoo

(defun redguardtoo-ediff-files ()
  "@see https://oremacs.com/2017/03/18/dired-ediff/."
  (interactive)
  (let* ((files (dired-get-marked-files))
         (wnd (current-window-configuration)))
    (cond
     ((<= (length files) 2)
      (let* ((file1 (car files))
             (file2 (if (cdr files)
                        (cadr files)
                      (read-file-name
                       "file: "
                       (dired-dwim-target-directory)))))
        (if (file-newer-than-file-p file1 file2)
            (ediff-files file2 file1)
          (ediff-files file1 file2))
        (add-hook 'ediff-after-quit-hook-internal
                  (lambda ()
                    (setq ediff-after-quit-hook-internal nil)
                    (set-window-configuration wnd)))))
     (t
      (error "no more than 2 files should be marked")))))

;;; xc for dired

(defun xc/dired-find-file (&optional arg)
  "Open each of the marked files, or the file under the
point, or when prefix arg, the next N files"
  (interactive "P")
  (mapc 'find-file (dired-get-marked-files nil arg)))

(defun dired-run-command (&optional filename)
  "Run file at point in a new buffer."
  (interactive)
  (unless filename
    (setq filename (expand-file-name
                    (dired-get-filename t t)
                    default-directory)))
  (let ((buffer (make-term
                 (file-name-nondirectory filename)
                 filename))
        (buffer-read-only nil))
    (with-current-buffer buffer
      ;; (term-mode)
      (term-char-mode)
      (term-set-escape-char ?\C-x))
    (set-process-sentinel
     (get-buffer-process buffer)
     (lambda (proc event)
       (when (not (process-live-p proc))
         (kill-buffer (process-buffer proc)))))
    (switch-to-buffer buffer)))

;;;###autoload
(defun daanturo-dired-home ()
  (interactive)
  (dired "~"))



(provide 'dired-extras)
