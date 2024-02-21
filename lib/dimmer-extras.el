;; COMMIT: add fix for corfu and posframe-related

(defun my-corfu--popup-showing-p ()
  (and (bufferp " *corfu*")
       (or (window-live-p (get-buffer-window " *corfu*"))
           (let ((window (get-buffer-window " *corfu*" t)))
             (and (window-live-p window)
                  (frame-visible-p (window-frame window)))))))

;;;###autoload
(defun my-dimmer-configure-corfu ()
  "Convenience settings for corfu users."
  (with-no-warnings
    (add-to-list
     'dimmer-exclusion-regexp-list "^ \\*\ corfu\\*$")
    (add-to-list
     'dimmer-prevent-dimming-predicates #'my-corfu--popup-showing-p)))

(defun dimmer-process-all (&optional force)
  "Process all buffers and dim or un-dim each.

When FORCE is true some special logic applies.  Namely, we must
process all buffers regardless of the various dimming predicates.
While performing this scan, any buffer that would have been
excluded due to the predicates before should be un-dimmed now."
  (dimmer--dbg-buffers 1 "dimmer-process-all")
  (let* ((selected (current-buffer))
         (ignore   (cl-some (lambda (f) (and (fboundp f) (funcall f)))
                            dimmer-prevent-dimming-predicates))
         (visbufs  (dimmer-visible-buffer-list))
         (filtbufs (dimmer-filtered-buffer-list visbufs)))
    (dimmer--dbg 1 "dimmer-process-all: force %s" force)
    (when (or force (not ignore))
      (dolist (buf (if force visbufs filtbufs))
        (dimmer--dbg 2 "dimmer-process-all: buf %s" buf)
        (if (or (eq buf selected)
                (and force (not (memq buf filtbufs)))
                (and (eq buf dimmer-last-buffer) (not (memq selected filtbufs)))) ;; changed
            (dimmer-restore-buffer buf)
          (dimmer-dim-buffer buf dimmer-fraction))))
    (setq dimmer-last-buffer selected))) ;; changed



(provide 'dimmer-extras)
