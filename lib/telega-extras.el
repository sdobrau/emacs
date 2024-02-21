;;; mine
;;;; view image in exwm floating frame

(defun telega-image-view-file (tl-file &optional for-msg)
  "View image in telegram TL-FILE from message FOR-MSG."
  (cl-assert (telega-file--downloaded-p tl-file))
  (exwm-shell-command
   (concat "sxiv -b "
           (expand-file-name (telega--tl-get tl-file :local :path)))))

(defun telega-send-latest-kill-to-most-recent-chat ()
  (interactive)
  (with-current-buffer (buffer-first-buffer-matching-mode 'telega-chat-mode)
    (yank)
    (telega-chatbuf-newline-or-input-send)))

(provide 'telega-extras)
