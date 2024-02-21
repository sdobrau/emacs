;;; mine

;;;; TODO:
;s/; tab-line-excluded-modes to do (tab-line-mode -1) otherwise it doesn’t work

;;; https://andreyorst.gitlab.io/posts/2020-05-07-making-emacs-tabs-work-like-in-atom/

(defun tab-line-close-tab (&optional e)
  "Close the selected tab. If tab is presented in another window,
  close the tab by using `bury-buffer` function.  If tab is uniq
  to all existing windows, kill the buffer with `kill-buffer`
  function.  Lastly, if no tabs left in the window, it is deleted
  with `delete-window` function."
  (interactive "e")
  (let* ((posnp (event-start e))
         (window (posn-window posnp))
         (buffer (get-pos-property 1 'tab (car (posn-string posnp)))))
    (with-selected-window window
      (let ((tab-list (tab-line-tabs-window-buffers))
            (buffer-list (flatten-list
                          (seq-reduce (lambda (list window)
                                        (select-window window t)
                                        (cons (tab-line-tabs-window-buffers) list))
                                      (window-list) nil))))
        (select-window window)
        (if (> (seq-count (lambda (b) (eq b buffer)) buffer-list) 1)
            (progn
              (if (eq buffer (current-buffer))
                  (bury-buffer)
                (set-window-prev-buffers window (assq-delete-all buffer (window-prev-buffers)))
                (set-window-next-buffers window (delq buffer (window-next-buffers))))
              (unless (cdr tab-list)
                (ignore-errors (delete-window window))))
          (and (kill-buffer buffer)
               (unless (cdr tab-list)
                 (ignore-errors (delete-window window)))))))
    (force-mode-line-update)))


;;; yao-kaz: tab-line next, tab-line-previous


(defun yao-kaz-tab-line-tab-previous ()
  "Switch to the previous tab in the tab-line"
  (interactive)
  (let* ((tabs (funcall tab-line-tabs-function))
         (tabs-max-index (1- (length tabs)))
         (cur-buff (current-buffer))
         (cur-buff-index (cl-position cur-buff tabs))
         (prev-buff-index (1- cur-buff-index)))
    (cond
     ;; If only one tab, don't do anything.
     ((<= tabs-max-index 0) nil)
     ;; If going beyond 0, go to the max index.
     ((< prev-buff-index 0) (switch-to-buffer (seq-elt tabs tabs-max-index)))
     ;; Otherwise, go to the previous index.
     (t (switch-to-buffer (seq-elt tabs prev-buff-index))))))

(defun yao-kaz-tab-line-tab-next ()
  "Switch to the next tab in the tab-line"
  (interactive)
  (let* ((tabs (funcall tab-line-tabs-function))
         (tabs-max-index (1- (length tabs)))
         (cur-buff (current-buffer))
         (cur-buff-index (cl-position cur-buff tabs))
         (next-buff-index (1+ cur-buff-index)))
    (cond
     ;; If only one tab, don't do anything.
     ((<= tabs-max-index 0) nil)
     ;; If going beyond the max, go to index 0.
     ((> next-buff-index tabs-max-index) (switch-to-buffer (seq-elt tabs 0)))
     ;; Otherwise, go to the next index.
     (t (switch-to-buffer (seq-elt tabs next-buff-index))))))



;;; TODO:
;;; open all files in dir as tabs
;;; temporarily don’t group them by mode for this
;;; apply macro to all files in tabs
;;; find-file-in-tab-line-group (temporarily don’t group tabs by mode)
;;; find-file multiple in window
;;; switch to buffer (tabs only) C-s-x quick preview frog-jump ?
;;; tab-line-switch-to-tab
;;; TODO: frog-jump for tabs
;;; TODO: make tabs fucking work

(provide 'tab-line-extras)
