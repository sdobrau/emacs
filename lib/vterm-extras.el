;;; mine

;; TODO: generalize
(defun my-vterm (&optional arg)
  (interactive "p")
  (let ((vterm-buf (--first (eq (buffer-local-value
                                 'major-mode it) 'vterm-mode) (buffer-list))))
    (if vterm-buf
        (switch-to-buffer vterm-buf)
      (vterm))))

(defun my-vterm-toggle ()
  (interactive "p")
  (switch-to-buffer (--first (eq (buffer-local-value
                                  'major-mode it) 'vterm-mode) (buffer-list))))

(defun my-vterm-v (&optional arg)
  (interactive "p")
  (let ((is-vterm-current (eq (symbol-value 'major-mode) 'vterm-mode)))
    (split-window-below-and-focus)
    (if is-vterm-current
        (vterm)
      (switch-to-buffer
       (--first (eq (buffer-local-value
                     'major-mode it) 'vterm-mode) (buffer-list))))))

(defun my-vterm-h (&optional arg)
  (interactive "p")
  (let ((is-vterm-current (eq (symbol-value 'major-mode) 'vterm-mode)))
    (split-window-right-and-focus)
    (if is-vterm-current
        (vterm)
      (switch-to-buffer
       (--first (eq (buffer-local-value
                     'major-mode it) 'vterm-mode) (buffer-list))))))

(defun my-vterm-consult-line ()
  (interactive)
  (if (vterm-copy-mode) nil
    (progn (vterm-copy-mode 1) (consult-line))))

(defun my-vterm-isearch-backward ()
  (interactive)
  (vterm-copy-mode)
  (isearch-backward))

(defun my-vterm-isearch-forward ()
  (interactive)
  (vterm-copy-mode)
  (isearch-forward))

;;; repl-like stuff

(defun my-vterm-insert-in-vterm-buffer-with-ret (string &optional buffer)
  (interactive "P")
  (my-vterm-insert-in-vterm-buffer string buffer t))

(defun my-vterm-insert-in-vterm-buffer (string &optional buffer do-ret)
  (interactive "P")
  (let ((buf (or buffer
                 (--filter (eq (buffer-local-value 'major-mode it))
                           'vterm-mode)))))
  (save-window-excursion
    (save-excursion
      (with-current-buffer buf
        (vterm-insert string)
        (if do-ret
            (vterm-send-return))))))

;;; dakra

;; Like normal `C-k'. Send `C-k' to libvterm but also put content in kill-ring
(defun vterm-send-C-k-and-kill ()
  "Send `C-k' to libvterm."
  (interactive)
  (kill-ring-save (point) (vterm-end-of-line))
  (vterm-send-key "k" nil nil t))

;;; akirak

(defun akirak/vterm-find-window ()
  (catch 'done
    (walk-window-tree
     (lambda (w)
       (when (eq 'vterm-mode
                 (buffer-local-value 'major-mode (window-buffer w)))
         (throw 'done w))))))

(defun akirak/vterm-cleanup ()
  (mapc (lambda (buf)
          (when (and (eq 'vterm-mode
                         (buffer-local-value 'major-mode buf))
                     (not (process-live-p (get-buffer-process buf))))
            (kill-buffer buf)))
        (buffer-list)))

(defun akirak/vterm-toggle-cd (&optional arg)
  "Toggle the vterm window, create a new buffer, or visit an existing buffer."
  (interactive "P")
  (akirak/vterm-cleanup)
  (pcase arg
    ('(16)
     (if-let ((buffer-list (--> (buffer-list)
                                (cl-remove-if-not
                                 (lambda (buf)
                                   (eq 'vterm-mode
                                       (buffer-local-value 'major-mode buf)))
                                 it)
                                (mapcar (lambda (buf)
                                          (cons (buffer-name buf) buf))
                                        it)))
              (buffer (completing-read "Select a vterm buffer" buffer-list)))
         (if-let ((window (akirak/vterm-find-window)))
             (progn
               (select-window window)
               (switch-to-buffer buffer))
           (pop-to-buffer buffer))
       (vterm-toggle-cd)))
    ('(4)
     (let ((cwd default-directory))
       (when-let ((window (akirak/vterm-find-window)))
         (select-window window))
       (let ((default-directory cwd))
         (vterm))))
    (_ (vterm-toggle-cd))))

(provide 'vterm-extras)
