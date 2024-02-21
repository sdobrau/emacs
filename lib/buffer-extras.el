;;; mine

;;;; for use

;;;###autoload
(defun my-recenter-top ()
  (interactive)
  (recenter "Top"))

;;;###autoload
(defun my-recenter-bottom()
  (interactive)
  (recenter "Bottom"))

;;;###autoload
(defun my-next-buffer-of-same-current-mode ()
  (interactive)
  (switch-to-buffer (second (--filter
                             (eq
                              (buffer-local-value 'major-mode it)
                              (buffer-local-value 'major-mode (current-buffer)))
                             (window-next-buffers)))))

;;;###autoload
(defun my-previous-buffer-of-same-current-mode ()
  (interactive)
  (switch-to-buffer (nth 2 (--filter
          (eq
           (buffer-local-value 'major-mode it)
           (buffer-local-value 'major-mode (current-buffer)))
          (window-next-buffers)))))

;;;; auxiliary

(defun my-current-buffer-matches-mode (&optional mode)
  (eq (buffer-local-value 'major-mode (current-buffer)) mode))

  (defun current-buffer-another ()
    (interactive)
    (car (buffer-list)))

(defun buffer-first-buffer-matching-regexp (str &optional buflist)
  (--first (string-match-p (regexp-quote str) (buffer-name it))
           (or buflist (buffer-list))))

(defun buffer-first-buffer-matching-mode (&optional mode buflist)
  (--first (eq (buffer-local-value 'major-mode it)
               (or mode
                   (buffer-local-value 'major-mode current-buffer)))
           (or buflist (buffer-list))))

(defun list-buffers-of-mode-containing-string (mode string)
  (list-buffers-with-string string (list-buffers-with-mode mode)))

(defun list-buffers-with-mode (&optional mode buflist)
  "Return a list of all buffers with mode MODE (with no args, the mode of the
current buffer) in buffer BUFLIST (with no args, all buffers)."
  (--filter
   (eq (buffer-local-value 'major-mode it)
       (or mode
           (buffer-local-value 'major-mode (current-buffer))))
   (or buflist (buffer-list))))

(defun list-buffers-with-string (&optional string buflist)
  "Return a list of all buffers containing string STRING."
  (--filter
   (string-match-p string (buffer-name it))
   (or buflist (buffer-list))))

;; TODO: scoping ?
(defun kill-buffers-with-mode (&optional mode)
  "Kill all buffers with mode MODE etc TODO"
  (interactive)
  (let ((number 0)
        (m (or mode
               (buffer-local-value 'major-mode (current-buffer)))))
    (mapc (lambda (buf) (kill-buffer buf)
            (cl-incf number))
          (list-buffers-with-mode m))
    (message (format "Killed %s buffers of mode %s"
                     number
                     m))))

;;;; little buffer switching functions
;; TODO: generalize, my-switch-to-last-buffer-matching-mode
;;;;; backend:
;;;;;; jao-buffer-same-mode + my additions

;; TODO
;; (defun next-buffer-same-mode (&optional mode)
;;   "Switch to the next buffer of mode MODE.
;; Defaults to the mode of the current buffer."
;;   (interactive)
;;   (switch-to-buffer))

;; TODO
;; (defun previous-buffer-same-mode (&optional mode)
;; "Switch to the previous buffer of mode MODE.
;; Defaults to the mode of the current buffer."
;; (interactive)
;; (switch-to-buffer
;;  (nth 1 (reverse
;;          (list-buffers-with-mode
;;           (or (buffer-local-value 'major-mode
;;                                   (current-buffer))
;;               mode))))))

;; why? idk

(defun jao-buffer-same-mode (&rest modes)
  "Pop to a buffer with a mode among MODES, or the current one if not given."
  (interactive)
  (let* ((modes (or modes (list major-mode)))
         (pred (lambda (b)
                 (let ((b (get-buffer (if (consp b) (car b) b))))
                   (member (buffer-local-value 'major-mode b) modes)))))
    (switch-to-buffer (read-buffer "Buffer: " nil t pred))))

;; My contribution.
(defun jao-buffer-same-mode-string (modes str)
  "Pop to a buffer with a mode among MODES, or the current one if not given."
  (interactive)
  (let* ((modes (or modes (list major-mode)))
         (str str)
         (pred (lambda (b)
                 (let* ((b (get-buffer (if (consp b) (car b) b)))
                        (s (buffer-name b)))
                   (and (member (buffer-local-value 'major-mode b) modes)
                        (string-match-p str s))))))
    (my-persp-switch-to-buffer-maybe-popper-popup* (read-buffer "Buffer: " nil t pred))))

;; wrapper

(defun my-jao-buffer-same-mode-switch-or-pop (&optional modes str arg)
  "Switch to a buffer with a mode among MODES, or the current one if not given.
If prefixed, then pop to the buffer instead."
  (interactive)
  (let* ((modes (or modes (list major-mode)))
         (pred (lambda (b)
                 (let* ((b (get-buffer (if (consp b) (car b) b)))
                        (s (buffer-name b)))
                   (member (buffer-local-value 'major-mode b) modes)))))
    (if current-prefix-arg ;; my addition
        (pop-to-buffer (read-buffer "Buffer: " nil t pred))
      (switch-to-buffer (read-buffer "Buffer: " nil
                                     t pred)))))

(defun my-jao-buffer-same-mode-string-switch-or-pop (&optional modes str arg)
  "Switch to a buffer with a mode among MODES, or the current one if not given.
If prefixed, then pop to the buffer instead."
  (interactive)
  (let* ((modes (or modes (list major-mode)))
         (str str)
         (pred (lambda (b)
                 (let* ((b (get-buffer (if (consp b) (car b) b)))
                        (s (buffer-name b)))
                   (and (member (buffer-local-value 'major-mode b) modes)
                        (string-match-p str s))))))
    (if current-prefix-arg ;; my addition
        (pop-to-buffer (read-buffer "Buffer: " nil t pred))
      (switch-to-buffer (read-buffer "Buffer: " nil t pred)))))

;;;;; front-end

(defun my-kill-all-buffers-with-current-mode ()
  (interactive)
  (kill-buffers-with-mode))

;;;; kill buffer and close window

(defun my-kill-buffer-and-close-window (&optional u-arg)
  "Kill the buffer and close its window.
Meant to be used on main buffers and not pop-up buffers."
  (interactive "P")
  (kill-buffer-dwim)
  (delete-window))

;;; from xc: duplicate it

(defun xc/duplicate-buffer (&optional dup)
  "Copy current buffer to new buffer named DUP.

Default DUP name is `#<buffer-name>#'."
  (interactive)
  (let* ((orig (buffer-name))
         (dup (or dup (concat "%" orig "%" ))))
    (if (not (bufferp dup))
        (progn
          (get-buffer-create dup)
          (switch-to-buffer dup)
          (insert-buffer-substring orig)
          (message "Duplicate buffer `%s' created" dup))
      (error "Duplicate buffer already exists"))))

;;; from malb: alternative with prev and current buffer

;; TODO: exclude popups:
(defun malb/switch-to-previous-buffer ()
  "Switch to previously open buffer.

Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;; from kf: make file exec for shell

(defun kf-make-file-executable ()
  "Make current buffer's file have permissions 755 \(rwxr-xr-x)\.
This will save the buffer if it is not currently saved."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer)
  (chmod (buffer-file-name) 493))

;;; other

;; https://in.comum.org/smart-way-to-close-files.html

(defun my-kill-this-buffer ()
  (interactive)
  (catch 'quit
    (save-window-excursion
      (let (done)
        (when (and buffer-file-name (buffer-modified-p))
          (while (not done)
            (let ((response (read-char-choice
                             (format "Save file %s? (y, n, d, q) " (buffer-file-name))
                             '(?y ?n ?d ?q))))
              (setq done (cond
                          ((eq response ?q) (throw 'quit nil))
                          ((eq response ?y) (save-buffer) t)
                          ((eq response ?n) (set-buffer-modified-p nil) t)
                          ((eq response ?d) (diff-buffer-with-file) nil))))))
        (kill-buffer (current-buffer))))))

;;; from sanityinc: make window dedicated

(defun sanityinc/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

;;; https://www.olivertaylor.net/emacs/kill-buffer-dwim.html : just kill it

(defun kill-buffer-dwim (&optional u-arg)
  "Call kill-current-buffer, with C-u: call kill-buffer."
  (interactive "P")
  (if u-arg
      (call-interactively 'kill-buffer)
    (call-interactively 'kill-current-buffer)))

;;; trash
;; (defun buffer-exclude-not-with-mode (buf)
;; (defun buffer-has-mode-p (buf mode)
;;   (eq (buffer-local-value 'major-mode (current-buffer)) mode))

;; (defun switch-to-buffer-with-mode (&optional mode)
;;   (interactive)
;;   "Switch to a buffer, offering only buffers of MODE, by default current
;; mode."
;;   (let* ((mode (or mode
;;                    (buffer-local-value 'major-mode (current-buffer)))))
;;     (switch-to-buffer
;;      (read-buffer
;;       ;; the prompt
;;      (concat "Switch to buffers of mode " (format "%s" mode) " ["
;;              (buffer-name
;;               (car (window-next-buffers)))
;;              "]: ")
;;       nil ;; def
;;       t ;; require-match
;;       (buffer-has-mode-p)))))

;;; daanturo

;;;###autoload
(defun daanturo-completing-read-multiple-buffers (prompt)
  (let ((cands (seq-map #'buffer-name (buffer-list))))
    (completing-read-multiple
     prompt
     (lambda (str pred action)
       (if (equal action 'metadata)
           `(metadata (category . buffer))
         (complete-with-action action cands str pred))))))

;;;###autoload
(defun daanturo-tranpose-regions-between (buffer0 beg0 end0 buffer1 beg1 end1)
  (if (equal (get-buffer buffer0) (get-buffer buffer1))
      (with-current-buffer buffer0
        (save-excursion
          (transpose-regions beg0 end0 beg1 end1)
          (pulse-momentary-highlight-region (min beg0 beg1) (max end0 end1))))
    (let ((str0 (with-current-buffer buffer0 (buffer-substring beg0 end0)))
          (str1 (with-current-buffer buffer1 (buffer-substring beg1 end1))))
      (with-current-buffer buffer0
        (save-excursion
          (goto-char beg0)
          (delete-region beg0 end0)
          (insert str1)))
      (with-current-buffer buffer1
        (save-excursion
          (goto-char beg1)
          (delete-region beg1 end1)
          (insert str0))))))

;;;###autoload
(defun daanturo-kill-buffer-no-ask (buf)
  (with-current-buffer buf
    (set-buffer-modified-p nil)
    (dlet ((kill-buffer-query-functions nil))
      (kill-buffer buf))))

;;;###autoload
(defun daanturo-kill-multiple-buffers (buffers-to-kill &optional yes)
  (interactive (list (daanturo-completing-read-multiple-buffers "Kill: ") nil))
  (when (or yes
            (y-or-n-p (format "Kill %s buffers:\n%s"
                              (length buffers-to-kill)
                              buffers-to-kill)))
    (dolist (buf buffers-to-kill)
      (condition-case _
          (kill-buffer buf)
        (error (kill-buffer (get-buffer buf)))))))


;;;###autoload
;; TODO: keybinding
(defun daanturo-kill-other-file-buffers (&optional kill-special-buffers no-confirm)
  (interactive "P")
  (daanturo-kill-multiple-buffers
   (remove (current-buffer)
           (-filter (if kill-special-buffers #'always #'buffer-file-name)
                    (buffer-list)))
   no-confirm))

;;;###autoload
(defun daanturo-save-unsaved-buffers-with-files (&optional arg)
  "Like `save-some-buffers', buf only ask once.
ARG is passed to `save-buffer'."
  (interactive "P")
  (let ((bufs (seq-map #'buffer-name
                       (--filter (and (buffer-modified-p it) (buffer-file-name it))
                                 (buffer-list)))))
    (cond
     ((length= bufs 0)
      (message 'daanturo-save-unsaved-buffers-with-files
               ": all buffers are already saved."))
     ((y-or-n-p (format "Save %s files: \n%s ?"
                        (length bufs)
                        (string-join bufs "\n")))
      (dolist (buf bufs)
        (with-current-buffer buf
          (save-buffer arg)))))))

(defun daanturo-comment-buffer ()
  (interactive)
  (comment-region (point-min) (point-max)))

(defun daanturo-save-buffer-no-hook ()
  "Save buffer without running `before-save-hook' & `after-save-hook'."
  (interactive)
  (dlet ((before-save-hook nil)
         (after-save-hook nil))
    (save-buffer)))



(provide 'buffer-extras)
