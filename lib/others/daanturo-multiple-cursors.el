;; -*- lexical-binding: t; -*-

;;; daanturo

;; alter between `multiple-cursors' & `evil-mc' when switching `evil' states? --
;; rejected, for now we only use 1 implementation, so adaptation to an official
;; one in the future should be more lenient

(defun daanturo-mc/remove-obsolete-commands-from-lists (&optional all)
  "With non-nil ALL, remove commands from other packages, too."
  (let ((obsolete-commands
         (seq-filter
          (lambda (cmd)
            (and (not (commandp cmd))
                 (if all
                     t
                   (string-match-p (format "^%s" 'daanturo-) (symbol-name cmd)))))
          (append mc/cmds-to-run-once mc/cmds-to-run-for-all))))
    (dolist (cmd obsolete-commands)
      (daanturo-delete-in-list! 'mc/cmds-to-run-once cmd)
      (daanturo-delete-in-list! 'mc/cmds-to-run-for-all cmd))
    obsolete-commands))

;;;###autoload
(defun daanturo-mc/move-command-between-run-for-all-and-once (from command to)
  "FROM & TO is in `mc/cmds-to-run-{once,for-all}'.
Useful when COMMAND is mistakenly placed in FROM as manual
editing procedure is tedious."
  (interactive
   (progn
     (require 'multiple-cursors-core)
     (let* ((lists '(mc/cmds-to-run-once mc/cmds-to-run-for-all))
            (from (intern (completing-read "Move command from: " lists)))
            (command (intern (completing-read "Command: " (symbol-value from))))
            (to (intern (completing-read (format "Move `%s' to: " command)
                                         (remove from lists)))))
       (list from command to))))
  (daanturo-mc/remove-obsolete-commands-from-lists)
  (mc/save-lists)
  (add-to-list to command)
  (set from (remove command (symbol-value from)))
  (mc/save-lists))

(defvar daanturo-mc/buffer-saved-cursor-alist nil
  "List of fake cursors which were created by `multiple-cursors'.
Keys are buffer objects. We can make it a buffer-local variable,
but it's easier to watch a global variable.")

(cl-defun daanturo-mc/get-paused-cursor-list (&optional (buffer (current-buffer)))
  (alist-get buffer daanturo-mc/buffer-saved-cursor-alist nil nil #'equal))

(cl-defun daanturo-mc/set-paused-cursor-list (positions &optional (buffer (current-buffer)))
  (setf (alist-get buffer daanturo-mc/buffer-saved-cursor-alist nil nil #'equal) positions))

(defun daanturo-mc/fake-cursor-pos-list ()
  (seq-map #'overlay-start (mc/all-fake-cursors)))

;;;###autoload
(defun daanturo-mc/--save-fake-cursors ()
  (when (mc/all-fake-cursors)
    (daanturo-mc/set-paused-cursor-list (daanturo-mc/fake-cursor-pos-list))))

(defvar daanturo-mc/indicate-paused-cursors-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode daanturo-mc/indicate-paused-cursors-mode
  "Show paused `multiple-cursors''s cursors."
  :keymap daanturo-mc/indicate-paused-cursors-mode-map
  :lighter " Paused cursors"

  (let ((positions (daanturo-mc/get-paused-cursor-list)))

    (when (and (bound-and-true-p multiple-cursors-mode)
               daanturo-mc/indicate-paused-cursors-mode)
      (message "Please turn %s off to avoid confusion" 'daanturo-mc/indicate-paused-cursors-mode))

    ;; `overlay' is slower than `text-properties' but doesn't modify the buffer

    ;; (set-text-properties (point-min) (point-max) nil)
    ;; (if daanturo-mc/indicate-paused-cursors-mode
    ;;     (dolist (pos positions)
    ;;       (put-text-property
    ;;        pos (1+ pos)
    ;;        'font-lock-face
    ;;        'mc/cursor-face))
    ;;   nil)

    (remove-overlays (point-min) (point-max) 'daanturo-mc t)
    (if daanturo-mc/indicate-paused-cursors-mode
        (dolist (pos positions)
          (let ((ol (make-overlay pos (1+ pos))))
            (overlay-put ol 'face 'mc/cursor-face)
            (overlay-put ol 'daanturo-mc t)))
      nil)

    ;;
    ))

;;;###autoload
(defun daanturo-mc/toggle-cursor-at-point ()
  "Toggle the cursor at point.

If `multiple-cursors-mode' is active, remove the cursor at point
and jump back the previous one (because the point is always a
real cursor).

Else if paused cursors are showing, toggle the cursor at point.

Else discard paused cursors and begin toggling new ones."
  (interactive)
  (if multiple-cursors-mode
      (let ((p (point)))
        (mc/cycle-backward)
        (mc/remove-fake-cursor (mc/fake-cursor-at-point p)))
    (let ((position-list
           (if daanturo-mc/indicate-paused-cursors-mode
               (daanturo-mc/get-paused-cursor-list)
             nil)))
      (setq position-list
            (if (member (point) position-list)
                (remove (point) position-list)
              (append position-list (list (point)))))
      (daanturo-mc/set-paused-cursor-list position-list)
      (daanturo-mc/indicate-paused-cursors-mode))))

;;;###autoload
(defun daanturo-mc/pause-cursors ()
  "Freeze cursors."
  (interactive)
  (when multiple-cursors-mode
    (daanturo-mc/--save-fake-cursors)
    (daanturo-mc/indicate-paused-cursors-mode)
    (mc/disable-multiple-cursors-mode)))

;;;###autoload
(defun daanturo-mc/create-fake-cursor-at (position)
  (save-excursion
    (goto-char position)
    (mc/create-fake-cursor-at-point)))

;;;###autoload
(defun daanturo-mc/resume-cursors ()
  "Thaw cursors."
  (interactive)
  (let ((cursor-list (daanturo-mc/get-paused-cursor-list)))
    (mapc #'daanturo-mc/create-fake-cursor-at cursor-list)
    (daanturo-mc/indicate-paused-cursors-mode -1)
    (mc/maybe-multiple-cursors-mode)))

;;;###autoload
(defun daanturo-mc/toggle-pausing-cursors ()
  "If `multiple-cursors-mode' is active, freeze fake cursors, else thaw them."
  (interactive)
  (if multiple-cursors-mode
      (daanturo-mc/pause-cursors)
    (daanturo-mc/resume-cursors)))

;; `multiple-cursors-mode-disabled-hook' is ran after removing fake cursors, so we advise this instead
(defun daanturo-mc/save-cursor-when-not-region-and-not-prefix-args--a (&rest _)
  (unless
      (or
       ;;  When regions are active, `mc/keyboard-quit' unmark them instead
       (use-region-p)
       current-prefix-arg)
    (daanturo-mc/--save-fake-cursors)))

(defun daanturo-mc/resume-paused-cursors-when-indicated (&rest _)
  (when daanturo-mc/indicate-paused-cursors-mode
    (daanturo-mc/resume-cursors))
  (daanturo-mc/indicate-paused-cursors-mode -1))

(defun daanturo-mc/goto-last-cursor ()
  (interactive)
  (when (< (point) (seq-max (daanturo-mc/fake-cursor-pos-list)))
    (mc/cycle (mc/last-fake-cursor-before (point-max))
              nil
              "")))

(defun daanturo-mc/goto-first-cursor ()
  (interactive)
  (when (> (point) (seq-min (daanturo-mc/fake-cursor-pos-list)))
    (mc/cycle (mc/first-fake-cursor-after (point-min))
              nil
              "")))

;; (defmacro daanturo-mc/for-each-fake-cursor-reversed-ordered (&rest forms)
;;   "Run FORMS for each fake cursor from the last to first.")

;; evil's non-insert states don't play well with `multiple-cursors', we
;; shouldn't set `evil-state' where `evil-normal-state' is easily switched to by
;; accident
(with-eval-after-load 'evil
  (dolist (k (list (kbd "RET") [return]))
    (evil-define-key* '(normal motion) daanturo-mc/indicate-paused-cursors-mode-map
      k #'daanturo-mc/toggle-cursor-at-point)))

;; It's confusing to display previously paused cursors when active
;; (daanturo-bind 'mc/keymap [remap daanturo-mc/indicate-paused-cursors-mode] #'ignore)

(defun daanturo-mc/sort-regions-or-sexps-by-length ()
  (interactive)
  (require 'mc-separate-operations)
  (unless (use-region-p)
    (mc/execute-command-for-all-cursors 'mark-sexp))
  (setq mc--strings-to-replace (sort (mc--ordered-region-strings) 'daanturo-by-length<))
  (mc--replace-region-strings))

;;;###autoload
(defun daanturo-mc/edit-lines-dwim ()
  "Like `mc/edit-lines', but operate on current paragraph by default."
  (interactive)
  (if (use-region-p)
      (mc/edit-lines)
    (progn
      (daanturo-mark-inner-paragraph)
      (mc/edit-ends-of-lines)
      (mc/for-each-cursor-ordered
       (deactivate-mark)))))

;;;###autoload
(defun daanturo-mc/mark-next-with-same-indent (&optional back)
  (interactive "P")
  (save-excursion
    (let ((found (daanturo-find-next-position-with-same-indent-as-current-line (point) back)))
      (when found
        (daanturo-mc/create-fake-cursor-at found)
        (mc/maybe-multiple-cursors-mode)))))

(defun daanturo-mc/--buffer-substring-at-fake-cursor (position bwd fwd)
  (buffer-substring-no-properties
   (+ bwd position)
   (+ fwd position)))

(defun daanturo-mc/get-matched-fake-cursor-start-list
    (bwd fwd real-cursor-content &optional fake-cursor-positions)
  (seq-filter
   (lambda (p)
     (string= real-cursor-content
              (daanturo-mc/--buffer-substring-at-fake-cursor p bwd fwd)))
   (or fake-cursor-positions
       (daanturo-mc/fake-cursor-pos-list))))

(defun daanturo-mc/around-of-fake-cursors-match-real-p
    (beg-pos end-pos &optional fake-cursor-positions)
  "Return whether the relative contents around fake cursors match
the real cursor's from BEG-POS to END-POS.
When FAKE-CURSOR-POSITIONS is non-nil, compute there."
  (let ((fake-cursor-positions (or fake-cursor-positions (daanturo-mc/fake-cursor-pos-list))))
    (= (length fake-cursor-positions)
       (length
        (daanturo-mc/get-matched-fake-cursor-start-list
         (- beg-pos (point))
         (- end-pos (point))
         (buffer-substring-no-properties beg-pos end-pos)
         fake-cursor-positions)))))

(defun daanturo-mc/switch-to-iedit-by-region (beg end &optional fake-cursor-positions-input)
  "For the real cursor and `multiple-cursors''s fake cursors, activate `iedit'.
With the content between REGION-BEG and REG-END as the mandatory
match. For each of fake cursor, the content to be compared will
be taken relatively just like [REGION-BEG `point' REGION-END]."
  (interactive "r")
  (if (bound-and-true-p iedit-mode)
      (user-error "%s is already enabled." 'iedit-mode)
    (let* ((fake-poss (or fake-cursor-positions-input (daanturo-mc/fake-cursor-pos-list)))
           (bwd (- beg (point)))
           (fwd (- end (point))))
      (if (daanturo-mc/around-of-fake-cursors-match-real-p beg end fake-poss)
          (progn
            ;; Disabling `multiple-cursors' first may erase `iedit''s overlays (?)
            (when (use-region-p) (deactivate-mark))
            (mc/keyboard-quit)
            (daanturo-iedit-make-occurences-at-points bwd fwd (cons (point) fake-poss)))
        (message "There are unmatched cursors.")))))

(defun daanturo-mc/switch-to-iedit-by-symbol (&optional fake-cursor-positions-input)
  "For the real cursor and `multiple-cursors''s fake cursors, activate `iedit'.
Use symbol at point to match."
  (interactive)
  (-let* (((beg . end) (bounds-of-thing-at-point 'symbol)))
    (daanturo-mc/switch-to-iedit-by-region beg end fake-cursor-positions-input)))

;;;###autoload
(defun daanturo-mc/switch-to-iedit ()
  (interactive)
  (if (use-region-p)
      (daanturo-mc/switch-to-iedit-by-region (region-beginning) (region-end))
    (daanturo-mc/switch-to-iedit-by-symbol)))

;; Just running `iedit-switch-to-mc-mode' to switch back may not work
;;;###autoload
(defun daanturo-completion-by-minibuffer-for-mc-a (func &rest args)
  "Switch from `multiple-cursors-mode' to `iedit-mode' to propagate completions.
Condition: all fake cursors are visible in current window (most
likely just a small number for performance reasons) and all's
completion regions are matched with the real cursor."
  (if-let* ((_ multiple-cursors-mode)
            (poss (daanturo-mc/fake-cursor-pos-list))
            (_ (seq-every-p #'pos-visible-in-window-p poss)))
      (progn
        (daanturo-mc/switch-to-iedit-by-symbol poss)
        (prog1
            (apply func args)
          (run-at-time 0 nil #'iedit-switch-to-mc-mode)))
    (apply func args)))
(defun daanturo-corfu-completions-for-mc--a (func &rest args)
  "Switch from `multiple-cursors-mode' to `iedit-mode' to propagate completions.
Condition: all fake cursors are visible in current window (most
likely just a small number for performance reasons) and all's
completion regions are matched with the real cursor."
  (if-let* ((_ multiple-cursors-mode)
            (poss (daanturo-mc/fake-cursor-pos-list))
            (_ (seq-every-p #'pos-visible-in-window-p poss)))
      (progn
        (cl-destructuring-bind (beg end table pred) completion-in-region--data
          (daanturo-mc/switch-to-iedit-by-region beg end poss))
        (prog1
            (apply func args)
          (run-at-time 0 nil #'iedit-switch-to-mc-mode)))
    (apply func args)))

(defun daanturo-corfu-tick-mc-compat--a (func &rest args)p
  (list (current-buffer)))
(defun daanturo-toggle-corfu-tick-advice-mc-compat--a (&rest _)
  (funcall (if multiple-cursors-mode #'daanturo-add-advice/s #'daanturo-remove-advice/s)
           #'corfu--auto-tick :around #'daanturo-corfu-tick-mc-compat--a))

;;;###autoload

(provide 'daanturo-multiple-cursors)
