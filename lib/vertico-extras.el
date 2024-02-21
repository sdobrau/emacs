;;; advice for function-specific vertico-count

;;;; back-end

(defmacro my-vertico-count-for-function (func)
  (advice-add :before ))

(defun my-vertico-count-apply-vertico-count-to-func (func count)
  (advice-add (quote (quote func)) :before (lambda (&rest args) (setq vertico-count count)))
  (advice-add (quote (quote func)) :after (lambda (&rest args) (setq vertico-count vertico-count))))

(defun my-vertico-count-process-alist ()
  (mapc (lambda (x)
          (my-vertico-count-apply-vertico-count-to-func (car x) (cdr x)))
        vertico-count-function-alist))
;; TODO: lol wtf
;; TODO: if function itself calls completing read, advice that instance of
;; completing-read
;;;; front-end

(defvar vertico-count-function-alist '()
  "Alist consisting of function-to-vertico-count mappings.
Car is the function to apply the Cdr vertico-count to.
Each car, then, will have the cdr count of vertico items.")

(setq vertico-count-function-alist '((consult-line . 10)
                                     (my-persp-switch-to-buffer-maybe-popper-popup* . 3)))



;;; daanturo

;;;###autoload
(defun daanturo-vertico-inc-count||height (&optional n)
  (interactive "p")
  (dlet ((vertico-resize t))
    (setq-local vertico-count (min (+ vertico-count n) (frame-height)))
    (vertico--exhibit)))

;;;###autoload
(defun daanturo-vertico-dec-count||height (&optional n)
  (interactive "p")
  (dlet ((vertico-resize t))
    (setq-local vertico-count (max (- vertico-count n) 0))
    (vertico--exhibit)))

;;;###autoload
(defun daanturo-get-symbol-in-minibuffer-selected-window ()
  (with-selected-window (minibuffer-selected-window)
    (format "%s" (or (symbol-at-point) ""))))

;;;###autoload
(defun daanturo-insert-future-minibuffer-history-without-clearing ()
  (interactive)
  ;; (insert (nth 0 (funcall minibuffer-default-add-function)))
  (let ((str (daanturo-get-symbol-in-minibuffer-selected-window)))
    (insert str)
    (not (string-empty-p str))))

;;;###autoload
(defun daanturo-vertico-sort-files (files)
  (--> (funcall (default-value 'vertico-sort-function) files)
       (-separate
        (lambda (file-name)
          ;; demote files who match one of those criteria to the end
          ;; of candidates
          (string-match-p (daanturo-group-or-regexps
                           '(
                             ;; TRAMP protocols from root
                             "\\`[^/]*:\\'"
                             ;; dot files
                             "\\`\\."
                             ))
                          file-name))
        it)
       (append (nth 1 it) (nth 0 it))))

(provide 'vertico-extras)
