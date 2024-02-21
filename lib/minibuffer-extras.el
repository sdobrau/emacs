;;; from oantolin
;; minibuffer-extras.el --- Miscellaneous minibuffer commands    -*- lexical-binding: t; -*-
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; tools for navigating the file system in the minibuffer
;;;bind in minibuffer-local-filename-map

(defun up-directory (arg)
  "Move up a directory (delete backwards to /)."
  (interactive "p")
  (if (string-match-p "/." (minibuffer-contents))
      (zap-up-to-char (- arg) ?/)
    (delete-minibuffer-contents)))

(defun exit-with-top-completion ()
  "Exit minibuffer with top completion candidate."
  (interactive)
  (let ((content (minibuffer-contents-no-properties)))
    (unless (let (completion-ignore-case)
              (test-completion content
                               minibuffer-completion-table
                               minibuffer-completion-predicate))
      (when-let ((completions (completion-all-sorted-completions)))
        (delete-minibuffer-contents)
        (insert
         (concat
          (substring content 0 (or (cdr (last completions)) 0))
          (car completions)))))
    (exit-minibuffer)))

;; daanturo

;;;###autoload
(defun daanturo-find-file-insert-/-after-~-h ()
  (when (and (= (point) (point-max))
             (equal "~"
                    (buffer-substring-no-properties
                     (if (and (bound-and-true-p rfn-eshadow-overlay)
                              (overlay-buffer rfn-eshadow-overlay))
                         (overlay-end rfn-eshadow-overlay)
                       (minibuffer-prompt-end))
                     (point-max))))
    (insert "/")))



(provide 'minibuffer-extras)
