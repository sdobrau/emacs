;;; outshine additions [[https://www.reddit.com/r/emacs/comments/4fgv87/comment/d2ak157/?utm_source=reddit&utm_medium=web2x&context=3][Code-folding while writing bash-script : emacs]]

;;;; implementation

;; COMMIT: add autoload for anon/sh-outline-level
;;;###autoload
(defun anon/sh-outline-level ()
  (or
   ;; Commented outline heading
   (and (string-match (rx
                       (* space)
                       (one-or-more (syntax comment-start))
                       (one-or-more space)
                       (group (one-or-more "\*"))
                       (one-or-more space))
                      (match-string 0))
        (- (match-end 1) (match-beginning 1) 1))

   ;; Keyword/function heading
   ;; Add 8 (the highest standard outline level) to every keyword
   ;; heading
   (+ 8 (- (match-end 3) (match-beginning 3)))))

;;;###autoload
(defun anon/sh-mode-outline-hook ()
  (setq outline-level 'anon/sh-outline-level)
  (setq outline-regexp
        (rx (group (or
                    ;; Outline headings
                    (and (* space)
                         (one-or-more (syntax comment-start))
                         (* space)
                         (group (one-or-more "\*"))
                         (* space))

                    ;; Keywords and functions
                    (and (group (* space))
                         (or
                          ;; e.g. "function foo"
                          (and (or "function" "if" "elif" "else" "for" "while")
                               (one-or-more space))

                          ;; e.g. "foo()"
                          (and (one-or-more (or alnum "_-"))
                               (* space)
                               (syntax open-parenthesis)
                               (syntax close-parenthesis)))))))))

;; COMMIT: force flycheck on in sh-mode
;;; front

;;;###autoload
(defun sh-mode-setup ()
  ;;(anon/sh-mode-outline-hook)
  (outshine-mode)
  (shfmt-on-save-mode)
  (flycheck-bashate-setup)
  (flycheck-mode)
  (bash-ts-mode))

(provide 'sh-mode-setup)
