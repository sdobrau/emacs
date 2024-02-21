;;; from kf

;; I cannot believe what I have to do to turn off font locking in mail
;; and message buffers.  Running `(font-lock-mode -1)' from every
;; possibly relevant gnus-*, mail-*, and message-* hook still left my
;; reply buffers font-locked.  Arrrgh.
;;
;; So the code below fools font-lock-mode into thinking the buffer is
;; already fontified (so it will do nothing -- see
;; font-lock.el:font-lock-mode for details), and then makes sure that
;; the very last thing run when I hit reply to a message is to turn
;; off font-lock-mode in that buffer, from post-command-hook.  Then
;; that function removes itself from post-command-hook so it's not run
;; with every command.

(defun kf-compensate-for-fucking-unbelievable-emacs-lossage ()
  (font-lock-mode -1)
  (remove-hook
   'post-command-hook
   'kf-compensate-for-fucking-unbelievable-emacs-lossage)
  (add-hook 'font-lock-mode-hook 'kf-font-lock-mode-hook)

  (defun kf-font-lock-mode-hook ()
    (if (or (eq major-mode 'message-mode)
            (eq major-mode 'mail-mode))
        (progn
          (make-local-variable 'font-lock-fontified)
          (setq font-lock-fontified t)
          (add-hook
           'post-command-hook
           'kf-compensate-for-fucking-unbelievable-emacs-lossage)))))



(provide 'mail-extras)
