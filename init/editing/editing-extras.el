(leaf editing
  :bind ("C-x M-u" . upcase-char))

;; COMMIT: add ace-jump-zap with custom command
(leaf ace-jump-zap
  ;; :after ace-jump-mode
  :ensure t
  :require t
  :preface
  (defun my-ace-jump-zap-up-to-char-or-dwim (&optional arg)
    "Without PREFIX, call `ace-jump-zap-up-to-char-dwim`.
With PREFIX, call `ace-jump-zap-up-to-char`"
    (interactive "P")
    (if arg
        (ace-jump-zap-up-to-char)
      (ace-jump-zap-up-to-char-dwim)))

  :ensure t
  :commands my-ace-jump-zap-up-to-char-or-dwim
  :bind ("M-z" . my-ace-jump-zap-up-to-char-or-dwim))

(leaf editing-extras
  :after daanturo-core-functions
  :bind (("M-d" . kill-word-dwim)
         ;; COMMIT: add backward-kill-word-or-join-lines
         ("M-DEL" . backward-kill-word-or-join-lines) ;; M-DEL backward-kill-word
         ("C-c RET" . daanturo-open-then-new-indented-line)
         ("C-c M-h" . daanturo-mark-inner-paragraph)
         ("C-M-c -" . daanturo-recenter-region-in-window)
         ("C-M-c |" . daanturo-recenter-left-right)
         ("C-x 8 t" . daanturo-insert-and-copy-date)
         ("C-x 8 M-t" . daanturo-insert-and-copy-date-and-time)
         ("C-x M-%" . daanturo-query-replace-regexp-in-whole-buffer)))
