;; - [2019-08-20 Tue 11:06] wrap-region did not go well together with org-goto.

(defun mw-wrap-region (beg end string-beg string-end)
  (interactive "r")
  (goto-char end)
  (insert string-end)
  (goto-char beg)
  (insert string-beg))

(with-eval-after-load "selected"
  (define-key selected-keymap (kbd "\"") (lambda () (interactive) (mw-wrap-region (region-beginning) (region-end) "\"" "\"")))
  (define-key selected-keymap (kbd "`") (lambda () (interactive) (mw-wrap-region (region-beginning) (region-end) "`" "'")))
  (define-key selected-keymap (kbd "[") (lambda () (interactive) (mw-wrap-region (region-beginning) (region-end) "[" "]")))
  (define-key selected-keymap (kbd "(") (lambda () (interactive) (mw-wrap-region (region-beginning) (region-end) "(" ")")))
  (define-key selected-keymap (kbd "{") (lambda () (interactive) (mw-wrap-region (region-beginning) (region-end) "{" "}")))
  (define-key selected-keymap (kbd "~") (lambda () (interactive) (mw-wrap-region (region-beginning) (region-end) "~" "~")))
  (define-key selected-keymap (kbd "*") (lambda () (interactive) (mw-wrap-region (region-beginning) (region-end) "*" "*")))
  (define-key selected-keymap (kbd "_") (lambda () (interactive) (mw-wrap-region (region-beginning) (region-end) "_" "_")))
  (define-key selected-keymap (kbd "=") (lambda () (interactive) (mw-wrap-region (region-beginning) (region-end) "=" "=")))
  (define-key selected-keymap (kbd ":") (lambda () (interactive) (mw-wrap-region (region-beginning) (region-end) ":" ":")))
  (define-key selected-keymap (kbd "q") (lambda () (interactive) (mw-wrap-region (region-beginning) (region-end) "#+begin_quote\n" "\n#+end_quote" )))
  (define-key selected-keymap (kbd "e") (lambda () (interactive) (mw-wrap-region (region-beginning) (region-end) "#+begin_example\n" "\n#+end_example" )))
  (define-key selected-keymap (kbd "v") (lambda () (interactive) (mw-wrap-region (region-beginning) (region-end) "#+begin_verse\n" "\n#+end_verse" )))
  (define-key selected-keymap (kbd "V") (lambda () (interactive) (mw-wrap-region (region-beginning) (region-end) "#+begin_verbatim\n" "\n#+end_verbatim" )))
  (define-key selected-keymap (kbd "s") (lambda () (interactive) (mw-wrap-region (region-beginning) (region-end) "#+begin_src \n" "\n#+end_src" ))))
