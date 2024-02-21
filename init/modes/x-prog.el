(leaf prog-mode
  :require t
  :hook (((prog-mode-hook org-mode-hook) . prettify-symbols-mode)
	 (lisp-mode-hook . prettify-symbols-lisp)
	 (emacs-lisp-mode-hook . prettify-symbols-lisp)
	 (org-mode-hook . prettify-symbols-org)
	 (python-mode-hook . prettify-symbols-python)
	 (c-mode-hook . prettify-symbols-c)
	 (c++-mode-hook . prettify-symbols-c++)
	 ((js-mode-hook js2-mode-hook) . prettify-symbols-js)
	 (prog-mode-hook . (lambda ()
			     (setq-local scroll-margin 3))))
  :bind (:prog-mode-map
         ("C-M-S-<right>" . org-increase-number-at-point)
         ("C-M-S-<left>" . org-decrease-number-at-point))

  :preface
  ;; prettify symbols
  (defun prettify-symbols-prog ()
    (push '("<=" . ?≤) prettify-symbols-alist)
    (push '(">=" . ?≥) prettify-symbols-alist))

  (defun prettify-symbols-lisp ()
    (push '("/=" . ?≠) prettify-symbols-alist)
    (push '("sqrt" . ?√) prettify-symbols-alist)
    (push '("not" . ?¬) prettify-symbols-alist)
    (push '("and" . ?∧) prettify-symbols-alist)
    (push '("or" . ?∨) prettify-symbols-alist))

  (defun prettify-symbols-c ()
    (push '("<=" . ?≤) prettify-symbols-alist)
    (push '(">=" . ?≥) prettify-symbols-alist)
    (push '("!=" . ?≠) prettify-symbols-alist)
    (push '("&&" . ?∧) prettify-symbols-alist)
    (push '("||" . ?∨) prettify-symbols-alist)
    (push '(">>" . ?») prettify-symbols-alist)
    (push '("<<" . ?«) prettify-symbols-alist))

  (defun prettify-symbols-c++ ()
    (push '("<=" . ?≤) prettify-symbols-alist)
    (push '(">=" . ?≥) prettify-symbols-alist)
    (push '("!=" . ?≠) prettify-symbols-alist)
    (push '("&&" . ?∧) prettify-symbols-alist)
    (push '("||" . ?∨) prettify-symbols-alist)
    (push '(">>" . ?») prettify-symbols-alist)
    (push '("<<" . ?«) prettify-symbols-alist)
    (push '("->" . ?→) prettify-symbols-alist))

  (defun prettify-symbols-python () ;; from adq
    (push '("lambda" . ?λ) prettify-symbols-alist)
    (push '("and" . ?∧) prettify-symbols-alist)
    (push '("or" . ?∨) prettify-symbols-alist)
    (push '("==" . ?≡) prettify-symbols-alist)
    (push '("!=" . ?≠) prettify-symbols-alist)
    (push '("<=" . ?≤) prettify-symbols-alist)
    (push '(">=" . ?≥) prettify-symbols-alist)
    (push '(">>" . ?≫) prettify-symbols-alist)
    (push '("<<" . ?≪) prettify-symbols-alist)
    (push '("->" . ?→) prettify-symbols-alist)
    (push '("not in" . ?∉) prettify-symbols-alist)
    (push '("in" . ?∈) prettify-symbols-alist)
    (push '("sum" . ?Σ) prettify-symbols-alist)
    (push '("all" . ?∀) prettify-symbols-alist)
    (push '("any" . ?∃) prettify-symbols-alist)
    (push '("..." . ?…) prettify-symbols-alist))

  (defun prettify-symbols-org ()
    ;; https://github.com/mihaiolteanu/.emacs.d/commit/336a4
    (setf prettify-symbols-alist '(("#+begin_src" . ">" )
				   ("#+end_src" . "- " ))))

  (defun prettify-symbols-js ()
    (push '("<=" . ?≤) prettify-symbols-alist)
    (push '(">=" . ?≥) prettify-symbols-alist)
    (push '("!=" . ?≠) prettify-symbols-alist)
    (push '("&&" . ?∧) prettify-symbols-alist)
    (push '("||" . ?∨) prettify-symbols-alist)
    (push '(">>" . ?») prettify-symbols-alist)
    (push '("<<" . ?«) prettify-symbols-alist)
    (push '("->" . ?→) prettify-symbols-alist)
    (push '("function" . ?λ) prettify-symbols-alist)
    (push '("=>" . ?⇒) prettify-symbols-alist)))

(leaf hl-todo
  :ensure t
  :hook (prog-mode-hook . hl-todo-mode))
