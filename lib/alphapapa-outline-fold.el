;;; https://gist.github.com/alphapapa/79ea7c33d03c9c975634559b1a776418

(defun ap/python-mode-outline-hook ()
  (setq outline-level 'ap/python-outline-level)

  (setq outline-regexp
	      (rx (or
	           ;; Commented outline heading
	           (group
	            (* space)	 ; 0 or more spaces
	            (one-or-more (syntax comment-start))
	            (one-or-more space)
	            ;; Heading level
	            (group (repeat 1 8 "\*"))	 ; Outline stars
	            (one-or-more space))

	           ;; Python keyword heading
	           (group
	            ;; Heading level

	            ;; TODO: Try setting this to python-indent-offset
	            ;; instead of space.  Might capture the indention levels
	            ;; better.
	            (group (* space))	; 0 or more spaces
	            bow
	            ;; Keywords
	            (or "class" "def")
	            eow)))))

(defun ap/python-outline-level ()
  ;; Based on this code found at
  ;; http://blog.zenspider.com/blog/2013/07/my-emacs-setup-ruby-and-outline.html:
  ;; (or (and (match-string 1)
  ;;	     (or (cdr (assoc (match-string 1) outline-heading-alist))
  ;;		 (- (match-end 1) (match-beginning 1))))
  ;;	(and (match-string 0)
  ;;	     (cdr (assoc (match-string 0) outline-heading-alist)))

  ;; This doesn't work properly. It sort-of works, but it's not
  ;; correct. Running this function consecutively on the same line
  ;; sometimes returns different results. And it doesn't seem to
  ;; correctly recognize top-level Python functions or classes as
  ;; top-level headings, so subheadings beneath them don't collapse
  ;; properly.

  (or
   ;; Commented outline heading
   (and (string-match (rx
		                   (* space)
		                   (one-or-more (syntax comment-start))
		                   (one-or-more space)
		                   (group (one-or-more "\*"))
		                   (one-or-more space))
		                  (match-string 0))
	      (- (match-end 0) (match-beginning 0)))

   ;; Python keyword heading, set by number of indentions
   ;; Add 8 (the highest standard outline level) to every Python keyword heading
   (+ 8 (- (match-end 0) (match-beginning 0)))))

(add-hook 'python-mode-hook 'ap/python-mode-outline-hook)

(defun ap/sh-outline-level ()
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

(defun ap/sh-mode-outline-hook ()
  (setq outline-level 'ap/sh-outline-level)
  (setq outline-regexp (rx (group (or
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

(add-hook 'sh-mode-hook 'ap/sh-mode-outline-hook)

(defun ap/el-outline-level ()
  (or
   ;; Commented outline heading
   (and (string-match (rx
		                   (* space)
		                   (group (one-or-more (syntax comment-start)))
		                   (one-or-more space))
		                  (match-string 0))
	      (- (match-end 0) (match-beginning 0) 1))

   ;; Lisp def heading
   ;; Add 8 (the highest standard outline level) to every keyword
   ;; heading
   (+ 8 (- (match-end 0) (match-beginning 0)))))

(defun ap/el-mode-outline-hook ()
  (setq outline-level 'ap/el-outline-level)
  (setq outline-regexp "\\(;;[;]\\{1,8\\} \\|\\((defun\\)\\)"))

(add-hook 'emacs-lisp-mode-hook 'ap/el-mode-outline-hook)

(defun ap/general-outline-level ()
  (or
   ;; Commented outline heading
   (and (string-match (rx
		                   (* space)
		                   (one-or-more (syntax comment-start))
		                   (one-or-more space)
		                   (group (one-or-more "\*"))
		                   (one-or-more space))
		                  (match-string 0))
	      (- (match-end 1) (match-beginning 1) 1))))

(defun ap/general-outline-mode-enable ()
  (interactive)
  (setq outline-level 'ap/general-outline-level)
  (setq outline-regexp (rx (group (* space)
                                  (one-or-more (syntax comment-start))
                                  (* space)
                                  (group (one-or-more "\*"))
                                  (* space))))
  (outline-minor-mode))

(provide 'alphapapa-outline-fold)
