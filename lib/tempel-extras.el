;; add | for newline-and-indent backward-char 2
;; for yaml sequence

(defun tempel--print-element (elt)
  "Return string representation of template ELT."
  (pcase elt
    ('nil nil)
    ((pred stringp) elt)
    (`(s ,name) (symbol-name name))
    (`(,(or 'p 'P) ,_ ,name . ,noinsert)
     (and (not (car noinsert)) (symbol-name name)))
    ((or 'n 'n> '> '& '% 'o 'x) " ")
    (_ "_")))

(defun tempel--element (st region elt)
  "Add template ELT to ST given the REGION."
  (pcase elt
    ('nil)
    ('n (insert "\n"))
    ('n> (insert "\n") (indent-according-to-mode))
    ('| (insert "\n") (indent-according-to-mode) (backward-indent)) ;; HERE
    ('> (indent-according-to-mode))
    ((pred stringp) (insert elt))
    ('& (unless (or (bolp) (save-excursion (re-search-backward "^\\s-*\\=" nil t)))
          (insert "\n")))
    ('% (unless (or (eolp) (save-excursion (re-search-forward "\\=\\s-*$" nil t)))
          (insert "\n")))
    ('o (unless (or (eolp) (save-excursion (re-search-forward "\\=\\s-*$" nil t)))
          (open-line 1)))
    (`(s ,name) (tempel--field st name))
    (`(l . ,lst) (dolist (e lst) (tempel--element st region e)))
    ((or 'p `(,(or 'p 'P) . ,rest)) (apply #'tempel--placeholder st rest))
    ((or 'r 'r> `(,(or 'r 'r>) . ,rest))
     (if (not region)
         (when-let (ov (apply #'tempel--placeholder st rest))
           (unless rest
             (overlay-put ov 'tempel--quit t)))
       (goto-char (cdr region))
       (when (eq (or (car-safe elt) elt) 'r>)
         (indent-region (car region) (cdr region) nil))))
    ;; TEMPEL EXTENSION: Quit template immediately
    ('q (overlay-put (tempel--field st) 'tempel--quit t))
    (_ (if-let (ret (run-hook-with-args-until-success 'tempel-user-elements elt))
           (tempel--element st region ret)
         ;; TEMPEL EXTENSION: Evaluate forms
         (tempel--form st elt)))))

(provide 'tempel-extras)
