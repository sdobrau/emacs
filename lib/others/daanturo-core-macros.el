;; -*- lexical-binding: t; -*-

;; NOTE For functions that are used in macros in the same file, and those macros
;; are also called in the above file, use `eval-and-compile' around its
;; definition. While loading the autoload file before is viable,
;; `native-compile-async' will complain about that.

(require 'dash)
;; (require 'cl-seq)

;;; Helpers

;;;###autoload
(defmacro daanturo-defun (name params &rest body)
  "Define function NAME and return NAME."
  (declare (indent defun) (docstring 3))
  `(progn
     (cl-defun ,name ,params ,@body)
     ',name))

;;;###autoload
(defmacro daanturo-maydefun (name params &rest body)
  "Define function NAME unless already defined.
A use case: let F be function defined in L, L is interpreted but
it's autoload file A is compiled, we can dump F into A using an
autoload cookie, so F is compiled when A is loaded but it will be
back to being interpreted when L is loaded, this prevents F's
interpreted version from replacing it's compiled version.

No need for this when L is (self-)?compiled."
  (declare (indent defun) (docstring 3))
  (cond
   ((fboundp name)
    ;; (when (not noninteractive) (message "In %s: `%s' is already defined" load-file-name ',name))
    `',name)
   (t
    `(progn
       (cl-defun ,name ,params ,@body)
       ',name))))

;;;###autoload
(defmacro daanturo-maydefun-set (name lambda-args &optional docstring)
  "Ensure a function whose name is dynamic NAME is defined with
LAMBDA-ARGS and DOCSTRING. NAME can be a string which will be
interned, LAMBDA-ARGS is list that will be spliced and pass to
`lambda'. Just return the interned NAME when it is already
defined.

Just like

(progn (defalias NAME (lambda ,@LAMBDA-ARGS) DOCSTRING)
        NAME)

But avoid redundant `lambda' creation."
  (declare (indent defun) (docstring 3))
  `(let ((func (let ((name* ,name))
                 (cond ((stringp name*) (intern name*))
                       (t name*)))))
     (unless (fboundp func)
       (defalias func
         (lambda ,@lambda-args)
         ,docstring))
     func))

;;;###autoload
(eval-and-compile
  (defun daanturo-unquote (exp)
    "Return unquoted EXP."
    (declare (pure t) (side-effect-free t))
    (named-let recur ((expr exp))
      (if (member (car-safe expr) '(quote function))
          (recur (cadr expr))
        expr))))

;;;###autoload
(defmacro daanturo-thread-as* (initial-form variable &rest forms)
  "Thread FORMS elements as VARIABLE, starting from INITIAL-FORM."
  (declare (debug (form symbolp body)))
  (if (null forms)
      initial-form
    (-let* ((let-varlist `((,variable ,initial-form)))
            final-return form)
      (while forms
        (setq form (pop forms))
        (setq form (if (symbolp form) `(,form ,variable) form))
        (if forms
            (push `(,variable ,form) let-varlist)
          (setq final-return form)))
      `(let* ,(nreverse let-varlist)
         ,final-return))))

;;;###autoload
(defmacro daanturo-thread-as (initial-form variable &rest forms)
  "Thread FORMS elements as VARIABLE, starting from INITIAL-FORM."
  (declare (debug (form symbolp body)))
  `(let* ((,variable ,initial-form)
          ,@(mapcar (lambda (form)
                      `(,variable ,(if (symbolp form)
                                       (list form variable)
                                     form)))
                    forms))
     ,variable))

;;;###autoload
(eval-and-compile
  (defun daanturo-normalize-bindings (bindings)
    (declare (pure t) (side-effect-free t))
    (cond
     ((vectorp bindings)
      (-partition 2 (append bindings '())))
     (t
      bindings))))

;;;###autoload
(defmacro daanturo-for (seq-exprs body-expr)
  "List comprehension. Takes a vector of one or more SEQ-EXPRS
 pairs, each followed by zero or more modifiers, and yields a
 sequence of evaluations of BODY-EXPR. Collections are iterated
 in a nested fashion, rightmost fastest, and nested coll-exprs
 can refer to bindings created in prior binding-forms. Supported
 modifiers are: :let [binding-form expr ...],
 :while test, :when test."
  (declare (indent defun) (debug t))
  (let ((all-binds (daanturo-normalize-bindings seq-exprs)))
    `(daanturo--for ,all-binds ,body-expr)))
(defmacro daanturo--for (bindings body-expr)
  (let* ((first-expr (car bindings))
         (next-modifiers-exprs (-take-while (lambda (%) (keywordp (car %)))
                                            (cdr bindings)))
         (rest-exprs (-drop (length next-modifiers-exprs) (cdr bindings))))
    (cl-destructuring-bind (daanturo--for-element daanturo--for-sequence) first-expr
      (let ((processed-modifiers (-mapcat #'daanturo-for--process-modifier
                                          next-modifiers-exprs)))
        (append `(cl-loop for ,daanturo--for-element being the elements of ,daanturo--for-sequence
                          ,@processed-modifiers)
                ;; "flatten" non-bottom iterated levels
                (if (seq-empty-p rest-exprs)
                    `(collect ,body-expr)
                  `(append (daanturo--for ,rest-exprs ,body-expr))))))))
(eval-and-compile
  (defun daanturo-for--process-modifier (pair)
    (cl-destructuring-bind (kw expr/s) pair
      (pcase kw
        (:let (cl-loop for (var val) in (daanturo-normalize-bindings expr/s)
                       append `(for ,var = ,val)))
        (:when `(when ,expr/s))
        (:while `(while ,expr/s))))))

;;;###autoload
(defmacro daanturo-loop (bindings &rest body)
  "Evaluates BODY in a lexical context in which the symbols in the
BINDING-forms are bound to their respective initial bindings or
parts therein. Acts as a `recur' target.
BINDINGS is either:
[var0 val0
 var1 val1 ...]
or
((var0 val0)
 (var1 val1) ...).
VAR can be destructured like `-let*'."
  (declare (indent defun) (debug t))
  (let* ((bindings* (daanturo-normalize-bindings bindings))
         (match-forms (-map #'car bindings*))
         (sources (-map #'cadr bindings*))
         (intermediate-vars (cl-loop for i below (length bindings*)
                                     collect (intern (format "daanturo-loop--source-%d" i)))))
    `(named-let recur ,(-zip-lists intermediate-vars sources)
       (-let* ,(-zip-lists match-forms intermediate-vars)
         ,@body))))

;;;###autoload
(eval-and-compile
  (defun daanturo-symbol-to-number (sym &optional beg-pos)
    (string-to-number (substring (symbol-name sym)
                                 beg-pos))))

;;;###autoload
(defmacro daanturo-fn% (&rest body)
  "Return an interactive variadic lambda.
With %[1..] %& as arguments."
  ;; Find the maximum num of `%num'
  (let* ((max-arg-index (cl-loop
                         for sym in (cons '%0 (flatten-tree body))
                         when (and (symbolp sym)
                                   (string-match-p "^%[0-9]+$" (symbol-name sym)))
                         maximize (daanturo-symbol-to-number sym 1))))
    `(lambda (,@(cl-loop for i in (number-sequence 1 max-arg-index) collect (intern (format "%%%s" i)))
         &rest %&)
       (interactive)
       ,@body)))

;;;###autoload
(defmacro daanturo-fn1 (destr-form &rest body)
  "Return an anonymous function whose arity is one (1).
DESTR-FORM is bounded to the result of it's argument and execute BODY."
  (declare (indent defun) (debug t))
  `(lambda (daanturo-fn1--arg)
     (cl-destructuring-bind ,destr-form daanturo-fn1--arg
       ,@body)))

;;;###autoload
(defmacro daanturo-safe (&rest forms)
  "Execute FORMS like `progn''s body when each of their first element is a function or a macro."
  (let ((body (cl-loop for form in forms
                       for first = (car-safe form)
                       when (or (functionp first)
                                (macrop first))
                       collect form)))
    `(progn ,@body)))

;;;###autoload
(defmacro daanturo-set (place placeholder value)
  "Set PLACE to VALUE with PLACEHOLDER as PLACE's old value.
(let ((xs (list '(1) 2 3))) (daanturo-set (car xs) it (append '(0) it)) xs)
=> '((0 1) 2 3)"
  `(let ((,placeholder ,place))
     (setf ,place ,value)))

;;;###autoload
(defmacro daanturo-inplace-delete! (place &rest elements)
  "In PLACE, inplace delete each element which is `equal' to one of ELEMENTS."
  `(let ((res (-difference ,place (list ,@elements))))
     (setf ,place res)
     res))

;;;###autoload
(defmacro daanturo-symbols->plist (&rest symbols)
  "Return (list :sym sym ...), for each sym in SYMBOLS."
  `(list ,@(-mapcat (lambda (sym)
                      (list (intern (format ":%s" sym))
                            sym))
                    symbols)))

;;;###autoload
(defmacro daanturo-fn--> (&rest forms)
  (declare (debug t))
  `(lambda (daanturo-fn-->arg)
     (--> daanturo-fn-->arg
          ,@forms)))

;;;###autoload
(defmacro daanturo-eval-at-position (pos &rest body)
  "Evaluate BODY at POS, saving current `point'.
For each form of BODY: when it's a function, `funcall' it
instead."
  (declare (indent defun) (debug t))
  `(save-excursion
     (goto-char ,pos)
     ,@(cl-loop for form in body
                collect (if (functionp (daanturo-unquote form))
                            (list 'funcall form)
                          form))))

;;;###autoload
(defmacro daanturo-save-in-local-variable (var form)
  (declare (indent defun) (debug t))
  `(progn
     (unless (local-variable-p ',var)
       (setq-local ,var ,form))
     ,var))

;;;###autoload
(defmacro daanturo-cond-let (&rest clauses)
  "Try each clause until one succeeds.

Each clause looks like (SPECS BODY...).

Each SPECS looks like ((VAR VAL)...) or [VAR VAL...]. When all
VALs evaluate to true, bind them to their corresponding VARs and
then the expressions in BODY are evaluated and the last one's
value is the value of the cond-form; otherwise evaluate the next
clause.

When no SPECS is satisfied, return nil.

Note: binding is done according to `-let*'. VALs are evaluated
sequentially, and evaluation stops after the first `nil' VAL is
encountered."
  (declare (debug t))
  (let ((reversed-clauses (nreverse clauses)))
    (-reduce-from
     (-lambda (accu (spec . body))
       `(-if-let* ,(daanturo-normalize-bindings spec)
            (progn ,@body)
          ,accu))
     (-let (((spec . body) (car reversed-clauses)))
       `(-when-let* ,(daanturo-normalize-bindings spec)
          ,@body))
     (cdr reversed-clauses))))

;;;###autoload
(defmacro daanturo-record-return-values-of (func &rest body)
  "While evaluating BODY, return return values of FUNC as a list."
  (declare (indent defun) (debug t))
  `(let* ((retvals '())
          (adv (lambda (fn &rest args)
                 (let ((retval (apply fn args)))
                   (push retval retvals)
                   retval))))
     (advice-add ,func :around adv)
     (progn (progn ,@body)
            (advice-remove ,func adv)
            (nreverse retvals))))

;;; Configurers

;;;###autoload
(defmacro daanturo-add-init (sth fn-id arg-list &rest body)
  "Initialize STH lazily.
When STH is a hook, add to STH a function whose name contains
FN-ID, with ARGLIST and BODY, and remove itself from STH before
running.
When STH is a function, behave like `advice-add', with
WHERE as the first element of BODY.
Else evaluate BODY after loading STH."
  (declare (indent defun))
  ;; Reason for not generating form inside `cond': STH & FN-ID may be unable to
  ;; be evaluated
  `(let ((func (daanturo-concat-symbols '@ ,sth ,fn-id)))
     (cond ((string-match-p "\\`hook-" (format "%s" ,sth))
            (let ((hook (intern (string-remove-prefix "hook-" (format "%s" ,sth)))))
              (fset func
                    (lambda ,arg-list
                      (remove-hook hook func)
                      ,@body))
              (add-hook hook func)))
           ((string-match-p "\\`after-" (format "%s" ,sth))
            (with-eval-after-load
                (intern
                 (string-remove-prefix (symbol-name 'after-) (format "%s" ,sth)))
              ,@body)))))

;;;###autoload
(defmacro daanturo-when-graphical (&rest body)
  "Evaluate BODY once when running in a graphical environment."
  `(letrec ((func (lambda ()
                    (when (display-graphic-p)
                      ,@body
                      (remove-hook 'server-after-make-frame-hook func)))))
     (add-hook 'server-after-make-frame-hook func)
     (funcall func)))

(defmacro daanturo-with-advice (symbol where function &rest body)
  "Run BODY with temporary (advice-add SYMBOL WHERE FUNCTION).
When FUNCTION is already added to SYMBOL prior to execution, this
won't unexpectedly remove FUNCTION from SYMBOL after that, it
will only remove what would have been added by itself.

\(daanturo-with-advice #'print :override #'ignore
  (print 0)
  (daanturo-with-advice #'print :override #'ignore)
  (print 1))

The above expression won't print \"1\"."
  (declare (indent defun) (debug t))
  `(let* ((advice-cookie-name (gensym "daanturo-with-advice")))
     (advice-add ,symbol ,where ,function
                 (list (cons 'name advice-cookie-name)))
     (prog1
         (progn ,@body)
       (advice-remove ,symbol advice-cookie-name))))

;;;###autoload
(defmacro daanturo-with-demoted-errors (&rest body)
  (declare (debug t))
  `(condition-case daanturo-with-demoted-errors--err
       (progn ,@body)
     (error
      (message "Demoted error: %S %s"
               daanturo-with-demoted-errors--err
               (if debug-on-error
                   (format "; from %s"
                           (--> ',body
                                (format "%s" it)
                                (daanturo-remove-str-fixes "(" ")" it)
                                (replace-regexp-in-string "\n" " " it)))
                 "")))))

;;;###autoload
(defmacro daanturo-with-silently-demoted-errors (&rest body)
  (declare (debug t))
  `(condition-case daanturo-with-demoted-errors--err
       (progn ,@body)
     (error)))

;;;###autoload
(defmacro daanturo-eval-until-no-error (&rest forms)
  "Evaluate each of FORMS until no error is signaled and return
that last evaluated value."
  (cl-reduce (lambda (next-forms prev-form)
               `(condition-case _
                    ,prev-form
                  (t ,next-forms)))
             (nreverse forms)))

;;;###autoload
(defmacro daanturo-condition-case* (var handler &rest bodyform)
  (declare (indent defun) (debug t))
  `(condition-case ,var
       (progn ,@bodyform)
     ,handler))

;;;###autoload
(defmacro daanturo-condition-case** (handler &rest bodyform)
  (declare (indent defun) (debug t))
  `(condition-case _
       (progn ,@bodyform)
     (error ,handler)))

;;;###autoload
(defmacro daanturo-with-deferred-gc (&rest body)
  "Defer garbage collection while executing BODY."
  (declare (debug t))
  `(dlet ((gc-cons-threshold most-positive-fixnum)
          (gc-cons-percentage 0.75))
     ,@body))

;;;###autoload
(defmacro daanturo-get-buffer-differences-after (&rest body)
  "After executing BODY, return visible appeared & disappeared buffers."
  `(let ((old-buffers (daanturo-get-all-buffers-in-frames)))
     ,@body
     (let ((new-buffers (daanturo-get-all-buffers-in-frames)))
       (list
        (-difference new-buffers old-buffers)
        (-difference old-buffers new-buffers)))))

;;;###autoload
(defmacro daanturo-after-each (libs &rest body)
  "Evaluate BODY like `with-eval-after-load' multiple times after each of LIBS has been loaded."
  (declare (indent defun))
  `(dolist (daanturo-after-each--lib ,libs)
     (with-eval-after-load daanturo-after-each--lib
       ,@body)))

;;;###autoload
(defmacro daanturo-after-once (libs &rest body)
  "Evaluate BODY like `with-eval-after-load' once after one of LIBS has been loaded."
  (declare (indent defun))
  `(letrec ((h (lambda (&rest _)
                 (when (-some (-cut member <> features) ,libs)
                   (remove-hook 'after-load-functions h)
                   ,@body))))
     (add-hook 'after-load-functions h)
     (funcall h)))

;;;###autoload
(defmacro daanturo-after-all (libs &rest body)
  "Evaluate BODY like `with-eval-after-load' once after all of LIBS have been loaded."
  (declare (indent defun))
  `(letrec ((h (lambda (&rest _)
                 (when (-every (-cut member <> features) ,libs)
                   (remove-hook 'after-load-functions h)
                   ,@body))))
     (add-hook 'after-load-functions h)
     (funcall h)))

;;;###autoload
(defmacro daanturo-with-mode/s (mode/s arg &rest body)
  "Execute BODY with MODE/S called with ARG.
Return BODY's evaluated value."
  (declare (indent defun) (debug t))
  `(let* ((modes (daanturo-ensure-list ,mode/s))
          (old-mode-value-alist
           (-map (lambda (mode)
                   (cons mode
                         (if (and (boundp mode) (symbol-value mode))
                             1
                           0)))
                 modes)))
     (dolist (mode modes)
       (when (fboundp mode)
         (funcall mode ,arg)))
     ;; not `prog1', restore modes even when return abnormally
     (unwind-protect
         (progn ,@body)
       (dolist (mode modes)
         (when (fboundp mode)
           (funcall mode (alist-get mode old-mode-value-alist)))))))

;;;###autoload
(defmacro daanturo-in-other-window (ensure-window &rest body)
  "Execute BODY in another window. With non-nil ENSURE-WINDOW, always splits, else depends on `split-window-sensibly'."
  `(progn
     (if ,ensure-window
         (if (> (window-pixel-height) (window-pixel-width))
             (split-window-below)
           (split-window-right))
       (split-window-sensibly))
     (other-window 1)
     ,@body))

;;;###autoload
(defmacro daanturo-without-spaces-inserted (&rest body)
  "After executing BODY, remove spaces between the previous point
and current point."
  `(let ((p (point)))
     ,@body
     (let ((str (buffer-substring-no-properties p (point))))
       (delete-region p (point))
       (insert (replace-regexp-in-string " " "" str)))))

;;;###autoload
(defmacro daanturo-with-minibuffer (&rest body)
  "Evaluate BODY in the minibuffer and return it's value.
When there isn't an active minibuffer, return nil."
  `(save-selected-window
     (if (active-minibuffer-window)
         (progn
           (select-window
            (active-minibuffer-window))
           ,@body)
       nil)))

;;;###autoload
(defmacro daanturo-quit-minibuffer-and-run (&rest body)
  (declare (debug t))
  `(progn
     (run-at-time
      nil nil
      (lambda ()
        (with-demoted-errors "Error: %S"
          ,@body)))
     (daanturo-with-minibuffer
      (abort-recursive-edit))))

;;;###autoload
(defmacro daanturo-add-first-editable-hook (&rest body)
  (declare (indent defun))
  `(letrec ((editable-hooks '(window-buffer-change-functions minibuffer-setup-hook))
            (func (lambda (&rest _)
                    (when (and after-init-time ; initialization is finished
                               (not buffer-read-only))
                      ,@body
                      (daanturo-remove-hook/s editable-hooks (list func))))))
     (daanturo-safe-add-hook/s editable-hooks (list func))
     (funcall func)))

;;;###autoload
(defmacro daanturo-hook-while (hook condition &rest body)
  "Hook BODY into HOOK while CONDITION is unsatisfied."
  (declare (indent defun) (debug t))
  `(letrec
       ((func
         (lambda (&rest _)
           (add-hook ,hook func)
           (if ,condition
               (progn ,@body)
             (remove-hook ,hook func)))))
     (funcall func)))

;;;###autoload
(defmacro daanturo-add-lazy-hook-when (hook condition &rest body)
  "Add a function into HOOK that runs BODY once when CONDITION is
satisfied, that function is returned."
  (declare (indent defun) (debug t))
  `(letrec
       ((func
         (lambda (&rest _)
           (when ,condition
             ,@body
             (remove-hook ,hook func)))))
     (add-hook ,hook func)
     func))

;;;###autoload
(defmacro daanturo-hook-once-when-then-forced-next (hook condition function)
  (declare (indent defun) (debug t))
  `(daanturo-add-hook-once ,hook
                     (lambda ()
                       (if ,condition
                           (funcall ,function)
                         (daanturo-add-hook-once ,hook ,function)))))

;;;###autoload
(defvar daanturo-accumulated-config-forms '()
  "Aggregated forms for evaluation at a later time, use when it's
not correct to evaluate those in the autoload file.")
;;;###autoload
(defmacro daanturo-accumulate-config (&rest body)
  `(add-to-list 'daanturo-accumulated-config-forms
                '(progn ,@body)
                'append))

;;;###autoload
(defmacro daanturo-add-startup-hook (&rest body)
  "Add BODY to `emacs-startup-hook'.
(info \"Summary: Sequence of Actions at Startup\")"
  (declare (indent defun) (debug t))
  `(add-hook 'emacs-startup-hook
             (lambda ()
               ,@body)))

;;;###autoload
(defmacro daanturo-with-file (file &rest body)
  (declare (indent defun) (debug t))
  `(with-current-buffer (find-file-noselect ,file)
     ,@body))

;;;###autoload
(defmacro daanturo-disable-mode-when (mode advise-name condition)
  (declare (indent defun) (debug t))
  `(progn
     (defun ,advise-name (func &optional arg)
       (if ,condition
           (when (bound-and-true-p ,mode)
             (funcall func 0))
         (funcall func arg)))
     (advice-add #',mode :around #',advise-name)))

;;; Utilities

;;;###autoload
(defmacro daanturo-with-selected-window-maybe (wd &rest body)
  (declare (indent defun) (debug t))
  `(if (equal ,wd (selected-window))
       (progn ,@body)
     (with-selected-window ,wd
       ,@body)))

;;;###autoload
(defmacro daanturo-save-line-col (&rest body)
  (declare (indent defun) (debug t))
  `(let ((daanturo-save-line-col--old-line (line-number-at-pos))
         (daanturo-save-line-col--old-column (current-column)))
     (prog1
         (progn ,@body)
       (progn
         (goto-char (point-min))
         (forward-line (1- daanturo-save-line-col--old-line))
         (move-to-column daanturo-save-line-col--old-column)))))

;;;###autoload
(defmacro daanturo-save-position-in-window (&rest body)
  `(-let* ((delta (- (line-number-at-pos)
                     (line-number-at-pos (window-start)))))
     (prog1 (progn ,@body)
       (recenter delta))))

;;;###autoload
(defmacro daanturo-y-or-n-forms (&rest forms)
  (declare (indent defun))
  `(when (y-or-n-p (--> (-map (lambda (form) (format "%s" form))
                              ',forms)
                        (string-join it " ")))
     (daanturo-lexical-eval '(progn ,@forms))))

(provide 'daanturo-core-macros)
