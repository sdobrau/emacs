;; -*- lexical-binding: t; -*-

;; Many `seq' functions are not autoloaded.
(require 'seq)
;; `cl-loop' is a macro who will cause problems when byte-compiling when not required
(require 'cl-macs)
(require 'gv)
;; (require 'subr-x nil 'noerror)

(require 'dash)
(require 's)
(require 'f)

(require 'daanturo-core-macros)

(defun daanturo-defalias-maybe (symbol definition &optional docstring)
  (declare (indent defun))
  (cond
   ((fboundp symbol))
   (t
    (defalias symbol definition docstring)))
  symbol)

;;; Pure

;;;###autoload
(defun daanturo-safe-substring (str &optional l r)
  (declare (pure t) (side-effect-free t))
  (let ((len (length str)))
    (let ((l (max (or l 0) 0))
          (r (min (or r len) len)))
      (if (< r l)
          ""
        (substring str l r)))))

;;;###autoload
(defun daanturo-keep (func &rest sequences)
  "Return non-nil results of FUNC applied over all SEQUENCES.
FUNC's arity must match the number of SEQUENCES."
  (seq-remove #'null (apply #'seq-mapn func sequences)))

;;;###autoload
(defun daanturo-flatten-1 (sequences)
  (declare (pure t) (side-effect-free t))
  (seq-mapcat #'identity sequences))
(seq-mapcat #'identity '((1 2)))

;;;###autoload
(defun daanturo-keep-indexed (predicate sequence)
  "Return non-nil results of PREDICATE applied on each of SEQUENCE.
PREDICATE must take 2 arguments: the element and its index."
  (seq-remove #'null (seq-map-indexed predicate sequence)))

;;;###autoload
(cl-defun daanturo-range (start|stop &optional stop (step 1))
  (declare (pure t) (side-effect-free t))
  (cond
   ((not stop)
    (number-sequence 0 (1- start|stop)))
   ;; When STOP - START is divisible by STEP, don't include STOP
   ((= 0 (% (- stop start|stop) step))
    (number-sequence start|stop (- stop step) step))
   (t
    (number-sequence start|stop stop step))))

;;;###autoload
(defun daanturo-at (seq position)
  "Return SEQ's element at POSITION cyclically."
  (declare (pure t) (side-effect-free t))
  (elt seq (mod position (length seq))))

(defun daanturo-in--index* (seq idx)
  (if (< idx 0)
      (+ idx (length seq))
    idx))
;;;###autoload
(defun daanturo-in (sequence &rest indexes)
  (declare (pure t) (side-effect-free t))
  (-reduce-from (lambda (seq idx)
                  (elt seq (daanturo-in--index* seq idx)))
                sequence
                indexes))
(gv-define-setter daanturo-in (value sequence &rest indexes)
  `(setf ,(-reduce-from (lambda (seq idx)
                          `(elt ,seq (daanturo-in--index* ,seq ,idx)))
                        sequence
                        indexes)
         ,value))

;;;###autoload
(defun daanturo-inserted-at (seq position &rest elements)
  "Return SEQ with ELEMENTS inserted at POSITION."
  (declare (pure t) (side-effect-free t))
  (append (seq-subseq seq 0 position)
          elements
          (seq-subseq seq position)
          nil ;; ensure list(s)
          ))

;;;###autoload
(defun daanturo-removed-at (seq &rest positions)
  "Return SEQ with elements at POSITIONS removed."
  (declare (pure t) (side-effect-free t))
  (let* ((l (length seq))
         (cyclic-positions (-map (-cut mod <> l) positions)))
    (->> seq
         (-map-indexed (lambda (i e)
                         (if (member i cyclic-positions)
                             nil
                           (list e))))
         -non-nil
         (-map #'car))))

;;;###autoload
(defun daanturo-plist-remove (plist key)
  "Return PLIST without KEY and its value in-destructively."
  (declare (pure t) (side-effect-free t))
  (named-let recur ((plist* plist)
                    (accu '()))
    (-let* ((init (car plist*)))
      (cond ((seq-empty-p plist*)
             (nreverse accu))
            ((equal init key)
             (recur (cddr plist*)
                    accu))
            (t
             (recur (cdr plist*)
                    (cons init accu)))))))

;;;###autoload
(defun daanturo-split-mixed-plist (lst)
  "Partition LST into 2 two groups as a cons cell: one with keys
and values and one with others."
  (declare (pure t) (side-effect-free t))
  (cons (daanturo-key-value-in-mixed-plist lst)
        (daanturo-non-key-value-in-mixed-plist lst)))

;;;###autoload
(defun daanturo-key-value-in-mixed-plist (lst)
  (declare (pure t) (side-effect-free t))
  (daanturo-loop [tail lst reversed-plist '()]
     (cond ((length= tail 0)
      (nreverse reversed-plist))
     ((keywordp (car tail))
      (recur (cddr tail)
       `(,(cadr tail) ,(car tail) . ,reversed-plist)))
     (t
      (recur (cdr tail)
       reversed-plist)))))

;;;###autoload
(defun daanturo-non-key-value-in-mixed-plist (lst)
  (declare (pure t) (side-effect-free t))
  (daanturo-loop [tail lst reversed-non-plist '()]
     (cond ((length= tail 0)
      (nreverse reversed-non-plist))
     ((keywordp (car tail))
      (recur (cddr tail)
       reversed-non-plist))
     (t
      (recur (cdr tail)
       `(,(car tail) . ,reversed-non-plist))))))

;;;###autoload
(defun daanturo-seq-differences (lst0 lst1)
  "Return 2 lists of member differences as a cons cell.
- Members of LST0 who are not in LST1.
- Members of LST1 who are not in LST0."
  (declare (pure t) (side-effect-free t))
  (cons (seq-difference lst0 lst1)
        (seq-difference lst1 lst0)))

;;;###autoload
(defun daanturo-n-below (n)
  "Return \\'(0 1 .. (1- n))."
  (declare (pure t) (side-effect-free t))
  (number-sequence 0 (1- n)))

;;;###autoload
(defun daanturo-cyclic-literal-string-substitute (origin &rest strs)
  "Replace STRS in ORIGIN in cycle.
Replace STRS[-2] with STRS[-1], [-3] with [-2], etc and [-1] with
[0] in ORIGIN."
  (declare (pure t) (side-effect-free t))
  (--> origin
       ;; Split the string using the last of STRS as the delimiter, into segments
       (split-string it (regexp-quote (car (last strs))))
       ;; Replace in each segment, each pair of of STRS
       (seq-reduce
        (lambda (segments pair)
          (-let* (((from . to) pair))
            (seq-map
             (apply-partially #'replace-regexp-in-string (regexp-quote from) to)
             segments)))
        ;; Replace from right to left to prevent overriding with duplicates
        (nreverse
         ;; Collect the pairs of replacements
         (cl-loop for (from to) on strs
                  ;; Exclude the last list ( var nil )
                  when to
                  collect (cons from to)))
        it)
       ;; Finally concatenate segments, fill each void with the first of STRS, completing a cyclic replacement
       (mapconcat #'identity it (car strs))))

;;;###autoload
(defun daanturo-repeat-list (n l)
  "Return a list as a result of N L concatenated."
  (declare (pure t) (side-effect-free t))
  (cl-loop repeat n append l))

;;;###autoload
(defun daanturo-multiply-string (num str)
  "Return the concatenation of NUM STRs."
  (declare (pure t) (side-effect-free t))
  (string-join (-repeat num str) ""))

;;;###autoload
(defun daanturo-non-empty-list? (l)
  "Return whether L is a non-empty proper list."
  (declare (pure t) (side-effect-free t))
  (and l (proper-list-p l)))
(defalias 'daanturo-non-empty-list-p 'daanturo-non-empty-list?)

;;;###autoload
(cl-defun daanturo-color-hex-to-name (hex &optional (digits 2))
  (-some
   (lambda (name)
     (if (equal hex (apply #'color-rgb-to-hex
                           (append (color-name-to-rgb name) (list digits))))
         name
       nil))
   (defined-colors)))

;;;###autoload
(cl-defun daanturo-int-to-bits (num &optional (len 0))
  (declare (pure t) (side-effect-free t))
  (if (< num 2)
      (append
       (cl-loop for i below (1- len)
                collect 0)
       (list num))
    (append (daanturo-int-to-bits (/ num 2) (1- len))
            (list (% num 2)))))

;;;###autoload
(defun daanturo-bits-to-int (bits)
  (declare (pure t) (side-effect-free t))
  (daanturo-loop ((bits bits)
            (result 0))
     (if bits
               (recur
    (cdr bits)
    (+ result
       (* (car bits) (expt 2 (length (cdr bits))))))
       result)))

;;;###autoload
(defun daanturo-decimal-part-to-bits (decimal bit-length)
  (declare (pure t) (side-effect-free t))
  (daanturo-loop ((decimal decimal)
            (bit-length bit-length)
            (result '()))
     (cond ((zerop bit-length)
      result)
     ((<= 0.5 decimal)
      (recur (* 2 (- decimal 0.5)) (1- bit-length) (append result '(1))))
     ((> 0.5 decimal)
      (recur (* 2 decimal) (1- bit-length) (append result '(0)))))))

;;;###autoload
(cl-defun daanturo-decimal-to-bits (num &optional (decimal-bit-length 52))
  (declare (pure t) (side-effect-free t))
  (let ((int-part (truncate num)))
    (cons (daanturo-int-to-bits int-part)
          (daanturo-decimal-part-to-bits (abs (- num int-part)) decimal-bit-length))))

;;;###autoload
(defun daanturo-2/two-complement-int (num)
  (declare (pure t) (side-effect-free t))
  (let ((bits (daanturo-int-to-bits (abs num))))
    (if (<= 0 num)
        bits
      (last
       (daanturo-int-to-bits
        (1+ (daanturo-bits-to-int
             (seq-map (apply-partially #'- 1) bits))))
       (length bits)))))

;;;###autoload
(defun daanturo-by-length< (s0 s1)
  (declare (pure t) (side-effect-free t))
  (< (length s0) (length s1)))

;;;###autoload
(defun daanturo-by-length> (s0 s1)
  (declare (pure t) (side-effect-free t))
  (> (length s0) (length s1)))

;;;###autoload
(defun daanturo-ensure-list (proper-list-candidate)
  "If PROPER-LIST-CANDIDATE is already a proper list, return it,
else return a list which contains it."
  (declare (pure t) (side-effect-free t))
  (if (proper-list-p proper-list-candidate)
      proper-list-candidate
    (list proper-list-candidate)))

(defun daanturo-delist (arg)
  "If ARG is a list, return its car, else return itself."
  (declare (pure t) (side-effect-free t))
  (if (listp arg) (car arg) arg))

;;;###autoload
(defun daanturo-ensure-kbd (arg)
  (declare (pure t) (side-effect-free t))
  (if (stringp arg)
      (kbd arg)
    arg))

;;;###autoload
(defun daanturo-promote-universal-prefix-arg (arg)
  (declare (pure t) (side-effect-free t))
  (cond
   ((null arg)
    '(4))
   ((numberp arg)
    (* 4 (max 1 arg)))
   (t
    (list (* 4 (car arg))))))

;;;###autoload
(defun daanturo-drop-trailing-nils (seq)
  (declare (pure t) (side-effect-free t))
  (nreverse
   (seq-drop-while #'null (reverse seq))))

;;;###autoload
(defun daanturo-factorial (num)
  "Return NUM!."
  (declare (pure t) (side-effect-free t))
  (if (and (integerp num)
           (<= 0 num))
      (daanturo-loop [res 1 num* num]
               (if (= 0 num*)
       res
     (recur (* res num*) (- num* 1))))
    (error "(daanturo-factorial %d)" num)))

;;;###autoload
(defun daanturo-variance1 (lst)
  "Calculate the variance of `LST': variance(X) = E(X^2) - (E(X))^2."
  (declare (pure t) (side-effect-free t))
  (- (daanturo-average1 (seq-map (daanturo-fn% (expt %1 2)) lst))
     (expt (daanturo-average1 lst) 2)))

;;;###autoload
(defun daanturo-average1 (lst)
  "Calculate the average of `LST'."
  (declare (pure t) (side-effect-free t))
  (/ (float (apply '+ lst)) (length lst)))

;;;###autoload
(defun daanturo-average (&rest lst)
  "Calculate the average of `LST'."
  (declare (pure t) (side-effect-free t))
  (/ (float (apply '+ lst)) (length lst)))

;;;###autoload
(defun daanturo-hook? (sym)
  "Return SYM when it's name looks like a hook."
  (and (string-match-p "-\\(hook\|functions\\)" (symbol-name sym))
       sym))
(defalias 'daanturo-hook-p 'daanturo-hook?)

;;;###autoload
(defun daanturo-list-of-functions-p (arg)
  (and (proper-list-p arg)
       (-every-p (-orfn #'functionp #'symbolp) arg)))

;;;###autoload
(defun daanturo-ensure-function-list (arg)
  (cond ((functionp arg)
         (list arg))
        ((daanturo-list-of-functions-p arg)
         arg)
        (t
         (list arg))))

;;;###autoload
(defun daanturo-regexp-matched-substrings (regexp str)
  (declare (pure t) (side-effect-free t))
  (save-match-data
    (if (string-match regexp str)
        (cl-loop for (beg end) on (match-data) by #'cddr
                 when (and beg end)
                 collect (substring str beg end))
      nil)))

;;;###autoload
(defun daanturo-str (&rest ys)
  (declare (pure t) (side-effect-free t))
  (mapconcat (lambda (it)
               (cond ((null it) "")
                     ((stringp it) it)
                     (t (format "%s" it))))
             ys))

;;;###autoload
(defun daanturo-safe-str-0-prop (str)
  (declare (pure t) (side-effect-free t))
  (if (stringp str)
      (substring-no-properties str)
    str))

;;;###autoload
(defun daanturo-cyclic||loop-list? (lst)
  (declare (pure t) (side-effect-free t))
  (daanturo-loop [slow-head lst fast-head (cdr lst)]
     (cond
      ((or (null fast-head)
     (null slow-head))
       nil)
      ((equal slow-head fast-head)
       t)
      (t
       (recur (cdr slow-head) (cddr fast-head))))))
(defalias 'daanturo-cyclic||loop-list-p 'daanturo-cyclic||loop-list?)

;;;###autoload
(defun daanturo-returned-map (map-func transform-func seq)
  "Apply a special map function MAP-FUNC that doesn't return
normally on SEQ, return TRANSFORM-FUNC's return values applied on
SEQ as it was called inside MAP-FUNC."
  (let (daanturo-mut-map--retval)
    (funcall map-func
             (lambda (&rest args)
               (push (apply transform-func args)
                     daanturo-mut-map--retval))
             seq)
    (nreverse daanturo-mut-map--retval)))

;;;###autoload
(defun daanturo-string-special-regexp|not-literal-p (str)
  (declare (pure t) (side-effect-free t))
  (not (string= str (regexp-quote str))))

;;;###autoload
(defun daanturo-valid-regexp? (str)
  (declare (pure t) (side-effect-free t))
  (condition-case _
      (or (string-match-p str "")
          t)
    (invalid-regexp nil)))
;;;###autoload
(defalias #'daanturo-valid-regexp-p #'daanturo-valid-regexp?)

;;;###autoload
(defun daanturo-ensure-valid-regexp-or-literal (str)
  (declare (pure t) (side-effect-free t))
  (if (daanturo-valid-regexp? str)
      str
    (regexp-quote str)))

;;;###autoload
(defun daanturo-split-string-by-space-and-ensure-validity (str)
  (declare (pure t) (side-effect-free t))
  (-map #'daanturo-ensure-valid-regexp-or-literal
        (split-string str " ")))

;;;###autoload
(defun daanturo-symbol-to-sentence (sym)
  (declare (pure t) (side-effect-free t))
  (s-capitalize
   (replace-regexp-in-string
    "\\(.\\)-"
    "\\1 "
    (string-remove-prefix (format "%s" 'daanturo-)
                          (format "%s" sym)))))

;;;###autoload
(defconst daanturo-golden-ratio (/ (+ 1 (sqrt 5)) 2.0))
;;;###autoload
(cl-defun daanturo-golden-ratio (&optional (power 1))
  (declare (pure t) (side-effect-free t))
  (expt (/ (+ 1 (sqrt 5)) 2.0)
        power))

;;;###autoload
(defun my**2 (power)
  (declare (pure t) (side-effect-free t))
  (expt 2 power))

;;;###autoload
(defun daanturo-step-to-0 (num)
  (declare (pure t) (side-effect-free t))
  (cond ((zerop num) 0)
        ((< 0 num) (1- num))
        ((< num 0) (1+ num))))

;;;###autoload
(cl-defun daanturo-replace-newlines (str &optional (replacer ""))
  (declare (pure t) (side-effect-free t))
  (string-replace "\n" replacer str))

;;;###autoload
(defun daanturo-length-alpha< (str0 str1)
  (declare (pure t) (side-effect-free t))
  (or (< (length str0) (length str1))
      (and (= (length str0) (length str1))
           (string< str0 str1))))

;;;###autoload
(defun daanturo-count-unique (lst)
  "Return an alist of unique elements in LST and their count
there."
  (declare (pure t) (side-effect-free t))
  (--> (-uniq lst)
       (-map (lambda (elem)
               (cons elem
                     (-count (-cut equal <> elem) lst)))
             it)
       (cl-sort it '> :key 'cdr)))

;;;###autoload
(defun daanturo-parse-lines->alist (str separator &optional trim regexp)
  "Construct an alist from lines in STR.
For each line of STR who matches REGEXP, take the substring
before SEPARATOR as key and the one after (with TRIM trimmed) as
value."
  (declare (pure t) (side-effect-free t))
  (let ((regexp (or regexp
                    ;; """(alias )?<alias-name>='<alias-definition>'"""
                    (concat "^\\([^ ]+ \\)?[^ ]+" separator))))
    (-keep (lambda (line)
             (and (string-match-p regexp line)
                  (-let (((k v) (s-split-up-to separator line 1)))
                    (cons k
                          (if trim
                              (string-trim v trim trim)
                            v)))))
           (split-string-and-unquote str "\n"))))

;;;###autoload
(defun daanturo-ensure-string-prefix-suffix (str prefix &optional suffix)
  (--> (daanturo-ensure-string-prefix prefix str)
       (daanturo-ensure-string-suffix (or suffix prefix) it)))

;;;###autoload
(defun daanturo-uppercase? (obj)
  (declare (pure t) (side-effect-free t))
  (equal obj (upcase obj)))
;;;###autoload
(defalias #'daanturo-uppercase-p #'daanturo-uppercase?)

;;;###autoload
(defun daanturo-pad-string-by-columns (width padding str &optional left)
  "Assume that PADDING's length is 1."
  (let ((final-length (* width
                         (ceiling (/ (length str)
                                     (float width))))))
    (funcall (if left #'s-pad-left #'s-pad-right)
             final-length
             padding
             str)))

;;;###autoload
(defun daanturo-ceiling-/* (x y)
  (* (ceiling (/ (float x)
                 y))
     y))

;;;###autoload
(defun daanturo-equal* (&rest args)
  (declare (pure t) (side-effect-free t))
  (daanturo-binary->seq-test #'equal args))

;;;###autoload
(defun daanturo-=* (&rest args)
  (declare (pure t) (side-effect-free t))
  (daanturo-binary->seq-test #'= args))

;;;###autoload
(defun daanturo-string-quote-spaces (string)
  (declare (pure t) (side-effect-free t))
  (string-replace " " "\\ " string))

;;;###autoload
(defun daanturo-make-vector (len init &optional copier)
  (declare (pure t) (side-effect-free t))
  (--> (cl-loop for _ below len
                collect (if copier
                            (funcall copier init)
                          init))
       (seq-into it 'vector)))

;;;###autoload
(cl-defun daanturo-make-tensor (dimension-lengths
                          init-value
                          &key (init-copier #'identity) (seq-copier #'copy-sequence))
  (declare (pure t) (side-effect-free t))
  (-let* ((last-to-1st-dims (reverse dimension-lengths))
          (last-dim (seq-first last-to-1st-dims))
          (to-1st-dims (seq-into (seq-rest last-to-1st-dims) 'list)))
    (-reduce-from (lambda (sub-tensor dim-size)
                    (daanturo-make-vector dim-size sub-tensor seq-copier))
                  (daanturo-make-vector last-dim init-value init-copier)
                  to-1st-dims)))

;;;###autoload
(defun daanturo-sequence? (arg)
  "(`sequencep' ARG) but not `consp', whose `cdr' is non-nil?"
  (declare (pure t) (side-effect-free t))
  (cond
   ((consp arg) (proper-list-p arg))
   (t (sequencep arg))))
;;;###autoload
(defalias #'daanturo-sequence-p #'daanturo-sequence?)

;;;###autoload
(defun daanturo-non-empty-sequence? (arg)
  (declare (pure t) (side-effect-free t))
  (and (daanturo-sequence? arg)
       (< 0 (length arg))))
;;;###autoload
(defalias #'daanturo-non-empty-sequence-p #'daanturo-non-empty-sequence?)

;;;###autoload
(cl-defun daanturo-tree-contain-node? (tree node &optional (compare-fn #'equal))
  (declare (pure t) (side-effect-free t))
  (cond ((funcall compare-fn tree node)
         t)
        ((sequencep tree)
         (or (daanturo-tree-contain-node? (seq-first tree) node)
             (-let* ((tail (seq-rest tree)))
               (or
                ;; a non-nil CDR
                (and node
                     (not (sequencep tail))
                     (funcall compare-fn tail node))
                (and (daanturo-non-empty-sequence? tail)
                     (daanturo-tree-contain-node? tail node))))))
        (t
         nil)))
;;;###autoload
(defalias #'daanturo-tree-contain-node-p #'daanturo-tree-contain-node?)

;;; Helpers

;;;###autoload
(defun daanturo-juxtapose-call (funcs &rest args)
  (-map (lambda (f) (apply f args))
        funcs))

;;;###autoload
(defun daanturo-shuffle (lst)
  "Return LST with orders of elements randomly shuffled."
  (daanturo-loop [shuffled '() remain (seq-into lst 'list)]
     (cond ((seq-empty-p remain)
      (nreverse shuffled))
     (t
      (-let* ((picked-position (random (length remain))))
        (recur (cons (nth picked-position remain) shuffled)
         (-remove-at picked-position remain)))))))

;;;###autoload
(defun daanturo-lexical-eval (form)
  (eval form t))

;;;###autoload
(defun daanturo-unary->variadic-call (func &rest args)
  "Call FUNC on ARGS.
FUNC is a function that can accept a list as its first argument."
  (declare (indent defun))
  (funcall func args))

;;;###autoload
(defun daanturo-binary->seq-test (pred args)
  "Return if binary PRED is non-nil for every consecutive pair of ARGS."
  (cl-every pred args (cdr args)))

;;;###autoload
(defun daanturo-defalias* (symbol definition &optional docstring)
  "Like `defalias', but return SYMBOL."
  (declare (indent defun))
  (defalias symbol definition docstring)
  symbol)

;;;###autoload
(defun daanturo-callable-p (sym)
  (or (functionp sym)
      (macrop sym)))

;;;###autoload
(defun daanturo-safe-call (sym &rest args)
  (when (functionp sym)
    (apply sym args)))

;;;###autoload
(defun daanturo->valid-symbol-name (str)
  (mapconcat (lambda (char)
               (let ((ss (char-to-string char)))
                 (if (string-match-p lisp-mode-symbol-regexp ss)
                     ss
                   "_")))
             str ""))

;;;###autoload
(defun daanturo-form-sym (form &rest args)
  "Intern a symbol whose name is in format FORM ARGS.
Beware when the name is too long, it may hide annotations in the
minibuffer."
  (intern (daanturo->valid-symbol-name (apply #'format form args))))

;;;###autoload
(defun daanturo-safe-add-hook/s (hook/s functions &optional depth local)
  (declare (indent defun))
  (daanturo-add-hook/s hook/s
     (-filter #'functionp functions) ;; `fboundp' doesn't work on lambdas
     depth local))

;;;###autoload
(defun daanturo-add-hook/s (hook/s functions &optional depth local)
  "`add-hook' multiple FUNCTIONS to a single/multiple HOOK/S.
DEPTH, LOCAL are passed to it."
  (declare (indent defun))
  (dolist (hook (ensure-list hook/s))
    (dolist (func functions)
      (add-hook hook func depth local))))

;;;###autoload
(defun daanturo-remove-hook/s (hook/s functions &optional local)
  (declare (indent defun))
  (dolist (hook (ensure-list hook/s))
    (dolist (func functions)
      (remove-hook hook func local))))

;;;###autoload
(defun daanturo-add-live-buffer-hook (hook/s functions &optional depth)
  (declare (indent defun))
  (dolist (func functions)
    (when (fboundp func)
      (let ((func-live-buf (daanturo-concat-symbols '@ 'my func 'when-live-buffer--h)))
        (defalias func-live-buf (lambda (&rest _)
                                  (when (buffer-live-p (current-buffer))
                                    (funcall func))))
        (dolist (hook (ensure-list hook/s))
          (add-hook hook func-live-buf depth))))))

;;;###autoload
(defun daanturo-add-advice/s (symbol/s where symbol|function-list &optional props)
  "Add SYMBOL|FUNCTION-LIST to SYMBOL/S.
WHERE and PROPS are passed to `advice-add'."
  (declare (indent defun))
  (let ((symbols (daanturo-ensure-list symbol/s))
        (functions (daanturo-ensure-function-list symbol|function-list)))
    (dolist (sym symbols)
      (dolist (func functions)
        (advice-add sym where func props)))))

;;;###autoload
(defun daanturo-remove-advice/s (symbol/s function/s-or-where &optional symbol|function-list)
  "Remove SYMBOL|FUNCTION-LIST from SYMBOL/S.
When SYMBOL|FUNCTION-LIST is nil, take from FUNCTION/S-OR-WHERE
instead (which is actually optional, just be there for
`daanturo-add-advice/s''s compatibility)."
  (declare (indent defun))
  (let ((symbols (daanturo-ensure-list symbol/s))
        (functions (daanturo-ensure-function-list (or symbol|function-list
                                                function/s-or-where))))
    (dolist (sym symbols)
      (dolist (func functions)
        (advice-remove sym func)))))

;;;###autoload
(defun daanturo-remove-advice (symbol function)
  "(`advice-remove' SYMBOL FUNCTION) interactively."
  (interactive
   (if-let* ((sym (daanturo-completing-read-symbol nil 'fboundp nil nil nil
                                             (thing-at-point 'symbol)))
             (adv (daanturo-completing-read-advice sym)))
       (list sym adv)
     (user-error "No advices.")))
  (when (called-interactively-p t)
    (message "%S" `(advice-remove ,symbol ,function)))
  (advice-remove symbol function))

;;;###autoload
(defun daanturo-ensure-keyword (symbol)
  (if (keywordp symbol)
      symbol
    (intern (format ":%s" symbol))))

;;;###autoload
(defun daanturo-define-keymap (&rest bindings)
  (-let* ((kmap (make-sparse-keymap)))
    (cl-loop for (k d) in (-partition 2 bindings)
             do (define-key kmap (daanturo-ensure-kbd k) d))
    kmap))

;;; Background functions

;; NOTE: need speed.
;;;###autoload
(defun daanturo-project-root (&optional dir)
  "Return DIR (or `default-directory')\'s project root path."
  (declare (side-effect-free t))
  (cond
   ;; `projectile' is better at detecting sub-projects
   ((fboundp 'projectile-project-root)
    (projectile-project-root dir))
   (t
    (when-let ((p (project-current nil dir)))
      (project-root p)))))

;;;###autoload
(defun daanturo-outline-level-at-point ()
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (and (looking-at-p outline-regexp)
           (funcall outline-level)))))

;;;###autoload
(defvar daanturo-time-ISO-format "%FT%T%z"
  "Doesn't affect `daanturo-ISO-time'.")
;;;###autoload
(defun daanturo-ISO-time (&optional time zone)
  (declare (side-effect-free t))
  (format-time-string "%FT%T%z" (or time (current-time)) zone))

;;;###autoload
(defun daanturo-seconds-of-a-day ()
  "(* 24 60 60)"
  (declare (pure t) (side-effect-free t))
  86400)

;;;###autoload
(defun daanturo-time-encode-hhmm (hhmm)
  (declare (pure t) (side-effect-free t))
  (-let* ((hhmm (string-to-number (replace-regexp-in-string "[^0-9]" "" hhmm)))
          (now (current-time))
          (decoded-now (decode-time now)))
    (encode-time (list 0 (% hhmm 100) (/ hhmm 100)
                       (decoded-time-day decoded-now)
                       (decoded-time-month decoded-now)
                       (decoded-time-year decoded-now)
                       nil nil nil))))

;;;###autoload
(defun daanturo-time-convert-hhmm-to-future (hhmm)
  "Shift HHMM to tomorrow when passed today."
  (declare (side-effect-free t))
  (-let* ((time (daanturo-time-encode-hhmm hhmm)))
    (if (time-less-p time (current-time))
        (time-add time (daanturo-seconds-of-a-day))
      time)))

;;;###autoload
(defun daanturo-run-at-future-hhmm-time (hhmm repeat function &rest args)
  (apply #'run-at-time (daanturo-time-convert-hhmm-to-future hhmm)
         repeat
         function
         args))

;;;###autoload
(defun daanturo-first-non-blank-position-in-line ()
  (declare (side-effect-free t))
  (save-excursion
    (back-to-indentation)
    (point)))

;;;###autoload
(defun daanturo-column-at-position (&optional pos)
  (declare (side-effect-free t))
  (save-excursion
    (goto-char (or pos (point)))
    (current-column)))

;;;###autoload
(defun daanturo-set-mark-from-to (other-end new-point)
  "Set the mark at OTHER-END and goto NEW-POINT.
Effectively making a region."
  (set-mark other-end)
  (goto-char new-point))

(defun daanturo-having-some-opened-files-p ()
  (declare (side-effect-free t))
  (-some #'buffer-file-name (buffer-list)))

(defun daanturo-get-emacs-version-of-elc-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (re-search-forward ";; in Emacs version \\(.*\\)$")
    (match-string 1)))

;;;###autoload
(defun daanturo-find-minimum-indentation-of-buffer ()
  (declare (side-effect-free t))
  ;; (string-match-p (rx (not blank)) (thing-at-point 'line t))
  (save-excursion
    (goto-char (point-min))
    (daanturo-loop [min-indent most-positive-fixnum]
       (let ((min-indent
        (min
         (if (looking-at-p "^$")
       min-indent
                       (string-match-p (rx (not blank))
                                       (thing-at-point 'line t)))
         min-indent)))
               (let ((line (line-number-at-pos)))
     (forward-line 1)
     (beginning-of-line)
     (if (or (eobp)
       (= line (line-number-at-pos)))
         (if (= min-indent most-positive-fixnum)
       0
                       min-indent)
       (recur min-indent)))))))

;;;###autoload
(defun daanturo-find-minimum-indentation-of-string (str)
  (declare (pure t) (side-effect-free t))
  ;; (string-match-p (rx (not blank)) (thing-at-point 'line t))
  (-let* ((lines (--> (split-string str "\n" 'omit-nulls)
                      (-filter (lambda (line)
                                 (string-match-p "^[[:space:]]*[^[:space:]]" line))
                               it))))
    (-min (-map (lambda (line)
                  (string-match-p "[^[:space:]]" line))
                lines))))

;;;###autoload
(defun daanturo-buffers-with-minor-mode (mode/s)
  "Return buffers where one on MODE/S is active."
  (declare (side-effect-free t))
  (-let* ((modes (ensure-list mode/s)))
    (-filter (lambda (buf)
               (-some (lambda (mode)
                        (buffer-local-value mode buf))
                      modes))
             (buffer-list))))

;;;###autoload
(defun daanturo-buffers-with-major-mode/s (mode/s)
  (declare (side-effect-free t))
  (let* ((modes (ensure-list mode/s)))
    (-filter (lambda (buf)
               (apply #'provided-mode-derived-p
                      (buffer-local-value 'major-mode buf)
                      modes))
             (buffer-list))))

;;;###autoload
(defun daanturo-beg-defun-position ()
  (declare (side-effect-free t))
  (save-excursion
    ;; Don't take the previous defun when at the top
    (when (and (bolp) (not (eobp)))
      (forward-char))
    (beginning-of-defun)
    (point)))

;;;###autoload
(defun daanturo-end-defun-position (&optional no-space)
  (declare (side-effect-free t))
  (save-excursion
    ;; If moving backward when `eolp', then `end-of-defun', we may end up moving
    ;; to the next defun
    (goto-char (daanturo-beg-defun-position))
    (end-of-defun)
    (when no-space (skip-chars-backward " \t\n\r"))
    (point)))

;;;###autoload
(defun daanturo-bounds-of-defun ()
  "Faster than (thing-at-point \\'defun)?."
  (declare (side-effect-free t))
  (cons (daanturo-beg-defun-position)
        (daanturo-end-defun-position)))

;;;###autoload
(defun daanturo-save-comint-input-ring-h ()
  (let ((input-var (daanturo-concat-symbols '@ 'daanturo-comint-input-history major-mode)))
    (savehist-mode)
    (add-to-list 'savehist-additional-variables input-var)
    (eval `(defvar ,input-var '() ,(format "%s's saved `comint-input-ring'." major-mode)))
    (dolist (entry (-take (- (ring-size comint-input-ring)
                             (ring-length comint-input-ring))
                          (symbol-value input-var)))
      (ring-insert-at-beginning comint-input-ring entry))
    (daanturo-add-hook/s
     ;; multiple hooks for saving at the time Emacs exits but the alive
     ;; buffer's `kill-buffer-hook' isn't executed
     '(kill-buffer-hook kill-emacs-hook savehist-save-hook)
     (list (lambda (&rest _)
             (set input-var (-union (seq-map #'substring-no-properties
                                             (ring-elements comint-input-ring))
                                    (symbol-value input-var)))))
     nil 'local)))

;;;###autoload
(defun daanturo-total-margin-widths-of-window (&optional window)
  "Return WINDOW's total width of its margins.
Using (- (window-width) (window-max-chars-per-line)) maybe more
reliable."
  (declare (side-effect-free t))
  (cl-destructuring-bind (rw . lw) (window-margins window)
    (+ (or rw 0) (or lw 0))))

;;;###autoload
(defun daanturo-score-completion-candidate (cand)
  (declare (pure t) (side-effect-free t))
  (let ((l (length cand)))
    (cond
     ;; Push candidates whose names are peculiar lower
     ((daanturo-common-character? (aref cand 0))
      l)
     (t (* 2 l)))))

;;;###autoload
(defun daanturo-common-character? (char)
  (or (<= ?a char ?z)
      (<= ?A char ?Z)
      (<= ?0 char ?9)
      (= char ?-)))
;;;###autoload
(defalias #'daanturo-common-character-p #'daanturo-common-character?)

;;;###autoload
(defun daanturo-sort-completion-candidates (cands)
  ;; `seq-sort-by' can also be used, but we don't really need immutability here
  (cl-sort cands #'< :key #'daanturo-score-completion-candidate))

;;;###autoload
(defun daanturo-1st-fn (&rest args)
  "Return the first one that is a function in ARGS."
  (declare (side-effect-free t))
  (-find #'functionp args))

;;;###autoload
(defun daanturo-get-line-string-at-position (pos &optional length-limit no-properties)
  (declare (side-effect-free t))
  (daanturo-get-line-string-at-line (line-number-at-pos pos) length-limit no-properties))

;;;###autoload
(defun daanturo-get-line-string-at-line (line &optional length-limit no-properties)
  (declare (side-effect-free t))
  (save-excursion
    (goto-char (point-min)) (forward-line (1- line))
    ;; (unless no-properties
    ;;   (font-lock-ensure (line-beginning-position) (line-end-position)))
    ;; (or (thing-at-point 'line no-properties) "")
    (--> (let ((beg (line-beginning-position)))
           (buffer-substring
            beg
            (if length-limit
                (min (line-end-position) (+ beg length-limit))
              (line-end-position))))
         (string-trim-right it))))

;;;###autoload
(defun daanturo-fbound-p (sym)
  (declare (side-effect-free t))
  (or (boundp sym) (fboundp sym)))

;;;###autoload
(defun daanturo-point-in-emptyish-line-p ()
  (declare (side-effect-free t))
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[ \t]*$")))

;;;###autoload
(defun daanturo-balanced-parens-and-brackets-p ()
  (declare (side-effect-free t))
  (save-excursion
    (condition-case _err
        (not (check-parens))
      ('user-error
       nil))))

;;;###autoload
(defun daanturo-symbol-bound-non-nil-p (sym)
  (declare (side-effect-free t))
  (and (boundp sym)
       (eval sym)))

;;;###autoload
(defun daanturo-expand-path (path &optional dir)
  (declare (side-effect-free t))
  (dlet (file-name-handler-alist)
    (expand-file-name path dir)))

;;;###autoload (unless (fboundp #'file-name-parent-directory) (autoload 'file-name-parent-directory "01-daanturo-core-functions"))
(unless (fboundp #'file-name-parent-directory)
  (defun file-name-parent-directory (path)
    (declare (side-effect-free t))
    (let* ((expanded-filename (expand-file-name path))
           (parent (file-name-directory (directory-file-name expanded-filename))))
      (cond
       ((or (null parent)
            (equal parent expanded-filename))
        nil)
       ((not (file-name-absolute-p path))
        (file-relative-name parent))
       (t
        parent)))))

;;;###autoload
(defun daanturo-apply-interactively (func &rest args)
  "Like `apply' FUNC on ARGS but mark the call as interactive."
  (apply #'funcall-interactively
         func
         `(,@(butlast args) ,@(car (last args)))))

;;;###autoload
(defun daanturo-add-list-beg! (list-var &rest elements)
  "Add new ELEMENTS to the beginning of LIST-VAR."
  (declare (indent defun))
  ;; Reverse the list to keep the order of added elements
  (dolist (element (nreverse elements))
    (add-to-list list-var element)))

;;;###autoload
(defun daanturo-add-list-end! (list-var &rest elements)
  "Add new ELEMENTS to the end of LIST-VAR."
  (declare (indent defun))
  (dolist (element elements)
    (add-to-list list-var element 'append)))

;;;###autoload
(defun daanturo-add-defaul-list (list-var elements &optional at-end)
  (declare (indent defun))
  (let ((dv (default-value list-var)))
    (set-default list-var
                 (--> (-difference elements dv)
                      (if at-end
                          (append dv it)
                        (append it dv))))))

;;;###autoload
(defun daanturo-delete-in-list! (list-var &rest elements)
  "In LIST-VAR, in-place delete each element which is `equal' to one of ELEMENTS."
  (declare (indent defun))
  (set list-var (-difference (symbol-value list-var) elements)))

;;;###autoload
(defun daanturo-delete-key-in-alist! (alist-var &rest keys)
  (set alist-var
       (-remove (-lambda ((key . val))
                  (member key keys))
                (symbol-value alist-var))))

;;;###autoload
(defun daanturo-get-command-remap-maybe (cmd &optional keymap/s)
  "Return the command which CMD is remapped to in KEYMAP/S or itself."
  (declare (side-effect-free t))
  (or (command-remapping cmd nil keymap/s) cmd))

;;;###autoload
(defun daanturo-get-command-with-same-bindings (cmd &optional original-map)
  (declare (side-effect-free t))
  (let ((keymap (pcase original-map
                  ('all nil)
                  ('nil global-map)
                  (_ original-map))))
    (key-binding (kbd (key-description (where-is-internal cmd (list keymap) t))))))

;;;###autoload
(defun daanturo-get-current-equivalence-command (cmd &optional original-map)
  (declare (side-effect-free t))
  (or (command-remapping cmd nil original-map)
      (daanturo-get-command-with-same-bindings cmd original-map)))

;;;###autoload
(cl-defun daanturo-get-all-buffers-in-frames (&optional (frames (frame-list)))
  (declare (side-effect-free t))
  (thread-last frames
               (-mapcat #'window-list)
               (-map #'window-buffer)
               (delete-dups)))

;;;###autoload
(cl-defun daanturo-log-to-buffer (obj &optional log-buf-name)
  (with-current-buffer (get-buffer-create
                        (or log-buf-name (format "*%s*" 'daanturo-log-to-buffer)))
    (goto-char (point-max))
    (insert (format "%s" obj))))

;;;###autoload
(defun daanturo-concat-symbols (separator &rest symbols)
  "Return a symbol whose name is a concatenation of SYMBOLS's (strings or symbols).
With SEPARATOR inserted between each pair."
  (intern (mapconcat (apply-partially #'format "%s") symbols (format "%s" separator))))

;;;###autoload
(defun daanturo-mode->hook (mode)
  "Convert MODE to it's corresponding hook."
  (intern (format "%s-hook" mode)))

;;;###autoload
(defun daanturo-remove-symbol-suffix (symbol suffix)
  (intern (string-remove-suffix (format "%s" suffix)
                                (format "%s" symbol))))

;;;###autoload
(defun daanturo-mode->map-symbol (mode)
  "Convert MODE to it's corresponding hook."
  (intern (format "%s-map" mode)))

;;;###autoload
(defun daanturo-describe-keymap (keymap)
  (declare (side-effect-free t))
  (mapconcat
   (daanturo-fn1 (key . def)
     (format "%s %s"
       (propertize (format "%s" key) 'face 'help-key-binding)
       (propertize (format "%s" def) 'face 'font-lock-function-name-face)))
   (daanturo-keymap-bindings keymap)
   "  "))

;;;###autoload
(cl-defun daanturo-set-face-decreasing-heights-by-level
    (min-lv max-lv fmt &optional (max-height 2.0))
  (dolist (lv (number-sequence min-lv max-lv))
    (let* ((face (intern (format fmt lv)))
           (inheritanted-from-face (face-attribute face :inherit))
           (target-height (1+ (/ (1- max-height) lv)))
           (height-to-set
            (if (string-match-p (format fmt (1- lv))
                                (symbol-name inheritanted-from-face))
                (* target-height
                   (/ 1.0 (face-attribute inheritanted-from-face :height nil t)))
              target-height)))
      ;; `custom-set-faces!' may be a macro
      (daanturo-lexical-eval
       `(custom-set-faces!
         '(,face :height ,height-to-set
     ;; `custom-set-faces!' may not preserve inheritance
     :inherit ,inheritanted-from-face))))))

;;;###autoload
(cl-defun daanturo-get-other-windows-in-frames
    (&optional (frame/s (frame-list)) (window (selected-window)))
  "Get the list of windows in FRAME/S (single or list) beside WINDOW."
  (declare (side-effect-free t))
  (let ((frames (daanturo-ensure-list frame/s)))
    (-remove (apply-partially #'equal window)
             (mapcan #'window-list frames))))

;;;###autoload
(defun daanturo-get-default-value-of-symbol (symbol)
  (daanturo-lexical-eval (car (or (get symbol 'saved-value)
                            (get symbol 'standard-value)))))

(defconst daanturo-clipboard-file-prefix "file://")

;;;###autoload
(defun daanturo-file-list-to-clipboard-string (file-list)
  (declare (side-effect-free t))
  (mapconcat (apply-partially #'concat daanturo-clipboard-file-prefix) file-list "\n"))

;;;###autoload
(defun daanturo-clipboard-string-to-file-list (str)
  (declare (side-effect-free t))
  (if (string-match-p (concat "^" daanturo-clipboard-file-prefix) str)
      (-map #'string-trim
            (split-string str
                          (concat "\\(\\n\\)?" daanturo-clipboard-file-prefix)
                          'omit-nulls))
    nil))

;;;###autoload
(defun daanturo-get-files-in-clipboard ()
  (declare (side-effect-free t))
  (-some #'daanturo-clipboard-string-to-file-list kill-ring))

;;;###autoload
(cl-defun daanturo-filter-map-symbols (&optional (filter-fn #'always) (map-fn #'identity))
  (cl-loop for sym being the symbols
           when (funcall filter-fn sym)
           collect (funcall map-fn sym)))

;;;###autoload
(defun daanturo-find-keymap-symbol (keymap)
  (declare (side-effect-free t))
  (daanturo-filter-map-symbols
   (lambda (kmap-sym)
     (let ((val (daanturo-bounded-value kmap-sym)))
       (and (keymapp val)
            (equal val keymap))))))

;;;###autoload
(defun daanturo-keymap-list ()
  (declare (side-effect-free t))
  (daanturo-filter-map-symbols
   (lambda (sym) (keymapp (daanturo-bounded-value sym)))
   #'identity))

;;;###autoload
(defun daanturo-lookup-key-in-all-maps (key)
  (declare (side-effect-free t))
  (daanturo-keep (lambda (kmap)
             (let ((cmd (lookup-key (daanturo-bounded-value kmap)
                                    (daanturo-ensure-kbd key))))
               (if (not (or (null cmd)
                            (numberp cmd)))
                   (cons kmap
                         (if (keymapp cmd)
                             (daanturo-keymap-bindings cmd)
                           cmd))
                 nil)))
           (daanturo-keymap-list)))

;;;###autoload
(cl-defun daanturo-get-alist (key alist &optional (testfn #'equal) default)
  (declare (pure t) (side-effect-free t))
  (alist-get key alist default nil testfn))

;;;###autoload
(cl-defmacro daanturo-set-alist (key alist value &optional (testfn #'equal))
  "In ALIST, set KEY's value to VALUE, compare keys by TESTFN."
  `(setf (alist-get ,key ,alist nil nil ',testfn) ,value))

;;;###autoload
(defun daanturo-bind-leader-key-in-this-buffer (&rest _)
  (local-set-key (kbd "SPC") #'doom/leader))

;;;###autoload
(cl-defun daanturo-point-at-opening-bracket-p (&optional (pos (point)))
  "Check whether POS is before an opening bracket."
  (declare (side-effect-free t))
  ;; Pos is start of the innermost parenthetical grouping contains it's right
  (eq (nth 1 (save-excursion (syntax-ppss (1+ pos))))
      pos))

;;;###autoload
(cl-defun daanturo-point-at-closing-bracket-p (&optional (pos (point)))
  "Check whether POS is before a closing bracket."
  (declare (side-effect-free t))
  ;; Pos' depth in parentheses is greater than it's right
  (> (nth 0 (save-excursion (syntax-ppss pos)))
     (nth 0 (save-excursion (syntax-ppss (1+ pos))))))

;;;###autoload
(defun daanturo-point-after-parenthetical-group-beg-p ()
  "Check whether POS is immediately after an opening bracket."
  (declare (side-effect-free t))
  (let ((beg (nth 1 (syntax-ppss))))
    (and (integerp beg)
         (= (1+ beg)
            (point)))))

;;;###autoload
(defun daanturo-point-before-parenthetical-group-end-p ()
  "Check whether POS is immediately before a closing bracket."
  (declare (side-effect-free t))
  (save-excursion
    (daanturo-point-at-closing-bracket-p (1+ (point)))))

;;;###autoload
(cl-defun daanturo-var-set-by-command
    (&optional (prefix 'daanturo-var-set-by-command) (command this-command) init-value)
  "Return a local variable whose has PREFIX as prefix, COMMAND as
suffix & INIT-VALUE as initial value."
  (daanturo-lexical-eval `(defvar-local ,(intern (format "%s@%s" prefix command)) ,init-value)))

;;;###autoload
(cl-defun daanturo-get-parent-modes (&optional (mode major-mode) result)
  "Get `MODE''s parent mode recursively up the top most mode.
Doesn't include MODE itself."
  (declare (side-effect-free t))
  (if mode
      (let ((parent-mode (get mode 'derived-mode-parent)))
        (daanturo-get-parent-modes
         parent-mode
         (append result (and parent-mode (list parent-mode)))))
    result))

;;;###autoload
(defun daanturo-parent-mode-p (parent child)
  "Is CHILD derived from PARENT (or is itself)?"
  (or (equal parent child)
      (provided-mode-derived-p child parent)))

;;;###autoload
(cl-defun daanturo-mode-regexp-list (&optional (mode 'prog-mode) single dont-load)
  "Return a list of regular expressions, each is a file name extension of a mode derived from MODE.
MODE defaults to `prog-mode'.
Warning: take a long time to process.
With non-nil SINGLE, return a single regexp with all elements ORed.
With non-nil DONT-LOAD, don't `autoload-do-load' to detect unloaded modes."
  (declare (side-effect-free t))
  (let ((l (cl-loop for con in auto-mode-alist
                    for r = (car con)
                    for mmode = (cdr con)
                    when (not dont-load)
                    do (ignore-errors (autoload-do-load (symbol-function mmode)))
                    when (commandp mmode)
                    when (provided-mode-derived-p mmode mode)
                    collect r)))
    (if single
        (string-join l "\\|")
      l)))

;;;###autoload
(defun daanturo-rgb-of-background ()
  (declare (pure t) (side-effect-free t))
  (color-name-to-rgb (face-attribute 'default :background)))

(defun daanturo-keys-of-command (command &optional include-states)
  "Return list of keybindings as strings bound to COMMAND in current context.
With non-nil INCLUDE-STATES, show \"*-state>\" keys."
  (declare (side-effect-free t))
  (daanturo-for [vect (where-is-internal command)
                :when (if (string-match-p "[a-z]+-state$" (format "%s" (elt vect 0)))
                          include-states
                        t)]
    (key-description vect)))

;;;###autoload
(defun daanturo-keymap-bindings (keymap &optional keep-sub-keymaps)
  (declare (side-effect-free t))
  (daanturo-for [(key . def) (daanturo-raw-keymap-bindings keymap keep-sub-keymaps)]
    (cons (key-description key) def)))

;;;###autoload
(defun daanturo-raw-keymap-bindings (keymap &optional keep-sub-keymaps)
  (declare (side-effect-free t))
  (-mapcat #'identity
           (daanturo-raw-keymap-bindings--unspliced keymap keep-sub-keymaps)))

(defun daanturo-raw-keymap-bindings--unspliced (keymap &optional keep-sub-keymaps)
  "-> (list (list (cons \"key\" \\'def)))"
  (declare (side-effect-free t))
  (daanturo-returned-map
   #'map-keymap
   (lambda (key def)
     (cond
      ((and (keymapp def)
            (not keep-sub-keymaps))
       (-map
        (daanturo-fn1 (key1 . def1)
    (cons (vconcat (vector key) key1)
                      def1))
        (-mapcat #'identity
                 (daanturo-raw-keymap-bindings--unspliced def keep-sub-keymaps))))
      (t
       (list
        (cons (vector key) def)))))
   (keymap-canonicalize keymap)))

;;;###autoload
(defun daanturo-move-key (map old-key new-key)
  "In `MAP', unbind `OLD-KEY' (string) and optionally bind its old non-nil command to `NEW-KEY' (string)."
  (let ((command (lookup-key map (kbd old-key))))
    (when command
      (define-key map (kbd old-key) nil)
      (define-key map (kbd new-key) command))))

;;;###autoload
(defun daanturo-compilation-comint-mode (&rest _)
  "Enable input from compilation buffer until it finishes when `major-mode' is `compilation-mode'."
  (interactive)
  (when (equal major-mode 'compilation-mode)
    (comint-mode)
    (compilation-shell-minor-mode t)
    (read-only-mode 0)
    (add-hook 'compilation-finish-functions
              (lambda (&rest _) (compilation-mode))
              nil
              'local)))

;;;###autoload
(defun daanturo-string-insert-new-before-1st-space-or-end (str0 str1)
  "If STR0 contains STR1, return STR0, else return a new string
with STR1 inserted before the first space or the end."
  (declare (pure t) (side-effect-free t))
  (if (string-match-p (regexp-quote str1) str0)
      str0
    (replace-regexp-in-string (rx (group string-start (* (not space))))
                              (concat "\\1" str1)
                              str0)))

;;;###autoload
(defun daanturo-read-file (path)
  (declare (side-effect-free t))
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

;;;###autoload
(defun daanturo-up-sexp-beg-&-end-positions ()
  "Return the upper sexp's beginning and end positions."
  (declare (side-effect-free t))
  (let ((upper-sexp-start (nth 1 (syntax-ppss))))
    (list upper-sexp-start (scan-sexps upper-sexp-start 1))))

;;;###autoload
(defun daanturo-next-sexp-beg-&-end-positions (num)
  "Return the NUM next sexps' beginning and end positions.
When NUM is negative, return previous sexp(s) instead."
  (declare (side-effect-free t))
  (list (point) (scan-sexps (point) num)))

;;;###autoload
(defun daanturo-ignore-auth-sources-a (func &rest args)
  (let (auth-sources)
    (apply func args)))

;;;###autoload
(defun daanturo-message* (form separator &rest objects)
  (apply #'message
         (string-join (-repeat (length objects) form) separator)
         objects))

;;;###autoload
(defun daanturo-message (&rest objects)
  "A variadic `message' on OBJECTS."
  (apply #'message
         (string-join (-repeat (length objects) "%s") " ")
         objects))

;;;###autoload
(defun daanturo-keep-messaging-until-next-command (&rest objects)
  (let ((timer-obj (apply #'run-with-timer 0 0.5
                          #'daanturo-message objects)))
    (daanturo-add-hook-once 'pre-command-hook
          (lambda () (cancel-timer timer-obj)))))

;;;###autoload
(defun daanturo-log-message (&rest object/s)
  "Log OBJECT/S."
  (dlet ((inhibit-message (active-minibuffer-window)))
    (apply #'daanturo-message object/s)))

(defun daanturo-lookup-key (key &optional keymap state)
  "`lookup-key' KEY (string or vector) in KEYMAP (`global-map' by
default), and optionally `evil' STATE."
  (declare (side-effect-free t))
  (let* ((keymap-to-look
          (cond ((null state)
                 (or keymap global-map))
                ((member keymap (list 'global global-map 'global-map nil))
                 (evil-state-property state :keymap t))
                (t
                 (evil-get-auxiliary-keymap keymap state t t))))
         (cmd (lookup-key keymap-to-look (daanturo-ensure-kbd key))))
    (when (or (commandp cmd)
              (keymapp cmd))
      cmd)))

;;;###autoload
(defun daanturo-get-file-ancestor-list (file &optional upmost no-self)
  "Get the ancestors of FILE as a list (including itself).
Excluding everyone who is UPMOST (default: \"/\") or its ancestor.
With non-nil NO-SELF, don't include FILE."
  (declare (side-effect-free t))
  (letrec
      ((upmost-abs-path (expand-file-name (or upmost "/")))
       (helper
        (lambda (f results)
          (let ((parent (file-name-directory
                         (directory-file-name f))))
            (cond
             ((or (string-match-p (regexp-quote (string-remove-suffix "/" parent))
                                  upmost-abs-path)
                  (equal f parent))
              (cons f results))
             (t
              (funcall helper parent (cons f results))))))))
    (funcall helper
             (if no-self
                 (file-name-directory
                  (directory-file-name file))
               file)
             nil)))

;;;###autoload
(defun daanturo-copy-or-insert (str insert-flag)
  "If INSERT-FLAG, insert STR to the kill ring, else at point."
  (if insert-flag
      (insert str)
    (progn (kill-new str)
           (daanturo-message str))))

(defun daanturo-insert-and-copy (str)
  "Insert STR at point and copy it to the kill ring."
  (unless buffer-read-only (insert str))
  (kill-new str)
  (daanturo-message str)
  str)

(defconst daanturo-external-emacs-command-format
  (concat "setsid " invocation-name " --eval %S"))

;;;###autoload
(defun daanturo-eval-in-detached-emacs (form &optional options command-format)
  "Evaluate FORM in an asynchronous Emacs instance with COMMAND-FORMAT.
OPTIONS: a string."
  (let ((command-format (or command-format daanturo-external-emacs-command-format)))
    (call-process-shell-command
     (concat (format command-format (format "%S" form))
             " " options))))

(defvar-local daanturo-cached-minibuffer-completion-category t
  "`nil' is a possible value too, so let's choose `t' as
uninitialized.")
;;;###autoload
(defun daanturo-minibuffer-completion-category (&optional refresh)
  ;; (run-hook-with-args-until-success 'marginalia-classifiers)
  (when (or (equal daanturo-cached-minibuffer-completion-category t)
            refresh)
    (setq daanturo-cached-minibuffer-completion-category
          ;; Currently there are some non-compliant commands such as `swiper'
          (condition-case _
              (daanturo-get-minibuffer-completion-category)
            (error
             (bound-and-true-p current-minibuffer-command)))))
  daanturo-cached-minibuffer-completion-category)
;;;###autoload
(defun daanturo-get-minibuffer-completion-category ()
  (completion-metadata-get (daanturo-completion-metadata) 'category))

;;;###autoload
(defun daanturo-completion-metadata (&optional account-for-point)
  (completion-metadata
   (if account-for-point
       (buffer-substring-no-properties
        (minibuffer-prompt-end)
        (max (minibuffer-prompt-end) (point)))
     (minibuffer-contents))
   minibuffer-completion-table
   minibuffer-completion-predicate))

;;;###autoload
(defun daanturo-minibuffer-candidates ()
  (daanturo-with-deferred-gc
   (when (minibufferp)
     (let ((all (completion-all-completions
                 (minibuffer-contents)
                 minibuffer-completion-table
                 minibuffer-completion-predicate
                 (max 0 (- (point) (minibuffer-prompt-end))))))
       ;; The last cdr may be a number
       (daanturo-loop [lst all retval nil]
    (cond ((or (numberp lst) (null lst))
                       (nreverse retval))
          (t (recur (cdr lst) `(,(car lst) . ,retval)))))))))

;;;###autoload
(defun daanturo-point-in-string-p (&optional position parsed-syntax)
  (declare (side-effect-free t))
  (let ((parsed-syntax (or parsed-syntax
                           (syntax-ppss (or position (point))))))
    (nth 3 parsed-syntax)))

;;;###autoload
(defun daanturo-point-in-comment-p (&optional position parsed-syntax)
  (declare (side-effect-free t))
  (let ((parsed-syntax (or parsed-syntax
                           (syntax-ppss (or position (point))))))
    (nth 4 parsed-syntax)))

;;;###autoload
(defun daanturo-point-in-string-or-comment-p (&optional position parsed-syntax)
  (declare (side-effect-free t))
  (let ((parsed-syntax (or parsed-syntax
                           (syntax-ppss (or position (point))))))
    (or (daanturo-point-in-string-p position parsed-syntax)
        (daanturo-point-in-comment-p position parsed-syntax))))

;;;###autoload
(defun daanturo-put-plist! (pl &rest prop-val-lst)
  (if (boundp pl)
      (cl-loop for (prop val) on prop-val-lst by #'cddr
               do (plist-put (eval pl) prop val))
    (set pl prop-val-lst)))

;;;###autoload
(defun daanturo-overwrite-file (what file)
  (with-temp-buffer
    (insert what)
    (write-file file)))

;;;###autoload
(defun daanturo-minimum-width-of-windows-and-notable-margins (window-lst)
  (declare (side-effect-free t))
  (cl-loop
   for window in window-lst
   minimize
   (let ((margin-width (daanturo-total-margin-widths-of-window window)))
     (if (<= daanturo-sidebar-width margin-width)
         (min (window-width window) margin-width)
       (window-width window)))))

(autoload #'notifications-notify "notifications")

;;;###autoload
(defun daanturo-notify-and-message (body &rest kvs)
  (apply #'notifications-notify :body body kvs)
  (message "%s" body))

;;;###autoload
(defun daanturo-user-invisible-frame-selected-p ()
  (declare (side-effect-free t))
  (and (eq t (framep (selected-frame)))
       (equal "initial_terminal" (terminal-name))))

;;;###autoload
(defun daanturo-find-next-position-with-same-indent-as-current-line (&optional init-point back)
  (declare (side-effect-free t))
  (save-excursion
    (when init-point (goto-char init-point))
    (let* ((ind (daanturo-first-non-blank-position-in-line))
           (colu0 (current-column))
           (line0 (line-number-at-pos))
           (ind-col (daanturo-column-at-position ind)))
      (cl-block nil
        (while (not (if back (bobp) (eobp)))
          (forward-line (if back -1 1))
          (back-to-indentation)
          (let ((line (line-number-at-pos))
                ;; columns are 0-based
                (found-point (+ (line-beginning-position) colu0)))
            (when (and (= ind-col (current-column))
                       (/= line0 (line-number-at-pos))
                       (= line (line-number-at-pos found-point)))
              (cl-return found-point))))))))

;;;###autoload
(defun daanturo-window-visible-p (window)
  (declare (side-effect-free t))
  (or (window-parameter window 'visible)
      (not (window-dedicated-p window))))

;;;###autoload
(defun daanturo-autoload-functions (feature symbol-lst &optional docstring interactive &rest other-autoload-args)
  (declare (indent defun))
  (let ((file (format "%s" feature)))
    (dolist (sym symbol-lst)
      (apply #'autoload
             sym file docstring interactive
             other-autoload-args))))

;;;###autoload
(defun daanturo-autoload-commands (feature command-list)
  (declare (indent defun))
  (let ((file (format "%s" feature)))
    (dolist (cmd command-list)
      (autoload cmd file nil t))))

;;;###autoload
(defun daanturo-compiled-function-p (func)
  (declare (side-effect-free t))
  (and (symbolp func)
       (or (and (fboundp 'subr-native-elisp-p)
                (subr-native-elisp-p (symbol-function func))
                'native)
           (and (byte-code-function-p (symbol-function func))
                'byte)
           ;; FUNC is an alias
           (let ((def0 (symbol-function func)))
             (and (symbolp def0)
                  (daanturo-compiled-function-p def0))))))

;;;###autoload
(defun daanturo-bounded-value (sym)
  (declare (side-effect-free t))
  (and (boundp sym)
       (symbol-value sym)))

;;;###autoload
(defun daanturo-beg-of-inner-paragraph-position ()
  (declare (side-effect-free t))
  (save-excursion
    ;; not (at the beginning of paragraph or empty line)?
    (unless (looking-back "\n\n\\|^$" (- (point) 2))
      ;; jump back to a paragraph separator, or the buffer's beginning
      ;; (string-start)
      (re-search-backward "\n\n\\|\\`")
      ;; jump to the beginning of text (non-newline)
      (skip-chars-forward "\n"))
    (point)))

;;;###autoload
(defun daanturo-end-of-inner-paragraph-position ()
  (declare (side-effect-free t))
  (save-excursion
    (unless (looking-at-p "\n\n\\|^$")
      (re-search-forward "\n\n\\|\\'")
      (skip-chars-backward "\n"))
    (point)))

;;;###autoload
(defun daanturo-bounds-of-paragraph ()
  (declare (side-effect-free t))
  (cons (daanturo-beg-of-inner-paragraph-position)
        (daanturo-end-of-inner-paragraph-position)))

;;;###autoload
(defun daanturo-buffer-emptyish-p (&optional buf)
  "Does BUF only contain white spaces (if any)?"
  (declare (side-effect-free t))
  (with-current-buffer (or buf (current-buffer))
    (string-blank-p
     (string-trim
      (buffer-substring-no-properties (point-min) (point-max))))))

;;;###autoload
(defun daanturo-buffer-string (buf &optional with-properties trim-last-newline)
  (declare (side-effect-free t))
  (with-current-buffer buf
    (--> (if with-properties
             (buffer-string)
           (buffer-substring-no-properties (point-min) (point-max)))
         (if trim-last-newline
             (string-trim-right it "\n")
           it))))

;;;###autoload
(defun daanturo-replace-in-buffer (replacement &optional beg end)
  "Replace buffer's string from BEG to END with REPLACEMENT."
  (let ((beg (or beg (minibuffer-prompt-end) (point-min)))
        (end (or end (point-max)))
        (p0 (point)))
    ;; (replace-region-contents beg end (-const replacement)) ; undoing later
    ;; doesn't give the correct `point'
    (delete-region beg end)
    (goto-char beg)
    (prog1
        (insert replacement)
      (cond ((< p0 beg)
             (goto-char p0))
            ((< end p0)
             (goto-char (+ p0 (- (length replacement) (- end beg)))))))))

;;;###autoload
(defun daanturo-used-active-region-bounds (&optional may-min-max)
  "(interactive \"r\") always gives non-nil values."
  (declare (side-effect-free t))
  (cond ((use-region-p) (list (region-beginning) (region-end)))
        (may-min-max (list (point-min) (point-max)))
        (t '(nil nil))))

;;;###autoload
(defun daanturo-skip-chars-position (&optional string pos lim backward)
  "From POS (default to current `point'), (`skip-chars-forward' STRING LIM).
STRING default to spaces and newlines. With non-nil BACKWARD, use
`skip-chars-backward' instead."
  (let ((string (or string " \t\n\r")))
    (save-excursion
      (when pos (goto-char pos))
      (if backward
          (skip-chars-backward string lim)
        (skip-chars-forward string lim))
      (point))))

;;;###autoload
(defun daanturo-update-hash (key value-fn table &optional default)
  "VALUE-FN receive the current value as its argument."
  (declare (indent defun))
  (let ((old-val (gethash key table default)))
    (puthash key
             (funcall value-fn old-val)
             table)))

(defvar-local daanturo-string-in-mode--cache nil)
;;;###autoload
(defun daanturo-string-in-mode (str mode &optional additional-functions new)
  "Return how STR would look in MODE.
Keep a cached buffer for re-usablity. MODE's hooks are ignore,
but ADDITIONAL-FUNCTIONS are called after initializing the
buffer. With non-nil NEW, kill the cache buffer first."
  (let ((buf (format " *%s %s %s*" #'daanturo-string-in-mode mode additional-functions)))
    (when (and new (get-buffer buf))
      (kill-buffer buf))
    (unless (get-buffer buf)
      (get-buffer-create buf 'inhibit-buffer-hooks)
      (with-current-buffer buf
        (delay-mode-hooks (funcall mode))
        (mapc #'daanturo-safe-call additional-functions)
        (setq daanturo-string-in-mode--cache (make-hash-table :test #'equal))))
    (with-current-buffer buf
      (or (gethash str daanturo-string-in-mode--cache)
          (progn (dlet ((inhibit-read-only t))
                   (erase-buffer)
                   (insert str)
                   (ignore-errors
                     (font-lock-ensure)))
                 (puthash str (buffer-string) daanturo-string-in-mode--cache))))))

;;;###autoload
(defun daanturo-string-in-lisp-mode (str)
  "STR in `lisp' mode, with some additional prettiers."
  (daanturo-string-in-mode str
                     'lisp-data-mode
                     '(rainbow-delimiters-mode highlight-quoted-mode)))

;;;###autoload
(defun daanturo-buffer-string-no-properties (&optional beg end)
  (buffer-substring-no-properties (or beg (point-min))
                                  (or end (point-max))))

;;;###autoload
(defun daanturo-echo-buffer-content (buf)
  (--> (with-current-buffer buf
         (daanturo-buffer-string-no-properties))
       (string-trim-right it)
       (message "%s" it)))

;;;###autoload
(defun daanturo-stylize-string (str &rest properties)
  (add-face-text-property 0 (length str) properties nil str)
  str)

;;;###autoload
(defun daanturo-random-string (len &optional character-pool)
  (let ((character-pool (or character-pool
                            (append (number-sequence ?a ?z)
                                    (number-sequence ?A ?Z)
                                    (number-sequence ?0 ?9)))))
    (apply #'string
           (daanturo-for [_ (-iota len)]
       (seq-random-elt character-pool)))))

;;;###autoload
(defun daanturo-temp-dir (&optional sub-dir)
  (--> (or (getenv "XDG_RUNTIME_DIR")
           (expand-file-name user-login-name temporary-file-directory))
       (if sub-dir
           (expand-file-name sub-dir it)
         it)
       (progn (make-directory it 'parents)
              it)))

;;;###autoload
(defun daanturo-make-temp-file-name (&optional prefix suffix)
  (let* ((temp-dir (daanturo-temp-dir)))
    (cl-flet ((gen () (--> (concat prefix (format "%08x" (random (my**2 32)))
                                   suffix)
                           (expand-file-name it temp-dir))))
      (daanturo-loop [file (gen)]
               (if (file-exists-p file)
       (recur (gen))
     file)))))

;;;###autoload
(defun daanturo-search-regexp-matched-position (func regexp &rest args)
  (save-excursion
    (and (apply func regexp args)
         (point))))

;;;###autoload
(defun daanturo-current-text-scale ()
  (expt text-scale-mode-step text-scale-mode-amount))

;;;###autoload
(defun daanturo-relative-buffer-substring (bw fw &optional with-properties)
  (funcall (if with-properties #'buffer-substring #'buffer-substring-no-properties)
           (max (- (point) bw) (point-min))
           (min (+ (point) fw) (point-max))))

;;;###autoload
(defun daanturo-emulate-self-insert-command (n char)
  "`self-insert-command' N CHAR"
  ;; (funcall-interactively #'self-insert-command n char)
  (setq last-command-event char)
  (setq this-command #'self-insert-command)
  (self-insert-command n char))

;;;###autoload
(cl-defun daanturo-align-affixation (lst &key pre-left-alignment
                                   (compl-right 4) (compl-col 8)
                                   (pre-right 4) (pre-col 8))
  (and
   (length> lst 0)
   (let* ((max-compl-len (-max (-map (-compose #'length #'car) lst)))
          (max-pre-len (-max (-map (-compose #'length #'cadr) lst))))
     (daanturo-for [(compl pre suf) lst]
       (list
              (-->
               (s-pad-right (daanturo-ceiling-/* max-compl-len compl-col) " " compl)
               (concat it (s-repeat compl-right " ")))
              (and pre
       (-->
        (funcall (if pre-left-alignment #'s-pad-right #'s-pad-left)
           (daanturo-ceiling-/* max-pre-len pre-col) " " pre)
        (concat it (s-repeat pre-right " "))))
              suf)))))

;;;###autoload
(defun daanturo-window-line-height-fraction ()
  (/ (float (daanturo-window-line-height-from-top))
     (window-height)))

;;;###autoload
(defun daanturo-window-line-height-from-top ()
  (- (line-number-at-pos)
     (line-number-at-pos (window-start))))

;;;###autoload
(defun daanturo-clone-buffer-contents (&optional orig-buffer)
  (let* ((orig-buffer (or orig-buffer (current-buffer)))
         (clone-buf (generate-new-buffer (buffer-name orig-buffer) t)))
    (with-current-buffer orig-buffer
      (copy-to-buffer clone-buf (point-min) (point-max))
      ;; minimize visual interruptions
      (let ((p (point))
            (header header-line-format))
        (with-current-buffer clone-buf
          (goto-char p)
          (when header
            (setq-local header-line-format "")))))
    clone-buf))

;;;###autoload
(defun daanturo-switch-to-buffer-prefer-existing-window (buffer)
  (if (get-buffer-window buffer)
      (pop-to-buffer buffer)
    (switch-to-buffer buffer)))

;;;###autoload
(defun daanturo-imenu->outline-regexp ()
  (--> (daanturo-for [(_ re . _) imenu-generic-expression]
               (string-remove-prefix "^" re))
       (daanturo-group-or-regexps it)))

;;;###autoload
(defun daanturo-enabled-mode-p (mode)
  "Check if MODE is enabled, it can either be a major or minor one."
  (or (derived-mode-p mode)
      (and (boundp mode)
           mode)))

;;;###autoload
(defun daanturo-thing-at-point-dwim (&optional with-properties)
  (declare (side-effect-free t))
  (cond
   ((daanturo-region-string-maybe (not with-properties) 'trim))
   (t
    (thing-at-point 'symbol (not with-properties)))))

;;;###autoload
(defun daanturo-region-string-maybe (&optional no-properties trim)
  (declare (side-effect-free t))
  (and (use-region-p)
       (--> (funcall (if no-properties
                         #'buffer-substring-no-properties
                       #'buffer-substring)
                     (region-beginning)
                     (region-end))
            (if trim
                (string-trim it)
              it))))

;;;###autoload
(defun daanturo-dispatch-cmds (&rest cond-body)
  "Return an extended menu to dispatch command base of COND-BODY.
COND-BODY is used for `cond', it may be better if the conditions
and commands are not tied together, but it need extra complexity
to solve the double quote that is needed before command names."
  `(menu-item
    "" nil
    :filter
    (lambda (_)
      (cond ,@cond-body))))

;;; General advices

;;;###autoload
(defun daanturo-demote-errors-from (func)
  "Demote FUNC's errors."
  (interactive (list (daanturo-completing-read-symbol "daanturo-demote-errors-from : " 'functionp)))
  (advice-add func :around #'daanturo-with-demoted-errors-a))

;;;###autoload
(defun daanturo-with-demoted-errors-a (func &rest args)
  (daanturo-with-silently-demoted-errors
   (apply func args)))

;;;###autoload
(defun daanturo-with-deferred-gc-around-a (func &rest args)
  (daanturo-with-deferred-gc
   (apply func args)))

;;;###autoload
(defun daanturo-function-with-calls-to-replaced (outer-func inner-func replacer)
  "Return OUTER-FUNC with calls to INNER-FUNC replaced by REPLACER."
  (let ((f (daanturo-concat-symbols '@ 'daanturo-function-with-calls-to-replaced
                                 outer-func inner-func replacer)))
    (defalias f
      (lambda (&rest args)
        (cl-letf (((symbol-function inner-func) replacer))
          (apply outer-func args))))
    (put f 'interactive-form (interactive-form outer-func))
    f))

;;;###autoload
(defun daanturo-make-wrapped-function (wrapper func)
  "Return a function where FUNC is called as WRAPPER's body.
Both must be named symbols."
  (let ((rf (daanturo-concat-symbols '@ 'my #'daanturo-make-wrapped-function wrapper func)))
    (defalias rf
      `(lambda (&rest args)
         (interactive)
         (,wrapper
          (apply #',func args))))
    rf))

(provide 'daanturo-core-functions)
