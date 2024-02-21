;; -*- lexical-binding: t; -*-

(require 'dash)

(require 'daanturo-core-macros)

;;;###autoload
(defun daanturo-get-minibuffer-completion-property (prop)
  (or (completion-metadata-get (daanturo-completion-metadata) prop)
      (plist-get completion-extra-properties
                 (intern (format ":%s" prop)))))

;;;###autoload
(defun daanturo-get-minibuffer-affixation-function (&optional metadata)
  (or
   (daanturo-get-minibuffer-completion-property 'affixation-function)
   ;; Create an affixation function from the annotation function
   (when-let ((annotator (daanturo-get-minibuffer-completion-property 'annotation-function)))
     (lambda (candidates)
       (-map (lambda (cand)
               (if-let ((suffix (funcall annotator cand)))
                   (list cand "" suffix)
                 cand))
             candidates)))))

;;;###autoload
(defun daanturo-get-minibuffer-annotations (&optional candidates metadata)
  (let ((candidates (or candidates (daanturo-minibuffer-candidates)))
        (affixation-function (daanturo-get-minibuffer-affixation-function)))
    ;; don't truncate
    (dlet ((marginalia-field-width most-positive-fixnum))
      (daanturo-with-advice #'marginalia--truncate :override (daanturo-fn% %1)
        (--> (sort candidates #'daanturo-length-alpha<)
             ;; ;; Double reversing may extend docstrings a bit, but make values less readable
             ;; nreverse
             ;; (marginalia--affixate metadata (marginalia--annotator (completion-metadata-get metadata 'category)) it)
             (if affixation-function
                 (funcall (daanturo-get-minibuffer-affixation-function) it)
               it)
             ;; nreverse
             )))))

;;;###autoload
(defun daanturo-pretty-symbol-value (sym)
  (and (boundp sym)
       (--> (symbol-value sym)
            (cond
             ;; dont quote those special values
             ((member it '(t nil))
              it)
             ;; sharp-quote functions
             ((and (symbolp it)
                   (fboundp it))
              (list 'function it))
             ;; ;; quote symbols or lists
             ;; ((or (symbolp it) (listp it)) (list 'quote it))
             ;; other values
             (t it))
            (format "%S" it)
            daanturo-string-in-lisp-mode
            daanturo-replace-newlines)))

;;;###autoload
(defun daanturo-export-minibuffer-annotations ()
  (interactive)
  (let ((candidates (daanturo-minibuffer-candidates)))
    (daanturo-export--minibuffer
     (format "*%s %s*"
             #'daanturo-export-minibuffer-annotations
             (minibuffer-contents-no-properties))
     (--> candidates
          daanturo-get-minibuffer-annotations
          ;; each element contains a list of three(?) elements
          (daanturo-for [annotation/s it]
            (list nil (seq-into (-map #'daanturo-replace-newlines annotation/s)
                                'vector)))))))

;;;###autoload
(defun daanturo-export-minibuffer-symbols (&optional no-docstring)
  (daanturo-with-deferred-gc
   (let* ((candidates (daanturo-minibuffer-candidates))
          (entries (daanturo-for [cand (sort candidates #'daanturo-length-alpha<)
                                 :let [sym (intern cand)]]
                     (list
                      nil
                      (vector
                       (propertize cand 'face 'marginalia-symbol)
                       (or (daanturo-pretty-symbol-value sym)
                           "")
                       (if no-docstring
                           ""
                         (propertize
                          (--> (daanturo-get-symbol-all-documentations|docstrings sym)
                               (string-join it " \n ")
                               daanturo-replace-newlines)
                          ;; 'face 'completions-annotations
                          )))))))
     (daanturo-export--minibuffer
      (format "*%s %s %s*"
              (daanturo-minibuffer-completion-category)
              #'daanturo-export-minibuffer-symbols
              (minibuffer-contents-no-properties))
      entries))))

(defun daanturo-export--minibuffer (buf-name entries)
  (let* ((buf (generate-new-buffer buf-name)))
    ;; (pop-to-buffer buf)
    (with-current-buffer buf
      (setq-local tabulated-list-entries entries)
      (daanturo-tabulated-list-auto-columns)
      (daanturo-exported-annotations-mode)
      (tabulated-list-print t)
      buf)))

;;;###autoload
(define-derived-mode daanturo-exported-annotations-mode tabulated-list-mode "Annotated Candidates"
  "`tabulated-list-format' must be set before."
  (tabulated-list-init-header))
(define-key daanturo-exported-annotations-mode-map
  [remap tabulated-list-widen-current-column] #'daanturo-tabulated-list-fit-width-all-columns)
(define-key daanturo-exported-annotations-mode-map
  [remap tabulated-list-narrow-current-column] #'daanturo-tabulated-list-restore-from-fit-width-all-columns)

;;;###autoload
(cl-defun daanturo-export-minibuffer-annotations-and-quit ()
  (interactive)
  (let ((buf (daanturo-export-minibuffer-annotations)))
    (daanturo-quit-minibuffer-and-run
     (pop-to-buffer buf))))

;;;###autoload
(cl-defun daanturo-export-minibuffer-symbols-and-quit (&optional no-docstring)
  (interactive "P")
  (let ((buf (daanturo-export-minibuffer-symbols no-docstring)))
    (daanturo-quit-minibuffer-and-run
     (pop-to-buffer buf))))

(provide 'daanturo-export-minibuffer-annotations)
