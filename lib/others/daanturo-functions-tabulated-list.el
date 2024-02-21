;; -*- lexical-binding: t; -*-

(defvar-local daanturo-tabulated-list-old-widths nil)
;;;###autoload
(defvar-local daanturo-tabulated-list-max-widths nil)
(defun daanturo-tabulated-list-fit-width-all-columns ()
  (interactive)
  (daanturo-with-deferred-gc
   (unless daanturo-tabulated-list-max-widths
     (setq-local daanturo-tabulated-list-max-widths
                 (daanturo-tabulated-list-get-maximum-entry-lengths)))
   (setq-local daanturo-tabulated-list-old-widths
               (seq-map (-lambda ((name width sort . props))
                          width)
                        tabulated-list-format))
   (setq-local tabulated-list-format
               (--> (seq-map-indexed
                     (-lambda ((name width sort . props) idx)
                       `(,name ,(nth idx daanturo-tabulated-list-max-widths)
                               ,sort . ,props))
                     tabulated-list-format)
                    (seq-into it 'vector)))
   (tabulated-list-init-header)
   (daanturo-save-line-col
     (tabulated-list-print t))))
(defun daanturo-tabulated-list-get-maximum-entry-lengths ()
  (cl-loop
   for idx below (length tabulated-list-format)
   collect (cl-loop
            for entry in tabulated-list-entries
            maximize
            (--> entry cadr (aref it idx)
                 (if (stringp it)
                     it
                   (car it))
                 length))))

;;;###autoload
(defun daanturo-tabulated-list-restore-from-fit-width-all-columns ()
  (interactive)
  (setq-local tabulated-list-format
              (--> (seq-map-indexed
                    (-lambda ((name width sort . props) idx)
                      `(,name
                        ,(min width
                              (nth idx daanturo-tabulated-list-old-widths))
                        ,sort
                        . ,props))
                    tabulated-list-format)
                   (seq-into it 'vector)))
  (tabulated-list-init-header)
  (daanturo-save-line-col
    (tabulated-list-print t)))

;;;###autoload
(defun daanturo-tabulated-list-auto-columns ()
  "Assume that `tabulated-list-entries' has been set."
  (let ((column-num (-->  tabulated-list-entries
                          car
                          (nth 1 it)
                          length)))
    (setq-local tabulated-list-format
                `[,@(cl-loop for i below column-num
                             collect (list (format "Column %d" i) 48 nil))])))

(provide 'daanturo-functions-tabulated-list)
