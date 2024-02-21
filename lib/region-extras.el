;;; mine

(defun my-search-google-region (&optional arg)
       (interactive "r")
       (engine/search-google (point) (mark) t))

;;; from kf: invoke command on each line, filter non-unique lines

(defun do-lines (command &optional start end)
  "Invoke COMMAND on the text of each line from START to END."
  (interactive
   (let* ((key  (read-key-sequence-vector "Hit key sequence: "))
          (cmd  (lookup-key global-map key t)
                (when (numberp cmd) (error "Not a valid key sequence"))
                (unless (commandp cmd) (error "Key `%s' is not defined" (key-description key)))
                (if (use-region-p)
                    (list cmd (region-beginning) (region-end))
                  (list cmd (point-min) (point-max)))))))
  (setq start  (copy-marker start)
        end    (copy-marker end))
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (funcall command (buffer-substring (line-beginning-position) (line-end-position))
               (forward-line 1)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defun kf-uniqify (start end)
  "Filter non-unique lines out of the region from START to END.
If interactive, display the number of lines deleted."
  (interactive "r")
  (kf-ensure-ordering start end)
  (setq end (copy-marker end))
  (goto-char start)
  (let ((seen-lines (make-hash-table :test 'equal))
        (removed-count 0))
    (while (< (point) end)
      (let ((line (buffer-substring-no-properties
                   (point) (progn (end-of-line) (point)))))
        (gethash line seen-lines)
        (if (gethash line seen-lines)
            (progn
              (beginning-of-line)
              (delete-region (point) (progn (forward-line 1) (point)))
              (setq removed-count (1+ removed-count)))
          (puthash line t seen-lines)
          (forward-line 1))))
    (if (> removed-count 0)
        (message "%d lines removed." removed-count)
      (message "No lines removed."))))

;;; from adq/Soft: eval and replace, sort list

(defun adq/eval-and-replace-region (from to)
  "Replace region with its evaluated form."
  (interactive "r")
  (save-excursion
    (let ((print-quoted t))
      (pp (eval (read (delete-and-extract-region from to)))
          (current-buffer)))))

(defun adq/sort-symbol-list-region (from to)
  "Sort list in region."
  (interactive "r")
  (save-excursion
    (let ((print-quoted t))
      (pp (-sort #'string< (read (delete-and-extract-region from to)))
          (current-buffer)))))

;;; cunene: flush-blank-lines

(defun cunene/flush-blank-lines (start end)
  "Remove blank lines.
START and END mark the region."
  (interactive "r")
  (flush-lines "^\\s-*$" start end))

;;; xc: xc/reselect-last-region

(defun xc/reselect-last-region ()
  "Reselect the last region.

Taken from URL
`https://web.archive.org/web/20170118020642/http://grapevine.net.au/~striggs/elisp/emacs-homebrew.el'"
  (interactive)
  (let ((start (mark t))
        (end (point)))
    (goto-char start)
    (call-interactively 'set-mark-command)
    (goto-char end)))

;;; daanturo

(defun daanturo-sort-characters-in-region ()
  (interactive)
  (-let* ((sorted (--> (buffer-substring-no-properties (region-beginning) (region-end))
                       (seq-map #'identity it)
                       (sort it #'<)
                       (apply #'string it))))
    (replace-region-contents (region-beginning)
                             (region-end)
                             (-const sorted))))

;;;###autoload
(defun daanturo-duplicate-line-or-region-up (arg)
  (interactive "p")
  (save-excursion
    (funcall (daanturo-1st-fn #'duplicate-dwim
                        #'crux-duplicate-current-line-or-region)
             arg)))

;;;###autoload
(defun daanturo-sort-lines-by-length (reverse beg end)
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((inhibit-field-text-motion t))
        (sort-subr reverse 'forward-line 'end-of-line
                   nil nil
                   (lambda (l0 l1)
                     (< (- (cdr l0) (car l0))
                        (- (cdr l1) (car l1)))))))))

;;; redguardtoo

(defun redguardtoo-narrow-to-region-indirect-buffer-maybe (start end use-indirect-buffer)
  "Indirect buffer could multiple widen on same file."
  (if (region-active-p) (deactivate-mark))
  (if use-indirect-buffer
      (with-current-buffer (clone-indirect-buffer
                            (generate-new-buffer-name
                             (format "%s-indirect-:%s-:%s"
                                     (buffer-name)
                                     (line-number-at-pos start)
                                     (line-number-at-pos end)))
                            'display)
        (narrow-to-region start end)
        (goto-char (point-min)))
      (narrow-to-region start end)))

;; @see https://gist.github.com/mwfogleman/95cc60c87a9323876c6c
;; fixed to behave correctly in org-src buffers; taken from:
;; https://lists.gnu.org/archive/html/emacs-orgmode/2019-09/msg00094.html
(defun redguardtoo-narrow-or-widen-dwim (&optional use-indirect-buffer)
  "If the buffer is narrowed, it widens.
Otherwise, it narrows to region or Org subtree.
If USE-INDIRECT-BUFFER is t, use `indirect-buffer' to hold widen content."
  (interactive "P")
  (cond
   ((and (not use-indirect-buffer) (buffer-narrowed-p))
    (widen))

   ((and (not use-indirect-buffer)
         (eq major-mode 'org-mode)
         (fboundp 'org-src-edit-buffer-p)
         (org-src-edit-buffer-p))
    (org-edit-src-exit))

   ;; narrow to region
   ((region-active-p)
    (redguardtoo-narrow-to-region-indirect-buffer-maybe (region-beginning)
                                                        (region-end)
                                                        use-indirect-buffer))

   ;; narrow to specific org element
   ((derived-mode-p 'org-mode)
    (cond
     ((ignore-errors (org-edit-src-code)) t)
     ((ignore-errors (org-narrow-to-block) t))
     ((ignore-errors (org-narrow-to-element) t))
     (t (org-narrow-to-subtree))))

   ((derived-mode-p 'diff-mode)
    (let* (b e)
      (save-excursion
        ;; If the (point) is already beginning or end of file diff,
        ;; the `diff-beginning-of-file' and `diff-end-of-file' return nil
        (setq b (progn (diff-beginning-of-file) (point)))
        (setq e (progn (diff-end-of-file) (point))))
      (when (and b e (< b e))
        (redguardtoo-narrow-to-region-indirect-buffer-maybe b e use-indirect-buffer))))

   ((derived-mode-p 'prog-mode)
    (mark-defun)
    (redguardtoo-narrow-to-region-indirect-buffer-maybe (region-beginning)
                                                        (region-end)
                                                        use-indirect-buffer))
   (t (error "Please select a region to narrow to"))))

;;; mw

(defun mw-region-delete-empty-lines (start end)
  "Delete all empty lines in region."
  (interactive "r")
  (flush-lines "^$" start end))

;; tiny shortage of mw wrap region with examples

(defun mw-wrap-region (beg end string-beg string-end)
  (interactive "r")
  (goto-char end)
  (insert string-end)
  (goto-char beg)
  (insert string-beg))

(defun mwwr2 (string-beg string-end)
  (interactive "r")
  (mw-wrap-region (region-beginning)
                  (region-end)
                  string-beg
                  string-end))

;; (with-eval-after-load "selected"
;;   "\""  (mwwr2 "\"" "\"")))
;;   "`" (mwwr2 "`" "'")))
;;   "[" (mwwr2 "[" "]")))
;;   "(" (mwwr2 "(" ")")))
;;   "{" (mwwr2 "{" "}")))
;;   "~" (mwwr2 "~" "~")))
;;   "*" (mwwr2 "*" "*")))
;;   "_" (mwwr2 "_" "_")))
;;   "=" (mwwr2 "=" "=")))
;;   ":" (mwwr2 ":" ":")))
;;   "q" (mwwr2 "#+begin_quote\n" "\n#+end_quote" )))
;;   "e" (mwwr2 "#+begin_example\n" "\n#+end_example" )))
;;   "v" (mwwr2 "#+begin_verse\n" "\n#+end_verse" )))
;;   "V" (mwwr2 "#+begin_verbatim\n" "\n#+end_verbatim" )))
;;   "s" (mwwr2 "#+begin_src \n" "\n#+end_src" ))))

;;; jf:

(defun jf/sort-unique-lines (reverse beg end
                                     &optional adjacent keep-blanks interactive)
  "Sort lines and delete duplicates.

  By default the sort is lexigraphically ascending.  To sort as
  descending set REVERSE to non-nil.  Specify BEG and END for the
  bounds of sorting.  By default, this is the selected region.

  I've included ADJACENT, KEEP-BLANKS, and INTERACTIVE so I can
  echo the method signature of `sort-lines' and
  `delete-duplicate-lines'"
  (interactive "P\nr")
  (sort-lines reverse beg end)
  (delete-duplicate-lines beg end reverse adjacent keep-blanks interactive))



(provide 'region-extras)
