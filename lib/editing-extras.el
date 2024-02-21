;;; mine

(defun new-line-below-and-indent ()
  "Insert a new-line just below POINT, and move the point there."
  (interactive)
  (end-of-line)
  (newline-and-indent))

;;; adam

;; https://adam.kruszewski.name/2022-05-08-backward-kill-word-or-join-lines.html

;;;###2autoload
(defun backward-kill-word-or-join-lines ()
  "Backward-kill-word that will join lines if there is no word on a current line to kill."
  (interactive)
  (let ((orig-point (point))
        (orig-column (current-column))
        (start-line (line-number-at-pos)))

    (backward-word)
    (if (> start-line (line-number-at-pos))
        (progn
          (goto-char orig-point)
          (delete-backward-char orig-column)
          (when (= orig-column 0)
            (delete-char -1)))
      (kill-region (point) orig-point))))

;;; daanturo

;;;###autoload
(defun daanturo-open-new-indented-line (&optional arg)
  (interactive "P")
  (save-excursion
    (newline-and-indent arg)))

;;;###autoload
(defun daanturo-open-then-new-indented-line ()
  (interactive)
  (save-excursion (open-line 1))
  (newline-and-indent))

;;;###autoload
(defun daanturo-mark-inner-paragraph ()
  "Mark current paragraph without the first empty line."
  (interactive)
  (if (use-region-p)
      (user-error "Active region!")
    (progn
      (mark-paragraph)
      (skip-chars-forward "\n"))))

;;;###autoload
(defun daanturo-recenter-region-in-window ()
  (interactive)
  (recenter-top-bottom
   (max 0
        (floor (/ (- (window-height)
                     (if (use-region-p)
                         (count-lines (region-beginning)
                                      (region-end))
                       0))
                  2)))))

;;;###autoload
(defun daanturo-recenter-left-right ()
  "Move current buffer column to the specified window column."
  (interactive)
  (set-window-hscroll (selected-window)
                      (- (current-column) (/ (window-width) 2))))


;;;###autoload
(defun daanturo-insert-and-copy-date ()
  "Insert current date in ISO format."
  (interactive)
  (daanturo-insert-and-copy (format-time-string "%F" (current-time))))

;;;###autoload
(defun daanturo-insert-and-copy-date-and-time ()
  "Insert current date & time."
  (interactive)
  (daanturo-insert-and-copy (format-time-string "%F %H:%M:%S" (current-time))))

;;;###autoload
(defun daanturo-query-replace-regexp-in-whole-buffer ()
  (interactive)
  (save-excursion
    (unless (use-region-p)
      (daanturo-add-advice-once #'query-replace-read-args :after
        (daanturo-fn% (goto-char (point-min)))))
    ;; `anzu' isn't compatible OOTB
    (call-interactively #'query-replace-regexp)))

;;; from kf: surround str with char

(defun kf-surround-with (char &optional parg)
  "Insert two of the same CHAR around a string near point.  The string
is delimited by whitespace, although the function will do the right
thing at beginning or end of line/buffer.  Also does the right thing
if the char is one of a matching pair.

Certain chars stand for more complex markup in certain modes: for
example, 's' does HTML <strong></strong> tags, and 'e' does emphasis
tags for various markup languages.  The markup syntax is determined
using kf-markup-flavor; note that XML is interpreted to mean the
DocBook Lite DTD.

The prefix arg has various meanings.  Usually it means don't do
complex markup, but in a few cases, where non-complex markup would
virtually never be useful and there are two useful forms of complex
markup, it signals which of the two forms to generate."
  (interactive "*cSurround with char: \nP")
  ;; hmm, ought to be able to do this with syntax tables?
  (let
      ((begthing char)
       (endthing char)
       (handle-ac nil) ; special case for LaTeX \ac{...}
       (markup-flavor (kf-markup-flavor)))
    ;; Generally, default to HTML if no known extension.
    (cond
     ((and (not parg) (equal char ?a))
      (cond
       ((eq markup-flavor 'ltx)
        (setq begthing "\\ac{")
        (setq handleg-ac t)
        (setq endthing "}"))))
     ((and (not parg) (equal char ?b))
      (cond
       ((eq markup-flavor 'xml)
        (setq begthing "<emphasis role=\"bold\">")
        (setq endthing "</emphasis>"))
       ((eq markup-flavor 'ltx)
        (setq begthing "\\textbf{")
        (setq endthing "}"))
       ((or (eq markup-flavor 'html) (eq major-mode 'text-mode))
        (setq begthing "<strong>")
        (setq endthing "</strong>"))))
     ((and (not parg) (equal char ?i))
      (cond
       ((eq markup-flavor 'ltx)
        (setq begthing "\\textit{")
        (setq endthing "}"))
       (t
        (setq begthing "<i>")
        (setq endthing "</i>"))))
     ((and (not parg) (equal char ?c))
      (cond
       ((eq markup-flavor 'ltx)
        (setq begthing "\\code{")
        (setq endthing "}"))
       ((eq markup-flavor 'xml)
        (setq begthing "<code>")
        (setq endthing "</code>"))
       (t ; Ah, what the heck, let's default to XML anyway.
        (setq begthing "<code>")
        (setq endthing "</code>"))))
     ((and (not parg) (equal char ?e))
      (cond
       ((eq markup-flavor 'xml)
        (setq begthing "<emphasis>")
        (setq endthing "</emphasis>"))
       ((eq markup-flavor 'ltx)
        (setq begthing "\\emph{")
        (setq endthing "}"))
       ((or t ; Remove this `t' if we ever choose another default for "e".
            (eq markup-flavor 'html)
            (eq major-mode 'text-mode))
        (setq begthing "<em>")
        (setq endthing "</em>"))))
     ((and (not parg) (equal char ?f))
      (cond
       ((eq markup-flavor 'ltx)
        (setq begthing "\\fullref{")
        (setq endthing "}"))
       ((eq markup-flavor 'texi)
        (setq begthing "@file{")
        (setq endthing "}"))
       ((eq markup-flavor 'xml)
        (setq begthing "<firstterm>")
        (setq endthing "</firstterm>"))))
     ((and (not parg) (equal char ?r))
      (when (eq markup-flavor 'ltx)
        (setq begthing "\\ref{")
        (setq endthing "}")))
     ((and (not parg) (equal char ?u))
      (if (eq markup-flavor 'ltx)
          (progn
            (setq begthing "\\otsurl{")
            (setq endthing "}"))
        (setq begthing "<url>")
        (setq endthing "</url>")))
     ((and (not parg) (equal char ?t))
      (cond
       ((eq markup-flavor 'ltx)
        (setq begthing "\\texttt{")
        (setq endthing "}"))
       (t
        (setq begthing "<tt>")
        (setq endthing "</tt>"))))
     ((and (not parg) (equal char ?s))
      (cond
       ((eq markup-flavor 'html)
        (setq begthing "<strong>")
        (setq endthing "</strong>"))
       ((eq markup-flavor 'ltx)
        (setq begthing "\\textbf{ ")
        (setq endthing "}"))
       ((eq markup-flavor 'xml)
        (setq begthing "<systemitem>")
        (setq endthing "</systemitem>"))))
     ((and (not parg) (equal char ?l))
      (setq begthing "<literal>")
      (setq endthing "</literal>"))
     ((and (not parg) (equal char ?r))
      (cond
       ((eq markup-flavor 'xml)
        (setq begthing "<remark>")
        (setq endthing "</remark>"))
       ((eq markup-flavor 'html)
        (setq begthing "<font color=\"red\">")
        (setq endthing "</font>"))))
     ((and (not parg) (equal char ?e))
      (cond
       ((eq markup-flavor 'xml)
        (setq begthing "<emphasis>")
        (setq endthing "</emphasis>"))
       ((eq markup-flavor 'texi)
        (setq begthing "@emph{")
        (setq endthing "}"))
       ((eq markup-flavor 'ltx)
        (setq begthing "{\\em ")
        (setq endthing "}"))))
     ((and (not parg) (equal char ?c))
      (cond
       ((eq markup-flavor 'texi)
        (setq begthing "@code{")
        (setq endthing "}"))
       ((eq markup-flavor 'xml)
        (setq begthing "<comment><para>(")
        (setq endthing ")</para></comment>"))))
     ((and (not parg) (equal char ?n))
      (cond
       ((eq markup-flavor 'xml)
        (setq begthing "<note>")
        (setq endthing "</note>"))))
     ((and (not parg) (equal char ?F))
      (cond
       ((eq markup-flavor 'xml)
        (setq begthing "<filename>")
        (setq endthing "</filename>"))))
     ((equal char ?x)
      (cond
       ((eq markup-flavor 'xml)
        (if parg
            (progn
              (setq begthing "<phrase output=\"printed\"> in <xref linkend=\"")
              (setq endthing "\" /></phrase>"))
          (setq begthing "<xref linkend=\"")
          (setq endthing "\"/>")))))
     ((or (equal char ?{) (equal char ?}))    ; get matching char
      (setq begthing ?{)
      (setq endthing ?}))
     ((or (equal char ?\() (equal char ?\)))  ; get matching char
      (setq begthing ?\()
      (setq endthing ?\)))
     ((or (equal char ?<) (equal char ?>))    ; get matching char
      (setq begthing ?<)
      (setq endthing ?>))
     ((and (equal char ?') (eq major-mode 'emacs-lisp-mode))
      (setq begthing ?`)
      (setq endthing ?'))
     ;; Having backticks on both sides is useful in some wikis.
     ;; ((equal char ?`)    ; do matching quote, but only via backtick
     ;;  (setq begthing ?`)
     ;;  (setq endthing ?'))
     ((or (equal char ?\[) (equal char ?\]))    ; get matching char
      (setq begthing ?\[)
      (setq endthing ?\]))
     ((and (not parg)                         ; do *TeX quotes
           (equal char ?\")
           (eq markup-flavor 'ltx)
           (not (nth 4 (syntax-ppss)))) ; see commit 2b404e8391b7 of
                                        ; 30 Aug 2016 in GNU Emacs.
      (setq begthing "``")
      (setq endthing "''")))

    ;; Okay, now discover the appropriate boundaries and surround:
    (re-search-backward "^\\|\\s-" (point-min))
    (if (not (bolp))
        (re-search-forward "\\s-")
      (if (looking-at "\\s-") (re-search-forward "\\s-")))
    (if (stringp begthing)
        (insert begthing)
      (insert-char begthing 1))
    (let ((opoint (point)))
      (if (re-search-forward "\\s-\\|\n" (point-max) t)
          (forward-char -1)
        (goto-char (point-max)))
      (let ((lastchar (char-after (1- (point)))))
        (if (= lastchar ?,)
            (forward-char -1)))
      (if (stringp endthing)
          (insert endthing)
        (insert-char endthing 1))
      (if (= (point) (1+ opoint))
          (forward-char -1)
        (when handle-ac
          ;; Since we know we're surrounding an acronym, let's take
          ;; care of upcasing the surrounded text too -- thus
          ;; relieving my grateful pinkies of some shift-key time.
          (upcase-region opoint (point))
          ;; Leave point right before the opening "{", in case this
          ;; "\ac" needs to be "\acp" or one of other other variants.
          (goto-char (1- opoint))))
      )))

;;; from excalamus: convert bs to fs / \

;; (defun xc/convert-slashes (&optional beg end)
;;   "Convert backslashes to forward slashes.

;; Only convert within region defined by BEG and END.  Use current
;; line if no region is provided."
;;   (interactive)
;;   (let* ((beg (or beg (if (use-region-p) (region-beginning)) (line-beginning-position)))
;;         (end (or end (if (use-region-p) (region-end)) (line-end-position))))
;;     (subst-char-in-region beg end ?// ?/)
;;     (replace-string "//" "/" nil beg end)))
;; TODO: invalid read syntax ?

;;; from junkw: kill-word-dwim

(defun kill-word-dwim (arg)
  "Call the `kill-word'  you want (Do What I Mean).

With argument ARG, do kill commands that many times."
  (interactive "p")
  (cond ((and (called-interactively-p 'any) transient-mark-mode mark-active)
         (kill-region (region-beginning) (region-end)))
        ((eobp)
         (backward-kill-word arg))
        (t
         (let ((char (char-to-string (char-after (point)))))
           (cond ((string-match "\n" char)
                  (delete-char 1) (delete-horizontal-space))
                 ((string-match "[\t ]" char)
                  (delete-horizontal-space))
                 ((string-match "[-@\[-`{-~]" char)
                  (kill-word arg))
                 (t
                  (beginning-of-thing 'word) (kill-word arg)))))))

(provide 'editing-extras)
