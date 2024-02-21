ssss;;; cunene TODO: implement

(defun cunene/org-cycle-parent (argument)
  "Go to the nearest parent heading and execute `org-cycle'.

ARGUMENT determines the visible heading."
  (interactive "p")
  (if (org-at-heading-p)
      (outline-up-heading argument)
    (org-previous-visible-heading argument))
  (org-cycle))

(defun cunene/org-show-next-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (interactive)
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (outline-show-children))
    (outline-next-heading)
    (unless (and (bolp) (org-at-heading-p))
      (org-up-heading-safe)
      (outline-hide-subtree)
      (user-error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (outline-show-children)))

(defun cunene/org-show-previous-heading-tidily ()
  "Show previous entry, keeping other entries closed."
  (interactive)
  (let ((pos (point)))
    (outline-previous-heading)
    (unless (and (< (point) pos) (bolp) (org-at-heading-p))
      (goto-char pos)
      (outline-hide-subtree)
      (user-error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (outline-show-children)))

;; TODO: f/b same level?

;;; mine

(defun my-org-format-src-block-from-outside ()
  (interactive)
  (org-edit-special)
  (my-org-format-src-block-from-within)
  (org-edit-src-exit))

(defun my-org-format-src-block-from-within () ;;TODO: just indent
  (interactive)
  (mark-whole-buffer)
  (indent-for-tab-command)
  (pop-global-mark))

(defun my-org-yank-src-block-properly ()
  (interactive)
  (let ((beg (point))
        (last-mode (progn (search-backward "#+begin_src")
                          (forward-word 3)
                          (thing-at-point 'word))))
    (message last-mode)
    (goto-char beg)
    (insert (concat "#+begin_src " last-mode))
    (newline)
    (yank)
    (newline)
    (beginning-of-line)
    (insert "#+end_src")
    (my-org-format-src-block-from-outside)
    (next-line 2)))

(defun my-edit-next-src-block-from-outside-current-src-edit-buffer ()
  (interactive)
  (org-edit-src-exit)
  (my-goto-next-src-block)
  (org-edit-special))

(defun my-edit-prev-src-block-from-outside-current-src-edit-buffer ()
  (interactive)
  (org-edit-src-exit)
  (my-goto-prev-src-block)
  (org-edit-special))

(defun my-goto-next-src-block ()
  (interactive)
  (search-forward "#+begin_src"))

(defun my-goto-prev-src-block ()
  (interactive)
  (search-backward "#+begin_src"))

(defun org-insert-subheading-after-point-no-newline-end-of-heading ()
  (interactive)
  (end-of-line)
  (org-insert-subheading 4)
  (previous-line)
  (org-kill-line)
  (end-of-line))

(defun my-org-insert-link ()
  "Insert org link where default description is set to html title."
  (interactive)
  (let* ((url (read-string "URL: "))
         (title (get-html-title-from-url url)))
    (org-insert-link nil url title)))

(defun get-html-title-from-url (url)
  "Return content in <title> tag."
  (let (x1 x2 (download-buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer download-buffer)
      (beginning-of-buffer)
      (setq x1 (search-forward "<title>"))
      (search-forward "</title>")
      (setq x2 (search-backward "<"))
      (mm-url-decode-entities-string (buffer-substring-no-properties
                                      x1 x2)))))

;; https://emacs.stackexchange.com/questions/21947/make-all-done-org-mode-items-invisible-collapsed

(defun croad-langshan/org-get-folded-state ()
  (cond
   ((not (or (org-at-item-p) (org-at-heading-p)))
    'not-at-node)
   ((org-before-first-heading-p)
    'not-at-node)
   (t
    (let (eoh eol eos has-children children-skipped struct)
      ;; First, determine end of headline (EOH), end of subtree or item
      ;; (EOS), and if item or heading has children (HAS-CHILDREN).
      (save-excursion
        (if (org-at-item-p)
            (progn
              (beginning-of-line)
              (setq struct (org-list-struct))
              (setq eoh (point-at-eol))
              (setq eos (org-list-get-item-end-before-blank (point) struct))
              (setq has-children (org-list-has-child-p (point) struct)))
          (org-back-to-heading)
          (setq eoh (save-excursion (outline-end-of-heading) (point)))
          (setq eos (save-excursion (org-end-of-subtree t t)
                                    (when (bolp) (backward-char)) (point)))
          (setq has-children
                (or (save-excursion
                      (let ((level (funcall outline-level)))
                        (outline-next-heading)
                        (and (org-at-heading-p t)
                             (> (funcall outline-level) level))))
                    (save-excursion
                      (org-list-search-forward (org-item-beginning-re) eos t)))))
        ;; Determine end invisible part of buffer (EOL)
        (beginning-of-line 2)
        (while (and (not (eobp)) ;; this is like `next-line'
                    (get-char-property (1- (point)) 'invisible))
          (goto-char (next-single-char-property-change (point) 'invisible))
          (and (eolp) (beginning-of-line 2)))
        (setq eol (point)))
      (cond
       ((= eos eoh)
        'empty-node)
       ((or (>= eol eos)
            (not (string-match "\\S-" (buffer-substring eol eos))))
        'folded)
       (t
        'not-folded))))))

(defun croad-langshan/org-tree-can-fold-p ()
  (not (member (croad-langshan/org-get-folded-state) (list 'folded 'empty-node))))

(defun croad-langshan/org-cycle-until-folded ()
  (while (croad-langshan/org-tree-can-fold-p)
    (org-cycle)))

(defun croad-langshan/org-hide-done-entries-in-range (start end)
  (save-excursion
    (goto-char end)
    (while (and (outline-previous-heading) (> (point) start))
      (when (org-entry-is-done-p)
        (croad-langshan/org-cycle-until-folded)))))

(defun croad-langshan/org-hide-done-entries-in-region (start end)
  (interactive "r")
  (croad-langshan/org-hide-done-entries-in-range start end))

(defun croad-langshan/org-hide-done-entries-in-buffer ()
  (interactive)
  (croad-langshan/org-hide-done-entries-in-range (point-min) (point-max)))

(defun my-org-insert-yank-at-end-of-heading-with-spacing (&optional arg)
  (interactive "p")
  (org-next-visible-heading 1)
  (backward-paragraph)
  (forward-paragraph)
  (org-return)
  (org-yank))

(defun my-org-insert-heading-and-yank-in-it (&optional arg)
  (interactive "p")
  (org-insert-heading-respect-content)
  (insert (read-string "headline: "))
  ;; corner case, otherwise function above yanks above heading
  (org-return)
  (org-return)
  (org-yank)
  (org-next-visible-heading 1))

(defun my-consult-org-insert-yank-into-heading (&optional arg)
  (interactive "p")
  (consult-org-heading)
  (my-org-insert-yank-at-end-of-heading-with-spacing))

;;; jao: org link support

(defun jao-org-link-at-point (&optional copy)
  (when (thing-at-point-looking-at "\\[\\[\\([^]]+\\)\\]\\[[^]]+\\]\\]")
    (when copy (kill-ring-save (match-beginning 1) (match-end 1)))
    (match-string-no-properties 1)))

(defun jao-org-copy-link-at-point ()
  (interactive)
  (message "%s" (or (jao-org-link-at-point t) "No link at point")))

;;; TODO org-return does not newline if point on header

;;; TODO: sane consult-orgheading

;; only show first level

;;; fucking org-indent-agent

(defun my-cancel-org-indent-initialize-agent ()
  (interactive)
  (let ((count 0))
    (dolist (timer timer-idle-list)
      (when (eq 'org-indent-initialize-agent (aref timer 5))
        (cl-incf count)
        (cancel-timer timer)))
    (when (> count 0)
      (message "Cancelled %s org-indent-initialize-agent timers" count))))

;;; fontification of inline code https://stackoverflow.com/a/20652913

;;; daanturo

;;;###autoload
(defun daanturo-org-sort-entries-by-time-reversed (&optional ascending)
  (interactive "P")
  (save-excursion
    (outline-up-heading 1)
    (org-sort-entries nil (string-to-char (if ascending "t" "T")))))

;;; from unpackaged (alphapapa)

;;;###autoload
(defun unpackaged/org-fix-blank-lines (&optional prefix)
  "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
  (interactive "P")
  (org-map-entries (lambda ()
                     (org-with-wide-buffer
                      ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                      ;; newlines before the current heading, so we do this part widened.
                      (while (not (looking-back "\n\n" nil))
                        ;; Insert blank lines before heading.
                        (insert "\n")))
                     (let ((end (org-entry-end-position)))
                       ;; Insert blank lines before entry content
                       (forward-line)
                       (while (and (org-at-planning-p)
                                   (< (point) (point-max)))
                         ;; Skip planning lines
                         (forward-line))
                       (while (re-search-forward org-drawer-regexp end t)
                         ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                         ;; for some reason it doesn't work correctly when operating on hidden text.
                         ;; This works, taken from `org-agenda-get-some-entry-text'.
                         (re-search-forward "^[ \t]*:END:.*\n?" end t)
                         (goto-char (match-end 0)))
                       (unless (or (= (point) (point-max))
                                   (org-at-heading-p)
                                   (looking-at-p "\n"))
                         (insert "\n"))))
                   t (if prefix
                         nil
                       'tree)))



(provide 'org-extras)
