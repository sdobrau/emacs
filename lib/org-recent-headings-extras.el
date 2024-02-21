(defun my-org-recent-headings--show-entry-indirect (real)
  "Show heading specified by REAL in an indirect buffer.
REAL is a plist with `:file', `:id', and `:regexp' entries.  If
`:id' is non-nil, `:file' and `:regexp may be nil.'"
  ;; By using `save-excursion' and `save-restriction', this function doesn't
  ;; change the position or narrowing of the entry's underlying buffer.
  (let ((marker (org-recent-headings--entry-marker real)))
    (save-excursion
      (save-restriction
        (my-persp-switch-to-buffer-maybe-popper-popup* (marker-buffer marker))
        (widen)
        (goto-char marker)
        (org-reveal)
        (org-show-entry)
        (org-tree-to-indirect-buffer)))))
