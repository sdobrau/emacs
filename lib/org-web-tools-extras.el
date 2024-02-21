;;; mine

(defun my-owb-to-org-other-window-next-link ()
  (interactive)
  ;; TODO: why extra heading? fuck
  ;; TODO: integrate; multiple links and do all of them in one run
  ;; TODO: as child node of current node
  ;; TODO: test against
  ;; https://python-wordpress-xmlrpc.readthedocs.io/en/latest/dev/testing.html#requirements
  ;; TODO: remove redundant child node of pasted org
  ;; TODO: ughhhhhh remove redundant headings
  (shr-maybe-probe-and-copy-url (get-text-property (point) 'shr-url))
  (xc/switch-to-last-window)
  (org-end-of-line)
  (org-web-tools-insert-web-page-as-entry
   (car (my-redguard-read-n-from-kill-ring 1)))
  (outline-show-subtree)
  (outline-next-heading)
  (org-shiftmetaleft)
  (dotimes (i 3) (org-kill-line)) ; 3 times to remove article
  (outline-previous-heading)
  (outline-hide-body)
  (outline-forward-same-level 1)
  (org-cycle) ; ?
  (xc/switch-to-last-window)
  (shr-next-link))

(defun my-owb-shift-left-and-remove-redundant-header (&optional arg)
  (interactive "p")
  (org-next-visible-heading 1)
  (org-promote-subtree)
  (org-previous-visible-heading 1)
  (org-cut-subtree))

(defun my-owb-shift-left-and-remove-redundant-header-next-header-same-level
    (&optional arg)
  (interactive "p")
  (my-owb-shift-left-and-remove-redundant-header)
  (org-forward-heading-same-level 1))



(provide 'org-web-tools-extras)
