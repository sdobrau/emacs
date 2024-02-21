(leaf phi-search
  :ensure t
  :require phi-replace)
;; NOTE: cannot exit out of it if the window in which selection was started is
;; deleted
;; "target window deleted"
;; :bind (("C-s" . phi-search)
;;        ("C-r" . phi-search-backward)
;;        ("M-%" . phi-replace-query)
;;        (:Info-mode-map ;; phi is not recursive
;;         ("C-s" . isearch-forward)
;;         ("C-r" . isearch-backward))))

(leaf anzu
  :ensure t
  :preface
  ;; deftsp
  (defun tl/anzu-update-mode-line (here total)
    "Custom update function which does not propertize the status."
    (when anzu--state
      (let ((status (cl-case anzu--state
                      (search (format "(%s/%d%s)"
                                      (anzu--format-here-position here total)
                                      total (if anzu--overflow-p "+" "")))
                      (replace-query (format "(%d replace)" total))
                      (replace (format "(%d/%d)" here total)))))
        status)))
  :global-minor-mode global-anzu-mode
  :config
  (setq anzu-mode-line-update-function 'tl/anzu-update-mode-line))

