;;; daanturo

;;;###autoload
(defun daanturo-expand-profiler-report ()
  (interactive)
  (daanturo-save-line-col
    (goto-char (point-min))
    ;; expand all non-zero entries
    (while (re-search-forward "\\([1-9]\\|[0-9]0\\)%[[:space:]]+\\+ " nil ':noerror)
      (profiler-report-expand-entry 'full)
      (goto-char (point-min)))
    ;; collapse all zero entries
    (goto-char (point-min))
    (while (re-search-forward "[[:space:]]0%[[:space:]]+\\-" nil ':noerror)
      (profiler-report-collapse-entry)
      (goto-char (point-min)))))



(provide 'profiler-report-extras)
