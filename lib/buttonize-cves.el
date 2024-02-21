;;; https://blog.lord.geek.nz/2019/04/17/emacs-buttons/ buttonize cve in buffer

(defun buttonize-buffer-with-cves (bufname)
  "Mark CVEs in a given buffer as hyperlinks."
  (interactive "p")
  (goto-char (point-min))
  (while (re-search-forward "CVE-[[:digit:]]+-[[:digit:]]+" nil 1)
    (let* ((start (match-beginning 0))
           (end (match-end 0))
           (cve (buffer-substring start end))
           (lexical-binding t))
      (make-button (match-beginning 0) (match-end 0)
                   'url (format "https://nvd.nist.gov/vuln/detail/%s" cve)
                   'action (lambda (button)
                             (let ((url (button-get button 'url)))
                               (prot-eww url t)))
                   'help-echo (format "Visit %s at NVD"
                                      cve)))))



(provide 'buttonize-cves)
