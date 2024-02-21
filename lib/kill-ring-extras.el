;;; redguard

(defun redguard-read-n-from-kill-ring ()
  (let* ((cands (subseq kill-ring 0 (min (read-number "fetch N `kill-ring'?" 1)
                                         (length kill-ring)))))
    (mapc (lambda (txt)
            (set-text-properties 0 (length txt) nil txt)
            txt)
          cands)))

(defun my-redguard-read-n-from-kill-ring (give-nr)
  (let* ((cands (subseq kill-ring 0 (min give-nr
                                         (length kill-ring)))))
    (mapc (lambda (txt)
            (set-text-properties 0 (length txt) nil txt)
            txt)
          cands)))



(provide 'kill-ring-extras)
