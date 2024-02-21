;;; aj lol

(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))

;;; manateelazycat indentation lol

(defun backward-indent ()
  "Backward indent."
  (interactive)
  (goto-column (- (current-column) yaml-indent-offset)) ;; addition
  ;; Remove blank after point if current line is empty line.
  (when (looking-at "\\s-+$")
    (kill-region (point)
                 (save-excursion
                   (end-of-line)
                   (point)))))

(defun goto-column (number)
  "Untabify, and go to a column NUMBER within the current line (0 is beginning of the line)."
  (interactive "nColumn number: ")
  (move-to-column number t))

;;; pashinin

(defun yaml-indent-line ()
  "Indent the current line.
The first time this command is used, the line will be indented to the
maximum sensible indentation.  Each immediately subsequent usage will
back-dent the line by `yaml-indent-offset' spaces.  On reaching column
0, it will cycle back to the maximum sensible indentation."
  (interactive "*")
  (let ((ci (current-indentation))
        (cc (current-column))
        (need (yaml-compute-indentation)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      ;;(if (and (equal last-command this-command) (/= ci 0))
      ;;    (indent-to (* (/ (- ci 1) yaml-indent-offset) yaml-indent-offset))
      ;;  (indent-to need))
      (indent-to need))
    (if (< (current-column) (current-indentation))
        (forward-to-indentation 0))
    ))

(defun my-yaml-delete-backward-char-or-yaml-indent ()
  (interactive)
  (if (looking-at-p "[ \t]*$") ;; line empty?
      (delete-backward-char yaml-indent)
    (delete-backward-char)))

(defun my-newline-and-indent-yaml-sequence (&optional arg)
  (interactive "p")
  (newline-and-indent)
  (backward-char yaml-indent))

;; backward delete char 2 times lol
(defun my-del-2-backwards ()
  (interactive)
  (setq last-command-event 'backspace)
  (backward-delete-char 1 nil)
  (setq last-command-event 'backspace)
  (backward-delete-char 1 nil)
  (setq last-command-event 101))

(defun my-tempel-insert-curly-brace ()
  (interactive)
  (tempel-insert '{}))



  (provide 'yaml-mode-extras)
