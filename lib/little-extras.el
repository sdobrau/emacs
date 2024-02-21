;;; mine
;;;; The Time.

(defun time ()
  (interactive)
  (message (format-time-string "⏰ is %H:%M")))

;; TODO: wtf? popup not working, maybe function or popper-popup-status
;; is at fault, might have to add it to shackle as well ? temporarily?
;; TODO: generalize
(defun my-write-a-function-quickly ()
  (interactive)
  (let* ((file-buf (nth 5 (f-split my-little-functions))))
    (with-current-buffer (find-file-noselect my-little-functions)
      (super-save-mode 0)
      (setq-local popper-popup-status 'popup)
      (add-hook
       'after-save-hook
       #'(lambda() (progn
                (setq-local popper-popup-status 'raised)
                (require 'little-extras)
                (my-popper-toggle-latest-bury-all-if-current-popup))) 9 t))
    (my-persp-switch-to-buffer-maybe-popper-popup*
     (find-file-noselect my-little-functions))))

;; convert display table to x’s
;; marcowahl

(defvar mw-allX-display-table
  (let ((table (make-display-table))
        (i 0))
    (while (< i 26)
      (aset table (+ i ?a) (vector ?A))
      (aset table (+ i ?A) (vector ?X))
      (setq i (1+ i)))
    table)
  "Char table to display chars as x and X.")

(defun mw-toggle-allX-mode ()
  "Toggle the use of x for all letters for the current window."
  (interactive)
  (if (eq (window-display-table) mw-allX-display-table)
      (set-window-display-table (selected-window) nil)
    (if (null (window-display-table))
        (set-window-display-table (selected-window) mw-allX-display-table))))

;; yrr cute
;;; Eric Schulte posted this in emacs-devel list

(defun cat-command ()
  "A command for cats."
  (interactive)
  (require 'animate)
  (let ((mouse "
           ___00
        ~~/____'>
          \"  \"")
        (h-pos (floor (/ (window-height) 2)))
        (contents (buffer-string))
        (mouse-buffer (generate-new-buffer "*mouse*")))
    (save-excursion
      (switch-to-buffer mouse-buffer)
      (insert contents)
      (setq truncate-lines t)
      (animate-string mouse h-pos 0)
      (dotimes (_ (window-width))
        (sit-for 0.02)
        (dotimes (n 3)
          (goto-line (+ h-pos n 2))
          (move-to-column 0)
          (insert " "))))
    (kill-buffer mouse-buffer)))

;; xc

(defun suicide ()
  "Kill all Emacs processes." ;; xc
  (interactive)
  (let ((cmd (if (eq window-system 'x)
                 "killall -9 -r emacs" ; probably won't kill server administered by systemd
               "taskkill /f /fi \"IMAGENAME eq emacs.exe\" /fi \"MEMUSAGE gt 15000\"")))
    (shell-command cmd)))



(provide 'little-extras)
