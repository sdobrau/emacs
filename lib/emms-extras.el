;;; mine
;;;; easy show format

(defcustom emms-show-format "%s"
  "*The format to use for `emms-show'.
Any \"%s\" is replaced by what `emms-track-description-function' returns
for the currently playing track."
  :group 'emms
  :type 'string)

;;;; show cover (as exwm floating window)
;; TODO: try embedded if no jpg
(defun exwm-emms-show-cover ()
  (interactive)
  ;; (dimmer-mode 1)
  (let ((command (concat
                  "sxiv -b -g 900x900 -s w "
                  "\""
                  (car
                   (directory-files
                    (f-dirname
                     (cdr
                      (nth 2 (emms-playlist-current-selected-track)))) ;; lol
                    'full
                    (rx (or (* "jpg")
                            (* "JPG")
                            (* "PNG")
                            (* "png")
                            (* "jpeg")))))
                  "\"")))
    (start-process-shell-command command nil command)))
;; (sit-for 1) ;; lol
;; (add-hook 'kill-buffer-hook #'(lambda () (dimmer-mode -1)) nil t)))



(provide 'emms-extras)
