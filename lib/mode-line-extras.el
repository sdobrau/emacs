;;; from jao: simple mode-line hide/show

(defun jao-toggle--face-height (face &optional all-frames)
  (let* ((h (face-attribute face :height (window-frame)))
         (nh (if (eq 'unspecified h) 1 'unspecified)))
    (set-face-attribute face (when (not all-frames) (window-frame)) :height nh)))

(defun jao-toggle-mode-line (&optional all-frames)
  (interactive)
  (jao-toggle--face-height 'mode-line all-frames))

(defun jao-toggle-inactive-mode-line (&optional all-frames)
  (interactive)
  (jao-toggle--face-height 'mode-line-inactive all-frames))

(defun jao-echo-mode-line ()
  (interactive)
  (message "%s" (format-mode-line mode-line-format)))


;;; center mode-line stuffs from stackexchange
;; https://emacs.stackexchange.com/questions/16654/how-to-re-arrange-things-in-mode-line

(setq mode-line-align-left
      '(""
        ""
        ("")
        " "))
;; (setq mode-line-align-left
;;       '(""
;;         ""
;;         (:propertize ((winum-get-number-string (selected-windows))) face font-lock-string-face)
;;         " "))

(setq mode-line-align-middle
      '(""
        (:propertize "%b" face font-lock-string-face)
        " "
        ))

(setq mode-line-align-right
      '(""
	      ""
        ""
        ""))

(setq-default mode-line-format
              (list
               mode-line-align-left
               '(:eval (mode-line-fill-center 'mode-line
                                              (reserve-left/middle)))
               mode-line-align-middle
               '(:eval
                 (mode-line-fill-center 'mode-line
                                        (reserve-middle/right)))))


;; Here is the code you need to make this work. I don't know where I got this from, the filling
;; functions are taken from powerline if I remember correctly.

(defun mode-line-fill-right (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(defun mode-line-fill-center (face reserve)
  "Return empty space using FACE to the center of remaining space leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ center (.5 . right-margin)) ,reserve
                                             (.5 . left-margin))))
              'face face))

(defconst RIGHT_PADDING 1)

(defun reserve-left/middle ()
  (/ (length (format-mode-line mode-line-align-middle)) 2))

(defun reserve-middle/right ()
  (+ RIGHT_PADDING (length (format-mode-line mode-line-align-right))))

;; To read more about the formatting rules of the mode line I recommend this[archive-link] blog
;; post. Here is a short example how you might want to define mode-line-align-left/middle/right:



(provide 'mode-line-extras)
