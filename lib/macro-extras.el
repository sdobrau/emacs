;;; mine
;;;; set-face-foreground for multiple faces
;; TODO: generalize for attributes
;; just a wrapper around seq-map etc
;; TODO: (macro-set-face-background-color
;; '(((highlight-indentation-face)
;;    . (color-darken-name
;;       (face-attribute 'default :background) 1.3))))
;; ^ (color-darkenname etc is not evaluated)
;; learn to evaluate

(defmacro macro-set-face-foreground-color (l)
  "Set a color for each face. Takes an alist.
car is the face, cdr is the color. car can be a list of faces."
  `(mapc
    (lambda (x) ;; for each list of face-color pairings
      (mapc
       (lambda (y) ;; for each face inside the face-list
         (set-face-attribute y
                             nil
                             :foreground
                             ;; apply the corresponding color part of the
                             ;; alist the face was found into ( . "red")
                             (cdr x)))
       (car x))) ;; in the face list


    ,l)) ;; inside the provided nested alist

;; TODO: generalize it

(defmacro macro-set-face-background-color (l)
  "Set a color for each face. Takes an alist.
car is the face, cdr is the color. car can be a list of faces."
  `(mapc
    (lambda (x) ;; for each list of face-color pairings
      (mapc
       (lambda (y) ;; for each face inside the face-list
         (set-face-attribute y
                             nil
                             :background
                             ;; apply the corresponding color part of the
                             ;; alist the face was found into ( . "red")
                             ,(cdr x)))
       (car x))) ;; in the face list
    ,l))

;;;; repeat macro for all files in current tab list

;; TODO: test but not now

(defun my-run-last-macro-in-tab-list-2 (&optional arg)
  (interactive "p")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (save-excursion          ;;
  ;;   (save-window-excursion ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (mapcar (lambda (buf)
            (progn (switch-to-buffer buf)
                   (call-last-kbd-macro arg)))
          (tab-line-tabs-mode-buffers)))




(provide 'macro-extras)
