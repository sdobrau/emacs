;;; mine
;;;; view-back and front

(defun viewing-1 ()
  (interactive)
  (default-text-scale-increment -30)
  (set-face-attribute 'variable-pitch nil :font "Lato" :weight 'semi-light :height 115))

(defun viewing-2 ()
  (interactive)
  (default-text-scale-increment 30)
  (set-face-attribute 'variable-pitch nil :font "Lato" :weight 'semi-light :height 145))



(provide 'default-text-scale-extras)
