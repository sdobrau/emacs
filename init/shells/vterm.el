(leaf vterm
  :ensure t
  :require t ;; lazy-load ?
  :custom ((vterm-buffer-name-string . "vterm %s")
           (vterm-timer-delay . 0.0001)
           (vterm-max-scrollback . 100000) ; max
           (vterm-disable-bold-font . t))
  :bind (:vterm-mode-map
         (("C-u" . vterm-send-C-u)
          ("M-g" . nil)
          ([tab] . vterm-send-tab)
          ("TAB" . vterm-send-tab)
          ("C-l" . vterm-send-C-l)
          ("C-M-l" . vterm-copy-mode)
          ("C-m" . nil)
          ("C-g" . vterm-send-C-g)
          ("C-]" . vterm)
          ("C-m" . nil)
          ("C-y" . vterm-yank)
          ("M-y" . vterm-yank-pop)
          ("C-c" . vterm--self-insert)
          ("s-s" . my-vterm-isearch-forward)
          ("s-r" . my-vterm-isearch-forward)
          ("C-m" . vterm-send-C-j)))) ;; wtf?

;;(add-to-list 'vterm-eval-cmds '("man" woman))
;; ;; TODO : special isearch wrapper lol

(leaf vterm-extras
  :after vterm
  :bind (("C-x M-1" . my-vterm)
         ("C-x M-2" . my-vterm-v)
         ("C-x M-3" . my-vterm-h)
         (:vterm-mode-map
          (("C-x M-1" . my-vterm)
           ("C-x M-2" . my-vterm-v)
           ("C-x M-3" . my-vterm-h)
           ("C-k" . vterm-send-C-k-and-kill))))) ;; C-k but also put to kill

;; TODO: think
(leaf vterm-anywhere
  :require t)

(leaf vterm-macros
  :after vterm
  :require t)

(leaf vterm-toggle
  :after vterm
  :ensure t)

;; vterm for project
(leaf multi-vterm
  :after vterm multi-project
  :ensure t
  :bind ("C-x $" . multi-vterm-project)) ;; selective-display
