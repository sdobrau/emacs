(leaf org-cliplink
  :ensure t
  :require t
  :custom (org-cliplink-transport-implementation . 'curl)
  :bind (:org-mode-map
         (("C-c C-l" . org-cliplink))))

(leaf org-protocol
  :ensure nil
  :leaf-defer t
  :config
  (add-to-list 'org-protocol-protocol-alist
               '("Add to bookmarks"
                 :protocol "bookmark"
                 :function my-add-ff-to-bm)))

(leaf org-protocol-capture-html
  :quelpa (org-protocol-capture-html
           :fetcher github
           :repo "alphapapa/org-protocol-capture-html")
  :require t)

;; DEBUG
;; (leaf org-capture-pop-frame
;;   :ensure t
;;   :after org-capture
;;   :require t
;;   :custom '(ocpf-frame-parameters . ((name . "org-capture-pop-frame")
;;                                      (width . 80)
;;                                      (height . 200)
;;                                      (tool-bar-lines . 0)
;;                                      (menu-bar-lines . 0))))

(leaf org-super-links
  :after helm-org-rifle
  :bind (:org-mode-map
         (("C-c C-/" . org-super-links-link))))

(leaf valign
  :after org
  :ensure t
  :disabled t
  :hook (org-mode-hook . valign-mode))

(leaf helm-org-rifle
  :after helm org
  :ensure t
  :custom ((helm-org-rifle-multiline . nil)
           (helm-org-rifle-heading-contents-separator . ":")
           (helm-org-rifle-show-path . nil)
           (helm-org-rifle-after-command-hook . 'org-narrow-to-subtree)
           (helm-org-rifle-before-command-hook . 'widen)))

;; https://github.com/alphapapa/org-web-tools/issues/48
(leaf org-web-tools
  :after shrface org
  :ensure t
  :custom (org-web-tools--pandoc--no-wrap-option . "--wrap=none --columns=80")
  :bind (:org-mode-map
         (("C-M-w" . org-web-tools-insert-web-page-as-entry))))

(leaf org-web-tools-extras
  :after org-web-tools
  :require t
  :bind (:shrface-mode-map
         (("C-c ." . my-owb-to-org-other-window-next-link))))

(leaf org-download
  :after org
  :ensure t
  :hook (org-mode-hook . org-download-enable)
  :bind (("C-x M-a c" . org-download-clipboard)
         ("C-x M-a s" . org-download-screenshot))
  :custom ((org-download-method . 'directory)
           (org-download-backend . "curl")
           (org-download-screenshot-method . "scrot -s -f -F %s")))

(leaf org-download-extras
  :after org-download
  :require t)

(leaf org-ql
  :ensure t
  :after org
  :require t)

(leaf org-tanglesync
  :ensure t
  :after org)

(leaf org-recent-headings
  :ensure t
  :after org
  :bind ("M-g C-o" . org-recent-headings)
  :config
  (add-to-list 'org-recent-headings-advise-functions #'consult-org-heading)
  (org-recent-headings-mode))

(leaf org-unique-id
  :disabled t
  ;; triggers tons of org...? initialize agent?
  :after org
  :config
  (remove-hook 'org-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook
                           (lambda ()
                             (when (and (eq major-mode       'org-mode)
                                        (eq buffer-read-only nil))
                               (org-unique-id)))))))

(leaf unpackaged
  :require t
  :config (unpackaged/def-org-maybe-surround "_" "~" "=" "*" "/" "+"))



(leaf org-autolist
  :ensure t
  :hook (org-mode-hook . org-autolist-mode))

(leaf org-mru-clock
  :ensure t
  :bind (("C-c C-x C-x" . org-clock-in-last)
         ("C-c C-x s" . org-mru-clock-select-recent-task)
         ("C-c C-x C-o" . org-clock-out)))

(leaf org-variable-pitch
  :ensure t
  :hook (org-mode-hook . org-variable-pitch-minor-mode))

;;; bullet

(leaf org-bulletproof
  :ensure t
  :hook (org-mode-hook . org-bulletproof-mode))

(leaf org-bullets
  :ensure t
  :hook (org-mode-hook . org-bullets-mode))
