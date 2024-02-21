(leaf elfeed
  :ensure t
  :require t ;; to trigger elfeed-org
  :bind (("C-x w" . elfeed)
         (:elfeed-search-mode-map
          (("g" . elfeed-update-feed))))

  :init (setf url-queue-timeout 60)
  :custom ((elfeed-search-title-max-width . 80)
           (elfeed-search-title-min-width . 80)
           (elfeed-search-print-entry-function
            . #'elfeed-search-print-entry--default)
           (elfeed-curl-max-connections . 32))

  :config
  (set-face-attribute 'elfeed-search-unread-title-face nil :foreground (color-darken-name (face-attribute 'default :foreground nil) 1.8))

  (setq elfeed-db-directory
  (no-littering-expand-var-file-name "elfeed/")
  elfeed-score-score-file
  (no-littering-expand-var-file-name "elfeed/scores")
  elfeed-enclosure-default-dir
  (no-littering-expand-var-file-name "elfeed/enclosures/")))

(leaf elfeed-extras
  :after elfeed
  :bind ((:elfeed-show-mode-map
          ((kbd "b") . bjm/elfeed-show-visit-gui)
          ((kbd "RET") . elfeed-show-eww-open))
         (:elfeed-search-mode-map
          ([remap elfeed-search-show-entry] . ic/elfeed-open-in-eww))))

(leaf elfeed-org
  :ensure t
  :require t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files `(,(no-littering-expand-etc-file-name "elfeed/elfeed.org"))))

(leaf elfeed-summary
  :ensure t)
