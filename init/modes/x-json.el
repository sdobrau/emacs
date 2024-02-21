(leaf json-mode
  :ensure t
  :after json-reformat json-snatcher
  :mode (("\\.bowerrc$" . json-mode)
         ("\\.jshintrc$" . json-mode)
         ("\\.json_schema$" . json-mode)
         ("\\.json\\'" . json-mode))
  :hook (json-mode-hook . (lambda () (show-paren-mode -1))))

(leaf json-navigator
  :ensure t
  :after hierarchy)

(leaf json-navigator-extras
  :require t
  :after json-navigator
  :bind ((:js-mode-map
          ("C-c s-j n" . spacemacs/json-navigator-dwim))
         (:js2-mode-map
          ("C-c s-j n" . spacemacs/json-navigator-dwim))
         (:json-mode-map
          ("C-c s-j n" . spacemacs/json-navigator-dwim))
         (:json-navigator-mode-map
          ("p" . widget-backward)
          ("n" . widget-forward)
          ("o" . widget-button-press))))

(leaf json-snatcher
  :ensure t)

(leaf json-snatcher-extras
  :after json-snatcher
  :bind ((:js-mode-map
          (("C-c s-j p j" . jsons-print-path)
           ("C-c s-j p j" . jsons-print-path-jq)
           ("C-c s-j p p" . jsons-print-path-python))
          (:js2-mode-map
           (("C-c s-j p j" . jsons-print-path)
            ("C-c s-j p j" . jsons-print-path-jq)
            ("C-c s-j p p" . jsons-print-path-python))))))

;; COMMIT: remove json-pointer

(leaf json
  :after js2-mode
  :require t
  :bind ((:js-mode-map
          ("C-c s-j f" . json-pretty-print-buffer)
          ("C-c s-j f" . json-pretty-print-buffer-ordered)
          ("C-c s-j M-f" . json-pretty-print)
          ("C-c s-j M-f" . json-pretty-print-ordered))
         (:js2-mode-map
          (("C-c s-j f" . json-pretty-print-buffer)
           ("C-c s-j f" . json-pretty-print-buffer-ordered)
           ("C-c s-j M-f" . json-pretty-print)
           ("C-c s-j M-f" . json-pretty-print-ordered)))))

(leaf flymake-json
  :ensure t
  :hook ((json-mode-hook . flymake-json-load)
         ((js-mode-hook
           js2-mode-hook) . flymake-json-maybe-load)))

(leaf counsel-jq
  :ensure t
  :require t)

(leaf counsel-jq-extras
  :after counsel-jq yaml-mode
  :bind ((:js-mode-map
          ("C-c s-j q" . counsel-jq-jq))
         (:js2-mode-map
          ("C-c s-j q" . counsel-jq-jq))
                                        ;(:json-mode-map
                                        ; ("C-c s-j q" . counsel-jq-jq)) todo: why
         (:yaml-mode-map
          ("C-c y q" . counsel-jq-yq))))
