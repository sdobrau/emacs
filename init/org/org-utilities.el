;; For non- buffers.

(leaf outshine
  :ensure t
  :after outline
  :commands outshine-mode
  :custom ((outshine-cycle-silently . t)
           (outshine-fontify-whole-heading-line . t)
           (outshine-max-level . 15))
  :bind (:outshine-mode-map
         (("M-g o" . consult-outline)
          ("<s-return>" . outshine-insert-heading)
          ("C-c RET" . outshine-kbd-M-RET) ; conflict with ansible RET
          ("C-i" . nil) ; outshine cycle-buffer conflict with tempel-expand
          ("M-s-f" . outline-forward-same-level)
          ("M-s-b" . outline-backward-same-level)
          ("M-s-n" . outline-next-visible-heading)
          ("M-s-p" . outline-previous-visible-heading)
          ("M-s-u" . outline-up-heading)
          ("M-s--" . outline-forward-element)
          ("M-s-8" . outline-backward-element)
          ("M-s-0" . outline-up-element)
          ("M-s-9" . outline-down-element)
          ("C-x n s" . org-narrow-to-subtree)
          ("C-x n w" . widen))))

(leaf org-fs-tree
  :quelpa (org-fs-tree
           :fetcher github
           :repo "ScriptDevil/org-fs-tree")
  :require t)

;; COMMIT: remove stupid comment
;; (leaf json-to-org-table TODO: fix 'package-desc'
;;   :quelpa (json-to-org-table
;;            :fetcher github
;;            :repo "Noonker/json-to-org-table"))

;; COMMIT: remove duplicate 'org-fs-tree' and 'org-json-to-org-table'

(leaf org-listcruncher
  :ensure t)

(leaf pandoc-mode
  :ensure t
  :after hydra
  ;;:leaf-defvar pandoc-mode-map
  ;; TODO: fix pandoc-mode-map not defined
  ;;:bind-keymap ("C-c p" . pandoc-mode-map)

  :hook ((markdown-mode-hook
          doc-view-minor-mode-hook
          org-mode-hook
          web-mode-hook) . pandoc-mode))

;; TEMPO
;; (leaf org-auto-tangle
;;   :after org
;;   :ensure t
;;   :hook (org-mode-hook . org-auto-tangle-mode)
;;   :config (add-to-list 'org-auto-tangle-babel-safelist my-init-org-file))

;; Inherit org features in =eww=, =nov=, ,=mu4e= and =dash-docs= buffers. This
;; also means that my org-mode keybindings can be applied here effortlessly.

;; I place this here, and load it before =nov= and =eww=. =shrface-trial= doc:
;; ‘need to be called once before loading eww, nov, dash-docs, mu4e, after
;; shr. shrface-tag-code is experimental, sometimes eww will hangup’

(leaf shrface
  :ensure t
  :after eww
  :require t
  :hook (((eww-after-render-hook
           nov-mode-hook) . shrface-mode)
         (shrface-mode-hook
          . (lambda () (progn (org-indent-mode -1)
                         (setq-local visual-fill-column-width 85)))))
  :bind ((:eww-mode-map
          (("<backtab>" . nil))) ; previous link eww
         (:shrface-mode-map
          (("M-g i" . shrface-headline-counsel)
           ("M-g o" . shrface-headline-counsel) ;; lol
           ("M-g l" . shrface-links-counsel)
           ("M-h" . mark-paragraph) ;; originally org-mark-element
           ("C-x n s" . org-narrow-to-subtree)
           ("C-x n w" . widen)
           ("C-c C-n" . my-org-next-heading-keep-narrow)
           ("C-c C-p" . my-org-prev-heading-keep-narrow)
           ("C-M-f" . org-forward-heading-same-level)
           ("C-M-b" . org-backward-heading-same-level)
           ("C-M-n" . org-next-visible-heading)
           ("C-M-p" . org-previous-visible-heading)
           ("C-M-u" . outline-up-heading)
           ("M-s-n" . org-forward-element)
           ("M-s-p" . org-backward-element)
           ("M-s-u" . org-up-element)
           ("M-s-d" . org-down-element)
           ("TAB" . shrface-outline-cycle)
           ("<backtab>" . shrface-outline-cycle-buffer)
           ("C-c C-e o" . shrface-html-export-as-org))))
  :custom ((shrface-bullets-bullet-list . '("*" "**" "***" "****"))
           (shrface-paragraph-fill-column . 80))
  :config
  (require 'eww-shrface-lib)
  (shrface-basic)
  (shrface-trial)
  (setq shrface-href-versatile t)
  (set-face-attribute 'shrface-href-face nil
                      :inherit 'variable-pitch
                      :foreground (face-attribute 'org-level-2 :foreground)))

;;; PDF/DOCUMENTS

;; TODO: parametrize document+notes frame/buffer title..
;; COMMIT: add org-noter
(leaf org-noter
  :ensure t
  :after pdf-tools
  :custom ((org-noter-arrow-delay . 0.1)
           (org-noter-separate-notes-from-heading . t) ;; extra empty line
           (org-noter-notes-window-location . 'other-frame)))

;; COMMIT: add org-inline-pdf
(leaf org-inline-pdf
  :ensure t
  :hook (orgmode-hook . org-inline-pdf-mode))

;;; MINDMAP

;; ;; TODO: ugh:
;; (require 'org-brain)
;; ;; COMMIT: add org-brain and co but disable
;; (leaf org-brain
;;   :ensure t
;;   :require t
;;   :preface
;;   (setq org-brain-path (concat my-org-directory "/brain"))
;;   (defcustom my-default-org-brain-file "brain"
;;     "Default file used for the package 'org-brain'.")
;;   :hook (org-brain-visualize-mode-hook . org-brain-polymode)
;;   :bind-keymap ("C-c b" . org-brain-prefix-map)
;;   :bind (:org-brain-visualize-mode-map
;;          (("L" . org-brain-cliplink-resource)))

;;   :custom ((org-id-track-globally . t)
;;            (org-brain-visualize-default-choices . 'all)
;;            (org-brain-title-max-length . 12)
;;            (org-brain-file-entries-use-title . nil)
;;            (org-brain-include-file-entries . nil)
;;            ;; (org-brain-show-resources . nil)
;;            ;; (org-brain-show-text . nil)
;;            (org-brain-file-entries-use-title . nil)
;;            (org-brain-headline-entry-name-format-string . "%2$s") ;; only headl
;;            (org-brain-default-file-parent . my-default-org-brain-file))
;;   :config

;;
;; ORG EXPIRY CONFIG
;;

;; (require 'org-expiry)
;; (setq org-expiry-inactive-timestamps t)
;; (defun org-expiry-created-comp (a b)
;;   "Compare `org-expiry-created-property-name' properties of A and B."
;;   (let ((ta (ignore-errors
;;               (org-time-string-to-seconds
;;                (org-entry-get (get-text-property 0 'org-marker a)
;;                               org-expiry-created-property-name))))
;;         (tb (ignore-errors
;;               (org-time-string-to-seconds
;;                (org-entry-get (get-text-property 0 'org-marker b)
;;                               org-expiry-created-property-name)))))
;;     (cond ((if ta (and tb (< ta tb)) tb) -1)
;;           ((if tb (and ta (< tb ta)) ta) +1))))

;; ;; Add CREATED property when adding a new org-brain headline entry
;; (add-hook 'org-brain-new-entry-hook #'org-expiry-insert-created)

;; ;; Finally add a function which lets us watch the entries chronologically
;; (defun org-brain-timeline ()
;;   "List all org-brain headlines in chronological order."
;;   (interactive)
;;   (let ((org-agenda-files (org-brain-files))
;;         (org-agenda-cmp-user-defined #'org-expiry-created-comp)
;;         (org-agenda-sorting-strategy '(user-defined-down)))
;;     (org-tags-view nil (format "+%s>\"\"" org-expiry-created-property-name))))

;;
;; OTHER
;;

;; ;; https://github.com/Kungsgeten/org-brain/issues/384
;; (define-polymode org-brain-polymode
;;   :hostmode 'org-brain-poly-hostmode
;;   :innermodes '(org-brain-poly-innermode)
;;   (setq-local polymode-move-these-vars-from-old-buffer
;;               (delq 'buffer-read-only polymode-move-these-vars-from-old-buffer)))

;; ;; TODO:
;; (defun my-org-brain-insert-brain-link ()
;;   (interactive)
;;   (org-insert-link))
;; ;; TODO: insert current eww link as resource for node
;; ;; ^ C-u makes a child node first and adds to that

;;   (defun org-brain-cliplink-resource ()
;;     "Add a URL from the clipboard as an org-brain resource.
;; Suggest the URL title as a description for resource."
;;     (interactive)
;;     (let ((url (org-cliplink-clipboard-content)))
;;       (org-brain-add-resource
;;        url
;;        (org-cliplink-retrieve-title-synchronously url)
;;        nil))) ;; nil to not prompt

;; (setq-default org-id-locations-file
;;               (concat my-org-directory "/.org-id-locations"))
;; (add-hook 'org-mode-hook #'
;;           (lambda () (add-hook 'before-save-hook
;;                           #'org-brain-ensure-ids-in-buffer 0 t)))
;; ;; org-capture
;; (push '("b" "Brain" plain (function org-brain-goto-end)
;;         "* %i%?" :empty-lines 1)
;;       org-capture-templates))

;; DEBUG
;; (leaf polymode ;; Allows you to edit entries directly from org-brain-visualize
;;   :ensure t
;;   :require t)

;; COMMIT: add local and load 'org-expiry' from file in packages/other
;; for 'org-brain'
;; TODO: use sourcehut https://git.sr.ht/~bzg/org-contrib

;; (leaf org-expiry
;;   :ensure nil)

(leaf org-bookmark-heading
  :ensure t
  :require t
  :custom (org-bookmark-jump-indirect . t))
