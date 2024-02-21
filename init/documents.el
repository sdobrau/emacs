;; TODO: recoll

(leaf pdf-outline
  :hook (pdf-view-mode-hook . pdf-outline-imenu-enable)
  :bind (:pdf-view-mode-map
         (("M-g o" . pdf-outline))))

(leaf tablist
  :ensure t
  :hook (tabulated-list-mode-hook . tablist-mode))

;; COMMIT: refactor pdf-tools. epdfinfo and lisp files managed in script issues
;; with getting it installed via normal package-install.
;;
;; ⛔ Warning (tar): Extracted
;; '/home/strangepr0gram/.emacs.d/packages/elpa/pdf-tools-1.1.0/README', a link,
;; as a normal file

;; TODO: tweak keybindings
(leaf pdf-tools
  ;; no ensure, managed via script
  :if (window-system) ;; would be useless without X
  :require (pdf-tools
            pdf-view
            pdf-misc
            pdf-occur
            pdf-util
            pdf-annot
            pdf-history
            pdf-info
            pdf-isearch
            pdf-links)

  :mode ("\\.pdf\\'" . pdf-tools-install)
  :preface
  (defun my-pdf-view-set-midnight-colors ()
    (interactive)
    (setq pdf-view-midnight-colors
          `(,(color-darken-name
              (face-attribute 'default :foreground)
              0.001)
            .
            ,(color-lighten-name
              (face-attribute 'default :background)
              0.001))))

  :bind (:pdf-view-mode-map
         (("C-c l" . org-store-link)))
  ;; :bind (:pdf-history-minor-mode-map TODO: fix otherwise gives undefined
  ;;        (("l" . pdf-history-backward)
  ;;         (";" . pdf-history-forward)))

  :hook ((pdf-view-after-change-page-hook . pdf-view-midnight-minor-mode)
         ;;(pdf-view-mode-hook . pdf-loader-install)
         ;;(pdf-view-mode-hook . pdf-view-midnight-minor-mode)
         (pdf-view-mode-hook . my-pdf-view-set-midnight-colors))
  ;; lol

  :custom ((pdf-tools-enabled-modes
            .
            '(;; keep history of previously visited pages
              pdf-history-minor-mode
              pdf-isearch-minor-mode ; can isearch
              pdf-links-minor-mode ; can find links
              pdf-outline-minor-mode ; can do outline
              ;; show size in mode-line
              ;; pdf-misc-size-indication-minor-mode
              ;; pdf-occur-global-minor-mode
              pdf-annot-minor-mode
              pdf-view-midnight-minor-mode
              pdf-view-auto-slice-minor-mode
              pdf-virtual-global-minor-mode))
           (pdf-view-display-size . 'fit-height)
           (pdf-view-continuous . t)
           (pdf-view-use-dedicated-register . nil)
           (pdf-view-max-image-width . 1080)
           (pdf-outline-imenu-use-flat-menus . t)
           (pdf-view-display-size . 'fit-page)
           (pdf-view-use-scaling . t))
  :config
  (pdf-loader-install :no-query)
  (set-face-attribute 'pdf-links-read-link nil :background
                      (face-attribute 'mode-line :background))
  (set-face-attribute 'pdf-links-read-link nil :foreground (face-attribute 'mode-line :foreground)))

;; org support
(leaf org-pdftools
  :ensure t
  :hook (org-mode-hook . org-pdftools-setup-link))

(leaf org-noter-pdftools
  :ensure t
  :require t
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freepointer-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

;; COMMIT
;; TODO: install and test
(leaf pdfgrep
  :if (window-system)
  :ensure t
  :commands pdfgrep
  :custom (pdfgrep-buffer-name))

(leaf nov
  :ensure t
  :after shrface
  :bind ((:nov-mode-map
          (("M-n" . nov-next-document)
           ("M-p" . nov-previous-document)
           ("n" . shr-next-link)
           ("p" . shr-previous-link)
           ;; TODO: forward/back word C-M-f C-M-b org
           ("l" . nov-history-back)
           (";" . nov-history-forward)
           ([tab] . shrface-outline-cycle)
           ("TAB" . shrface-outline-cycle)
           ("C-t" . shrface-toggle-bullets)
           ("C-j" . shrface-next-headline)
           ("C-k" . shrface-previous-headline)
           ("a" . nil) ;; nov-reopen-as-archive
           ("M-l" . shrface-links-consult)
           ("M-g i" . consult-imenu)))
         (:nov-button-map
          (("M-n" . nov-next-document)
           ("M-p" . nov-previous-document)
           ("n" . shr-next-link)
           ("p" . shr-previous-link))))
  :custom (nov-text-width . nil)
  :mode "\\.epub\\'"
  :config
  (setq nov-shr-rendering-functions (append
                                     nov-shr-rendering-functions
                                     shr-external-rendering-functions)
        nov-header-line-format ""))
