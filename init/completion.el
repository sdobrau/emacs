;;; Main options
(setq-default completions-cycle-threshold t ;; always cycle completions
              ;; vertical completion in columns in minibuffer
              completions-format  'vertical
              ;; below this is cycled
              completion-cycle-threshold nil
              ;; enable indentation+completion using the TAB key.
              ;; `completion-at-point'is often bound to M-TAB.
              tab-always-indent 'complete)


;; COMMIT: add 'prescient' and 'prescient-corfu'

(leaf prescient
  :ensure t
  :global-minor-mode prescient-persist-mode
  :custom (prescient-use-case-folding . 'smart))

;;; Corfu suite

(leaf orderless
  :ensure t
  :after vertico
  :custom ((completion-styles . '(basic substring partial-completion flex))
           (completion-category-overrides
            . '((file (styles basic partial-completion))
                (buffer (styles . (flex))))))
  :config
  ;; Try out host-name completion (with Vertico.)
  (defun basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list
   'completion-styles-alist
   '(basic-remote basic-remote-try-completion basic-remote-all-completions nil))
  (setq completion-styles '(orderless)
        completion-category-overrides '((file (styles basic-remote partial-completion)))))

(leaf corfu
  :ensure t
  :config

  (defun corfu-insert-shell-filter (&optional _)
    "Insert completion candidate and send when inside comint/eshell."
    (when (or (derived-mode-p 'eshell-mode) (derived-mode-p 'comint-mode))
      (lambda ()
        (interactive)
        (corfu-insert)
        ;; `corfu-send-shell' was defined above
        (corfu-send-shell))))

  (defun corfu-just-newline (&optional _)
    "Insert completion candidate and send when inside comint/eshell."
    (interactive "P")
    (corfu-quit)
    (newline))

  ;; SPC as separator
  (setq corfu-separator 32)

  ;; https://github.com/minad/corfu/wiki#same-key-used-for-both-the-separator-and-the-insertion
  ;; highly recommanded to use corfu-separator with "32" (space)
  (define-key corfu-map (kbd "SPC")
              (lambda ()
                (interactive)
                (if current-prefix-arg
                    ;;we suppose that we want leave the word like that, so do a space
                    (progn
                      (corfu-quit)
                      (insert " "))
                  (if (and (= (char-before) corfu-separator)
                           (or
                            ;; check if space, return or nothing after
                            (not (char-after))
                            (= (char-after) ?\s)
                            (= (char-after) ?\n)))
                      (progn
                        (corfu-insert)
                        (insert " "))
                    (corfu-insert-separator)))))

  (defun corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell."
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
      (comint-send-input))))

  (advice-add #'corfu-insert :after #'corfu-send-shell)

  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
         `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  (orderless-define-completion-style orderless-fast
    (orderless-dispatch '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))

  :hook (((emacs-lisp-mode-hook
           sh-mode-hook
           ;; TODO: add in function
           eshell-mode-hook) . corfu-mode)
         (corfu-mode-hook . corfu-history-mode))
  :bind (:corfu-map
         (("TAB" . corfu-next)
          ([tab] . corfu-next)
          ("C-<tab>" . corfu-previous)
          ("M-d" . corfu-show-documentation)
          ("M-l" . corfu-show-location)
          ;; for eshell and etc
          ;; COMMIT: add corfu for eshell
          ("C-n" . corfu-next)
          ("RET" . corfu-just-newline)
          ("C-p" . corfu-previous)
          ("M-RET" . corfu-insert)
          ([backtab] . corfu-previous)))
  :custom ((corfu-cycle . nil) ; enable cycling
           (corfu-auto . t) ; popup auto on delay
           (corfu-preselect-first . t)
           (corfu-separator . ?\s) ; space
           (corfu-min-width . 50)
           (corfu-max-width . 50)
           (corfu-auto-prefix . 1)  ;; COMMIT: now 3
           (corfu-separator . ?\s) ; for orderless comp, use spc?
           (corfu-quit-at-boundary . nil)
           (corfu-quit-no-match . 'separator)
           (corfu-sort-function . #'corfu-sort-length-alpha)
           (corfu-echo-documentation . t)
           (corfu-auto-delay . 3)
           (corfu-scroll-margin . 5)
           (corfu-count . 15)
           (completion-styles . '(orderless-fast initials))
           (completion-category-overrides ((file (styles orderless-fast partial-completion)))))
  :config
  (corfu-popupinfo-mode)
  ;; TODO: consider minibuffer completion for corfu
  ;; (add-to-list 'corfu--frame-parameters `(font . ,my-font))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer)
  (advice-add 'window-configuration-change-hook :before #'corfu--popup-hide))



(leaf corfu-prescient
  :ensure t
  :after corfu
  :global-minor-mode corfu-prescient-mode)

;; TODO: think of key
;; (leaf corfu-candidate-overlay
;;   :quelpa (corfu-candidate-overlay
;;            :fetcher git
;;            :url "https://code.bsdgeek.org/adam/corfu-candidate-overlay.git")
;; :after corfu
;; :config
;; ;; enable corfu-candidate-overlay mode globally
;; ;; this relies on having corfu-auto set to nil
;; (corfu-candidate-overlay-mode +1)
;; ;; bind Ctrl + TAB to trigger the completion popup of corfu
;; (global-set-key (kbd "C-<tab>") 'completion-at-point)
;; ;; bind Ctrl + Shift + Tab to trigger completion of the first candidate
;; ;; (keybing <iso-lefttab> may not work for your keyboard model)
;; (global-set-key (kbd "C-<iso-lefttab>") 'corfu-candidate-overlay-complete-at-point))


;; COMMIT: add corfu-overlay
(leaf corfu-popup)

(leaf popon)

;; TODO: if window-system...
(leaf corfu-terminal
  :ensure t
  :config
  (unless (eq (window-system) 'x) (corfu-terminal-mode +1)))


;; additional completions
(leaf cape
  :ensure t
  :bind (("C-x C-p" . nil) ;; mark-page
         ("C-x C-p p" . completion-at-point) ;; capf
         ("C-x C-p t" . complete-tag)        ;; etags
         ("C-x C-p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-x C-p f" . cape-file)
         ("C-x C-p k" . cape-keyword)
         ("C-x C-p s" . cape-symbol)
         ("C-x C-p a" . cape-abbrev)
         ("C-x C-p i" . cape-ispell)
         ("C-x C-p l" . cape-line)
         ("C-x C-p w" . cape-dict))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

;;; Others
(leaf abbrev
  :hook (text-mode . abbrev-mode)
  :custom (abbrev-suggest . t) ;; if typing expansion instead of existing abbrv
  :bind (:abbrev-map
         (("?" . abbrev-suggest-show-report))))


(leaf hippie-exp
  :ensure nil
  :bind* (("M-/" . hippie-expand)
          ("s-?" . hippie-expand-line))
  :hook ((emacs-lisp-mode-hook ielm-mode-hook) .
         (lambda ()
           (setq-local
            hippie-expand-try-functions-list
            (append '(try-complete-lisp-symbol-partially
                      try-complete-lisp-symbol
                      hippie-expand-try-functions-list))))))
