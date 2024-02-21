;; Don't ask for confirmation when reverting a buffer
(setq-default revert-without-query '(".*")
              whitespace-line-column 120
              require-final-newline t)

(leaf ibuffer
  :commands ibuffer
  :hook (ibuffer-mode-hook .
         (lambda () (ibuffer-switch-to-saved-filter-groups "default")))
  :custom ((ibuffer-default-display-maybe-show-predicates . nil)
           (ibuffer-expert . t) ;; don’t warn of dangerous operations
           ;; don’t show empty filter groups
           (ibuffer-show-empty-filter-groups . nil)
           (ibuffer-shrink-to-minimum-size . t)
           (ibuffer-user-other-window . t)
           ;; redguardtoo
           (ibuffer-saved-filter-groups
            . '(("default"
                 ("code" (or (mode . emacs-lisp-mode)
                             (mode . cperl-mode)
                             (mode . c-mode)
                             (mode . java-mode)
                             (mode . idl-mode)
                             (mode . web-mode)
                             (mode . lisp-mode)
                             (mode . js2-mode)
                             (mode . c++-mode)
                             (mode . lua-mode)
                             (mode . cmake-mode)
                             (mode . ruby-mode)
                             (mode . css-mode)
                             (mode . objc-mode)
                             (mode . sql-mode)
                             (mode . python-mode)
                             (mode . php-mode)
                             (mode . sh-mode)
                             (mode . json-mode)
                             (mode . scala-mode)
                             (mode . go-mode)
                             (mode . erlang-mode)))

                 ("dired" (or (mode . dired-mode)
                              (mode . sr-mode)))

                 ("erc" (mode . erc-mode))

                 ("planner" (or (name . "^\\*calendar\\*$")
                                (name . "^diary$")
                                (mode . muse-mode)
                                (mode . org-mode)
                                (mode . org-agenda-mode)))

                 ("emacs" (or (name . "^\\*scratch\\*$")
                              (name . "^\\*messages\\*$")))

                 ("gnus" (or (mode . message-mode)
                             (mode . bbdb-mode)
                             (mode . mail-mode)
                             (mode . gnus-group-mode)
                             (mode . gnus-summary-mode)
                             (mode . gnus-article-mode)
                             (name . "^\\.bbdb$")
                             (name . "^\\.newsrc-dribble")))))))

  :bind (("C-x C-b" . ibuffer)
         (:ibuffer-mode-map
          (("M-o" . nil)))) ;; ibuffer-visit-buffer-1-window conflict with M-o

  :config
  (setq ibuffer-formats '((mark modified read-only
                           " "
                           (name 18 18 :left :elide)
                           " "
                           (size 10 -1 :right)
                           " "
                           (mode 16 16 :left :elide)
                           " " filename)
                          (mark " " (name 16 -1) " " filename))))

(setq large-file-warning-threshold 500000000)

(leaf auto-revert-mode
  :custom ((auto-revert-interval . 999999999) ;;  sec between checks
     (auto-revert-verbose . nil))
  :config
  (auto-revert-mode -1)
  (global-auto-revert-mode -1))

(leaf uniquify
  :custom (;; file/dirb/dirc + file/dirc/dird =
     ;; file/dirb/dirc/,
     ;; file/dirc/dird/ buffer names
           (uniquify-buffer-name-style . 'forward)
           ;; strip common dir suffix from unique buffers
           (uniquify-strip-common-suffix . t)
           ;; rename buffers after uniq buffer killed
           (uniquify-after-kill-buffer-p . t)
           ;; buffer names which should be uniquified
           (uniquify-ignore-buffers-re . "^\\*")
           ;; separator for uniquified buffers
           (uniquify-separator . "/")))

(leaf diminish-buffer ;;todo
  :ensure t
  :disabled t
  :global-minor-mode diminish-buffer-mode
  :custom (diminish-buffer-list . '("[*]helm")))

;; helpful functions:
;;
;; =C-x b=: switch to a buffer, listing only buffers with the same mode as the
;;   current buffer. (=jao=)
;;
;; =C-x o=: switch to the previous buffer of this window. repeated
;;   invocations return to the initial buffer. (=malb=)
;;
;; =C-x k= kills the buffer by default, =c-u c-x -k= prompts for a buffer.

(leaf buffer-extras
  :require t
  :bind (("C-x k" . my-kill-this-buffer)
         ("C-x C-M-s" . daanturo-save-unsaved-buffers-with-files)
         ("C-x M-s" . daanturo-save-buffer-no-hook)
         ("C-x M-b" . switch-to-buffer-other-window)
         ;; fast switch
         ("C-x B" . jao-buffer-same-mode)
         ("C-x M-n" . my-next-buffer-of-same-current-mode)
         ("C-x M-p" . my-previous-buffer-of-same-current-mode)
         ;; switch to to other buffer of same mode
         ("C-x s-o" . my-switch-to-other-buffer-same-mode)
         ;; completing-read switch
         ;; general
         ("C-x l" . previous-buffer)
         ("C-x ;" . next-buffer)
         ("C-x o" . malb/switch-to-previous-buffer)
         ;; killing
         ;; ("C-x k" . kill-buffer-dwim) moved to popper-extras
         ("C-x M-k" . my-kill-all-buffers-with-current-mode)
         ("C-x C-M-k" . my-kill-buffer-and-close-window)))

(leaf nasy-buffers
  :require t)

;; TODO: what?
(leaf b)

;; List-oriented buffer operations
(leaf m-buffer
  :ensure t
  :require t)

;; * Formatting

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Unobtrusively remove whitespace
(leaf ws-butler
  :ensure t
  :hook ((prog-mode-hook text-mode-hook) . ws-butler-mode))

(leaf buffer-sets
  :ensure t
  :require t
  :global-minor-mode buffer-sets-mode)

;; ;; TODO: buffer narrowing+ buffer-set grouping ..
;; ;; OR: via marginalia (see consult-gh)
;; (setf buffer-sets
;;       '(("mailman3dev" .
;;   '(:files "/ssh:isdccaemrs@mailman3dev.ad.ucl.ac.uk|sudo:root@mailman3dev.ad.ucl.ac.uk:/etc/mailman3/settings.py" ))))
