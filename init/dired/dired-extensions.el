;; ugh TODO: change
(leaf dired-hacks
  ;; COMMIT: fix dired-hacks
  :quelpa (dired-hacks
           :fetcher github
           :repo "Fuco1/dired-hacks")
  :commands dired
  :config
  (leaf dired-filter
    :require t
    :custom ((dired-filter-prefix . "/")
             (dired-filter-mark-prefix . "M-/")
             (dired-filter-revert . t))
    :hook ((dired-mode-hook . dired-filter-mode)
           (dired-sidebar-mode-hook . (lambda () (progn (dired-filter-by-git-ignored)
                                                   (setq-local dired-filter-header-line-format nil)))))))
(leaf dired-subtree
  :bind (:dired-mode-map
         (("C-M-b" . dired-subtree-insert)
          ("C-M-n" . dired-subtree-next-sibling)
          ("C-M-p" . dired-subtree-previous-sibling)
          ("C-M-u" . dired-subtree-up)
          ("C-M-d" . dired-subtree-down)
          ("M-RET" . dired-subtree-mark-subtree)
          ("TAB" . nil)
          ("C-x M-RET" . dired-subtree-unmark-subtree)
          ("C-c l" . dired-subtree-with-subtree)
          ;; cant tab at the moment, tab-for-indent
          ("TAB" . dired-subtree-cycle)
          ("C-c M-d" . dired-subtree-remove)))
  :custom (dired-subtree-use-backgrounds . nil))

(leaf dired-map) ;; TODO: where is dired-map ?

(leaf dired-single
  :quelpa dired-single
  :bind ((:dired-mode-map
          (([remap dired-find-file] . dired-single-buffer)))
         (:dired-mode-map :package dired+
                          ([remap diredp-up-directory] . dired-single-up-directory))))

;; TODO: reimplement?
(leaf dired-aux
  :require t
  :custom ((dired-vc-rename-file . t)
           (dired-dwim-target . #'dired-dwim-target-next-visible)
           (dired-create-destination-dirs . 'ask)
           ;; if point on file, search file, otherwise whole buffer
           (dired-isearch-filenames . 'dwim)
           (dired-create-destination-dirs . t)
           ;; rename file using vc mechanism
           (dired-vc-rename-file . t)
           (dired-do-revert-buffer . (lambda (dir) (not (file-remote-p dir))))))

(leaf dired-posframe
  :ensure t
  :after posframe
  :bind (:dired-mode-map
         ("C-*" . dired-posframe-show)))

;; TODO
(leaf dired-x
  :hook (dired-mode-hook . dired-omit-mode)
  :bind (("s-\\" . dired-jump-other-window)
         (:dired-mode-map
          ((")" . dired-omit-mode))))
  :custom ((dired-clean-up-buffers-too . nil)
           (dired-clean-confirm-killing-deleted-buffers . nil)))

(leaf fd-dired
  :bind (:dired-mode-map
         (("/" . fd-dired)))
  :custom (fd-dired-generate-random-buffer . t))

;; Takes a while to run.
(leaf dired-du
  :ensure t
  :bind (:dired-mode-map
         (("s" . dired-du-mode)))
  :custom (;; don’t map c-x m-r to dired-du in dired-mode-map
           dired-du-bind-mode . nil))

;; TODO: dired-guess

;; implement + abstract function to accept emacs func not just shell cmd
;; https://github.com/abo-abo/dired-guess/blob/master/dired-guess.el

;; dired+: more extra dired functions
;; At the moment it doesn’t work. Later.
;;
;; dired-internal-do-deletions: Wrong type argument: integer-or-marker-p, nil
;;
;; dired-internal-do-deletions file-alist arg trash)= is expecting an =arg= and
;; does not get any.

(leaf dired+
  :quelpa dired+
  :custom (diredp-auto-focus-frame-for-thumbnail-tooltip-flag . t))

;; TODO diredc: extensions. _navigation history_
;; https://github.com/boruch-baum/emacs-diredc

(leaf dired-single
  :quelpa dired-single
  :bind ((:dired-mode-map
          (([remap dired-find-file] . dired-single-buffer)))
         (:dired-mode-map
          :package dired+
          ([remap diredp-up-directory] . dired-single-up-directory))))

(leaf dired-aux
  :require t
  :custom ((dired-vc-rename-file . t)
           (dired-dwim-target . #'dired-dwim-target-next-visible)
           (dired-create-destination-dirs . 'ask)
           ;; if point on file, search file, otherwise whole buffer
           (dired-isearch-filenames . 'dwim)
           (dired-create-destination-dirs . t)
           ;; rename file using vc mechanism
           (dired-vc-rename-file . t)
           (dired-do-revert-buffer . (lambda (dir) (not
                                               (file-remote-p dir))))))

(leaf dired-posframe
  :ensure t
  :after posframe
  :bind (:dired-mode-map
         ("C-*" . dired-posframe-show)))

;; async copy and move
(leaf dired-async
  ;;:ensure t TODO ? FIXME
  :hook (dired-mode-hook . dired-async-mode))

(leaf grep-dired
  :quelpa (grep-dired
           :fetcher github
           :repo "manateelazycat/grep-dired")
  ;; COMMIT: change keybinding
  :bind ("M-^" . grep-dired))

;; project
(leaf dired-sidebar
  :ensure t
  :commands dired-sidebar-toggle-sidebar
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :bind (:dired-sidebar-mode-map
         (("C-M-u" . dired-subtree-up)
          ("g" . revert-buffer)))
  :custom ((dired-sidebar-theme . "ascii")
           (dired-sidear-subtree-line-prefix . "__")
           (dired-sidebar-width . 30)
           ;; COMMIT: dired-sidebar-should-follow-file is t
           (dired-sidebar-should-follow-file . t))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands))

;; COMMIT: explicit dired-k2
(leaf dired-k2
  :ensure t
  :after dired-sidebar
  :hook ((dired-mode-hook
          dired-sidebar-mode-hook) . dired-k2))
