;; TODO:
;; https://codeberg.org/ideasman42/emacs-bookmark-in-project

(leaf bookmark
  ;; NOTE:
  ;; COMMIT: add ensure nil to bookmark
  :ensure nil
  :bind ("C-x r C-b" . bookmark-jump)
  :custom ((bookmark-fontify . nil)
           ;; save bookmark file whenever bookmarks are modified
           (bookmark-save-flag . 1)
           (bookmark-version-control . t)
           ;; COMMIT: bookmark-automatically-show-annotations to t
           (bookmark-automatically-show-annotations . t))
  :config
  (setq bookmark-default-file
        ;; COMMIT: change to bookmark-default.el in line with default expected
        ;; behaviour
        (no-littering-expand-var-file-name "bookmark-default.el")))

;; TODO: https://github.com/joodland/bm/issues/45
(leaf bm
  :ensure t
  :require t
  :init (setq bm-restore-repository-on-load t) ;; restore on load b4 loading bm
  :custom ((bm-marker . 'bm-marker-right)
           (bm-cycle-all-buffers . t)) ;; cross-buffer next
  :config

  (setq bm-repository-file
        (no-littering-expand-var-file-name "bm-repository")) ;; where to store

  (setq-default bm-buffer-persistence t) ;; save bookmarks

  :hook ((after-init-hook . bm-repository-load) ;; load on start up
         (kill-buffer-hook . bm-buffer-save)
         (kill-emacs-hook . (lambda nil (bm-buffer-save-all) (bm-repository-save)))
         (after-save-hook . bm-buffer-save)

         ;; restoring

         (find-file-hooks . bm-buffer-restore)
         (after-revert-hook . bm-buffer-restore)

         ;; https://www.manueluberti.eu//emacs/2020/03/19/lockdown-beam-bm/
         ;; ^ magit-pre-refresh-hook for magit and not vc-before-checkin-hook
         ;; todo:
         ;; https://github.com/soft/emacs.d/blob/master/init/init-bookmarks.el#l58:l155
         (magit-pre-refresh-hook . bm-buffer-save))

  ;; COMMIT: more comfortable bindings in line with native bookmark feature for
  ;; 'bm'
  :bind (("C-x r '" . bm-toggle)
         ("C-x r M-'" . bm-show)
         ("C-x r [" . bm-previous)
         ("C-x r ]" . bm-next)
         ("C-x r C-'" . bm-bookmark-annotate)))
