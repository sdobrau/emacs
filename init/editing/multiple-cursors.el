(leaf multiple-cursors
  :ensure t
  ;; TODO: call-last-macro-on-each-multiple-cursor
  ;; TODO: function to make a temporary buffer as large as bottom-most cursor,;
  ;; then the macro can operate on that buffer... ?
  :bind (("M-c" . nil) ;; upcase-word
         ("M-s m l" . mc/edit-lines)
         ;;
         ("C-M-<" . mc/unmark-previous-like-this)
         ("C-M->" . mc/unmark-next-like-this)
         ("M-c m w p" . mc/mark-previous-like-this-word)
         ("M-c m p p" . mc/mark-previous-like-this-symbol)
         ("M-s m w n" . mc/mark-next-like-this-word)
         ("M-s m s n"  . mc/mark-next-like-this-symbol)
         ("M-s m a a" . mc/mark-all-like-this)
         ("M-s m w a" . mc/mark-all-words-like-this)
         ("M-s m s a" . mc/mark-all-symbols-like-this)
         (:mc/keymap
          (("M-n" . mc/cycle-forward)
           ("M-n" . mc/cycle-backward)
           ("M-<mouse-1>" . mc/add-cursor-on-click)
           ("<return>" . nil)
           ("<escape>" . nil))))
  :custom (mc/max-cursors . 6000)
  :config
  (add-to-list 'mc/unsupported-minor-modes 'corfu-mode))

(leaf daanturo-multiple-cursors
  :after multiple-cursors daanturo-core-macros daanturo-core-functions
  ;; FIXME: not loading
  :bind ("M-c l" . daanturo-mc/edit-lines-dwim)
  :hook ((multiple-cursors-mode-enabled-hook
          . daanturo-mc/resume-paused-cursors-when-indicated)
         (multiple-cursors-mode-hook
          . daanturo-toggle-corfu-tick-advice-mc-compat--a))
  :config
  (advice-add #'mc/keyboard-quit
              :before
              #'daanturo-mc/save-cursor-when-not-region-and-not-prefix-args--a)
  (daanturo-add-advice/s #'(corfu-insert corfu-complete)
    :around
    #'daanturo-corfu-completions-for-mc--a)
  (daanturo-add-advice/s #'(consult-completion-in-region)
    :around
    #'daanturo-completion-by-minibuffer-for-mc-a))
;; (daanturo-bind [remap +multiple-cursors/evil-mc-toggle-cursor-here] #'daanturo-mc/toggle-cursor-at-point)
;; (daanturo-bind [remap +multiple-cursors/evil-mc-toggle-cursors] #'daanturo-mc/toggle-pausing-cursors)
;; (daanturo-bind [remap evil-mc-undo-all-cursors] #'daanturo-mc/indicate-paused-cursors-mode)
;; (global-unset-key (kbd "m-<down-mouse-1>"))))
