(leaf vertico
  ;;  :commands vertico--exhibit
  ;; TODO: vertico: either add extensions to load-path or use recipe format
  ;; for flat dir. then again magit does load with ‘lisp’ dir so i don’t know
  :ensure t
  :after orderless prescient
  ;; COMMIT: use prescient-mode
  ;; COMMIT: 'orderless' hard dependency, load after 'orderless'
  :global-minor-mode vertico-mode vertico-prescient-mode
  :custom ((vertico-resize . 'grow-only)
           (vertico-cycle . t)
           (vertico-count . 10)
           (vertico-sort-function . #'vertico-sort-alpha) ;; hist+alpha
           (vertico-grid-max-columns . 10)
           (vertico-grid-lookahead . 9999)
           (vertico-grid-min-columns . 4)
           (vertico-grid-rows . 3)
           (vertico-multiform-categories . '((file grid))))

  :hook ((rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
         (minibuffer-setup-hook . vertico-repeat-save))
  :bind (("M-r" . vertico-repeat)
         (:vertico-map
          ;; vertico-directory
          (("TAB" . my-vertico-insert-unless-tramp)
           ("M-DEL" . vertico-directory-up)
           ("RET" . vertico-directory-enter)
           ("M-d" . vertico-directory-delete-word)
           ("C-d" . vertico-directory-delete-char)))
         ;; minibuffer from outside minibuffer
         ("C-c M-n" . down-from-outside)
         ("C-c M-p" . up-from-outside)
         ("C-c C-M-o" . to-and-fro-minibuffer)
         ("C-c C-g" . my-exit-from-outside))
  :config
  (ignore-error (vertico-multiform-mode))
  ;; todo understand prescient from filtering/sorting from
  ;; https://github.com/minad/vertico/wiki
  ;; adapted from vertico-reverse
  (defun vertico-bottom--display-candidates (lines)
    "Display lines in bottom."
    (move-overlay vertico--candidates-ov (point-min) (point-min))
    (unless (eq vertico-resize t)
      (setq lines (nconc (make-list (max 0 (- vertico-count (length lines))) "\n") lines)))
    (let ((string (apply #'concat lines)))
      (add-face-text-property 0 (length string) 'default 'append string)
      (overlay-put vertico--candidates-ov 'before-string string)
      (overlay-put vertico--candidates-ov 'after-string nil))
    (vertico--resize-window (length lines)))

  (defun vertico-resize--minibuffer ()
    (add-hook 'window-size-change-functions
              (lambda (win)
                (let ((height (window-height win)))
                  (when (/= (1- height) vertico-count)
                    (setq-local vertico-count (1- height))
                    (vertico--exhibit))))
              t t))

  ;; binding this function to tab in vertico-map temporarily disables vertico while
  ;; completing remote paths to restore the shell-like tab-completes-common-prefix
  ;; behavior. this is a usability trade-off to work around a peculiarity in tramp’s
  ;; hostname completion.

  (defun my-vertico-insert-unless-tramp ()
    "Insert current candidate in minibuffer, except for tramp."
    (interactive)
    (if (vertico--remote-p (vertico--candidate))
        (minibuffer-complete)
      (vertico-insert)))

  ;;;; dealing with minibuffer

  (defun down-from-outside ()
    "Move to next candidate in minibuffer, even when minibuffer isn't selected."
    (interactive)
    (with-selected-window (active-minibuffer-window)
      (execute-kbd-macro [down])))

  ;; mine lol
  (defun my-exit-from-outside ()
    "Move to next candidate in minibuffer, even when minibuffer isn't selected."
    (interactive)
    (with-selected-window (active-minibuffer-window)
      (minibuffer-keyboard-quit)))

  (defun up-from-outside ()
    "Move to previous candidate in minibuffer, even when minibuffer isn't selected."
    (interactive)
    (with-selected-window (active-minibuffer-window)
      (execute-kbd-macro [up])))

  (defun to-and-fro-minibuffer ()
    "Go back and forth between minibuffer and other window."
    (interactive)
    (if (window-minibuffer-p (selected-window))
        (select-window (minibuffer-selected-window))
      (select-window (active-minibuffer-window))))

  (defun +embark-live-vertico ()
    "Shrink vertico minibuffer when `embark-live' is active."
    (when-let (win (and (string-prefix-p "*embark live" (buffer-name))
                        (active-minibuffer-window)))
      (with-selected-window win
        (when (and (bound-and-true-p vertico--input)
                   (fboundp 'vertico-multiform-unobtrusive))
          (vertico-multiform-unobtrusive)))))

  ;; when resizing the minibuffer (e.g., via the mouse), adjust the number of
  ;; visible candidates in vertico automatically.
  (advice-add #'vertico--setup :before #'vertico-resize--minibuffer)

  ;; prefix the current candidate with “» “.
  ;; "⋈ "
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "⋈ " 'face 'vertico-current)
                   "  ")
                 cand)))

  ;; automatically shrink vertico for embark-live
  (add-hook 'embark-collect-mode-hook #'+embark-live-vertico)

  ;; input at bottom of completion list
  (advice-add #'vertico--display-candidates
              :override #'vertico-bottom--display-candidates))

(leaf vertico-extras
  ;; TODO: why not lazy-load bind?
  :after vertico
  :require t
  :bind (:vertico-map
         (("C->" . daanturo-vertico-inc-count||height)
          ("C-<" . daanturo-vertico-dec-count||height))))

(leaf vertico-prescient
  :after vertico
  :ensure t
  :global-minor-mode vertico-prescient-mode)

;;custom (vertico-sort-function . #'daanturo-vertico-sort-files))

;; TODO: https://github.com/minad/mini-popup
;; (leaf mini-popup)
