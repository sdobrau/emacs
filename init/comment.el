;; COMMIT: remove prot-comment-comment-dwim
(leaf comment-extras
  :bind (("C-M-;" . comment-box)
         ("C-c M-;" . hide/show-comments-toggle)
         ;; ("C-c M-;" . )
         ("C-x M-;" . daanturo-transpose-line-and-swap-comment-status)
         (:prog-mode-map
          (("M-RET" . nasyxx/newline-indent-and-continue-comments-a)))))

(setq byte-compile-dynamic-docstrings t)

;; Comments saved in project-root .evc or customizable global file.
(leaf virtual-comment
  :quelpa virtual-comment
  :hook (find-file-hook . virtual-comment-mode)
  :bind (:prog-mode-map
         (("C-c M-;" . virtual-comment-make)
          ("C-c C-M-;" . virtual-comment-show)))

  :config
  ;; TODO: face for higlight
  (setq virtual-comment-default-file (no-littering-expand-var-file-name ".evc")))
