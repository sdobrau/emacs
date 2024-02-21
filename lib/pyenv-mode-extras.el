;; fix
;; TODO: why global ? what?
;;;###autoload
(define-minor-mode pyenv-mode
  "Minor mode for pyenv interaction.

\\{pyenv-mode-map}"
  :global t
  :lighter ""
  :keymap pyenv-mode-map
  (if pyenv-mode
      (if (and (executable-find "pyenv")
               ;; v this
               (eq (buffer-local-value 'major-mode (current-buffer)) 'python-mode))
          (add-to-list 'mode-line-misc-info pyenv-mode-mode-line-format)
        (error "pyenv-mode: pyenv executable not found."))
    (setq mode-line-misc-info
          (delete pyenv-mode-mode-line-format mode-line-misc-info))))

(provide 'pyenv-mode-extras)
